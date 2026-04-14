(** wal_reader.ml
    S-expression parser: string -> Wal_ast.expr list

    LEARNING NOTE:
    This is a hand-rolled recursive descent parser — the most educational
    kind. No parser generator, no magic. Just functions that consume
    characters and return structured data.

    Key OCaml concepts practiced here:
    - Pattern matching on characters
    - Recursive functions (read_expr calls read_list, read_list calls read_expr)
    - Exception types for error handling
    - String / Buffer manipulation
    - Option types: int_of_string_opt returns Some n or None *)

open Wal_ast

(** Parse errors carry a human message and byte position. *)
exception Parse_error of string * int

let parse_error pos msg = raise (Parse_error (msg, pos))

(* ------------------------------------------------------------------ *)
(*  Character helpers                                                    *)
(* ------------------------------------------------------------------ *)

let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

(** Characters that terminate a symbol / number token. *)
let is_delimiter = function
  | '(' | ')' | '"' | ';' -> true
  | c -> is_whitespace c

(** Advance [pos] past whitespace and ; line-comments. *)
let rec skip_whitespace src pos =
  let len = String.length src in
  if pos >= len then pos
  else match src.[pos] with
  | c when is_whitespace c -> skip_whitespace src (pos + 1)
  | ';' ->
    (* skip until newline *)
    let rec skip_line i =
      if i >= len || src.[i] = '\n' then skip_whitespace src (i + 1)
      else skip_line (i + 1)
    in
    skip_line (pos + 1)
  | _ -> pos

(* ------------------------------------------------------------------ *)
(*  Token readers                                                        *)
(* ------------------------------------------------------------------ *)

(** Read a quoted string.  [pos] points just past the opening '"'. *)
let read_string src pos =
  let buf = Buffer.create 32 in
  let len = String.length src in
  let rec loop i =
    if i >= len then parse_error i "Unterminated string literal"
    else match src.[i] with
    | '"'  -> (Str (Buffer.contents buf), i + 1)
    | '\\' ->
      if i + 1 >= len then parse_error i "Unexpected end after backslash"
      else begin
        (match src.[i + 1] with
        | '"'  -> Buffer.add_char buf '"'
        | '\\' -> Buffer.add_char buf '\\'
        | 'n'  -> Buffer.add_char buf '\n'
        | 't'  -> Buffer.add_char buf '\t'
        | c    -> Buffer.add_char buf '\\'; Buffer.add_char buf c);
        loop (i + 2)
      end
    | c ->
      Buffer.add_char buf c;
      loop (i + 1)
  in
  loop pos

(** Read a symbol, number, or boolean atom. *)
let read_atom src pos =
  let len   = String.length src in
  let start = pos in
  let rec scan i =
    if i >= len || is_delimiter src.[i] then i
    else scan (i + 1)
  in
  let end_ = scan pos in
  if end_ = start then parse_error pos "Expected token"
  else
    let token = String.sub src start (end_ - start) in
    let expr = match token with
      | "#t" | "#true"  -> Bool true
      | "#f" | "#false" -> Bool false
      | _ ->
        (match int_of_string_opt token with
        | Some n -> Int n
        | None   ->
          match float_of_string_opt token with
          | Some f -> Float f
          | None   -> Symbol token)
    in
    (expr, end_)

(* ------------------------------------------------------------------ *)
(*  Main recursive parser                                                *)
(* ------------------------------------------------------------------ *)

(** Parse one expression from [src] starting at [pos].
    Returns [(expr, next_pos)]. *)
let rec read_expr src pos =
  let pos = skip_whitespace src pos in
  let len = String.length src in
  if pos >= len then parse_error pos "Unexpected end of input"
  else match src.[pos] with
  | '(' -> read_list src (pos + 1)
  | ')' -> parse_error pos "Unexpected ')'"
  | '"' -> read_string src (pos + 1)
  | '\'' ->
    let (inner, pos') = read_expr src (pos + 1) in
    (List [Symbol "quote"; inner], pos')
  | '`' ->
    let (inner, pos') = read_expr src (pos + 1) in
    (List [Symbol "quasiquote"; inner], pos')
  | ',' ->
    let (inner, pos') = read_expr src (pos + 1) in
    (List [Symbol "unquote"; inner], pos')
  | _ -> read_atom src pos

(** Read list contents after the opening paren, until ')'. *)
and read_list src pos =
  let pos = skip_whitespace src pos in
  let len = String.length src in
  if pos >= len then parse_error pos "Unterminated list"
  else if src.[pos] = ')' then (Nil, pos + 1)
  else
    let rec collect acc pos =
      let pos = skip_whitespace src pos in
      if pos >= String.length src then parse_error pos "Unterminated list"
      else if src.[pos] = ')' then (List (List.rev acc), pos + 1)
      else
        let (expr, pos') = read_expr src pos in
        collect (expr :: acc) pos'
    in
    collect [] pos

(* ------------------------------------------------------------------ *)
(*  Public API                                                           *)
(* ------------------------------------------------------------------ *)

(** Parse a single expression from a string. *)
let parse_expr (src : string) : expr =
  let (expr, _) = read_expr src 0 in
  expr

(** Parse all top-level expressions from a string (a WAL program). *)
let parse_program (src : string) : expr list =
  let len = String.length src in
  let rec loop pos acc =
    let pos = skip_whitespace src pos in
    if pos >= len then List.rev acc
    else
      let (expr, pos') = read_expr src pos in
      loop pos' (expr :: acc)
  in
  loop 0 []

(** Parse a WAL file from disk. *)
let parse_file (path : string) : expr list =
  let ic  = open_in path in
  let n   = in_channel_length ic in
  let src = Bytes.create n in
  really_input ic src 0 n;
  close_in ic;
  parse_program (Bytes.to_string src)

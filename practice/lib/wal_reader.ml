[@@@warning "-27-39-50"]
(* wal_reader.ml
    S-expression parser: string → Wal_ast.expr list

    LEARNING NOTE — Recursive Descent Parsing
    ==========================================
    A "recursive descent parser" is a set of mutually recursive functions,
    one per grammar rule.  No parser generator, no magic.

    WAL's grammar is very simple (it's a Lisp!):

      program  ::= expr*
      expr     ::= atom | list | string | quoted
      list     ::= '(' expr* ')'
      atom     ::= boolean | integer | float | symbol
      boolean  ::= '#t' | '#f'
      quoted   ::= '\'' expr          (* shorthand for (quote expr) *)

    We thread a position integer [pos : int] through every function.
    Each function consumes some characters and returns the NEW position.
    Think of [pos] as a cursor into the string.

    Pattern: every reader returns [(result, new_pos)].

    OCaml concepts practiced here:
    - Pattern matching on characters: [match src.[i] with | '(' -> ...]
    - Recursive functions
    - Exceptions: [exception Parse_error of string * int]
    - Buffer: mutable byte accumulator, like StringBuilder in Java
    - Option types: [int_of_string_opt] returns [Some n] or [None] *)

open Wal_ast

(** Parse errors carry a message and byte position. *)
exception Parse_error of string * int

let parse_error pos msg = raise (Parse_error (msg, pos))

(* ------------------------------------------------------------------ *)
(*  Character helpers                                                    *)
(* ------------------------------------------------------------------ *)

(** Is this character whitespace? *)
let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

(** Characters that STOP a symbol/number token. *)
let is_delimiter = function
  | '(' | ')' | '"' | ';' -> true
  | c -> is_whitespace c

(** [skip_whitespace src pos] — advance [pos] past spaces and ; comments.

    HINT: Recursive.
    - If pos >= String.length src, return pos  (end of input)
    - If src.[pos] is whitespace, recurse at pos+1
    - If src.[pos] is ';', skip to end of line then recurse
    - Otherwise return pos *)
let rec skip_whitespace src pos =
  let len = String.length src in
  (* TODO *)
  ignore len;
  failwith "TODO: skip_whitespace"

(* ------------------------------------------------------------------ *)
(*  Token readers                                                        *)
(* ------------------------------------------------------------------ *)

(** [read_string src pos] — pos points just PAST the opening '"'.
    Read until the closing '"', handling backslash escapes.
    Returns (Str contents, position after closing '"').

    HINT:
    - Create a [Buffer.create 32]
    - Loop with a recursive function [loop i]:
      - if i >= len → parse_error "Unterminated string"
      - if src.[i] = '"' → return (Str (Buffer.contents buf), i+1)
      - if src.[i] = '\\' → handle escape at src.[i+1], then loop (i+2)
      - otherwise → Buffer.add_char buf src.[i]; loop (i+1)

    Escapes to handle: \" \\ \n \t *)
let read_string src pos =
  ignore src; ignore pos;
  failwith "TODO: read_string"

(** [read_atom src pos] — read a symbol, integer, float, or boolean.
    Stops at any delimiter character.
    Returns (expr, position after the token).

    HINT:
    1. Scan forward to find where the token ends (use is_delimiter)
    2. Extract the token string with String.sub
    3. Match it:
       - "#t" or "#true"  → Bool true
       - "#f" or "#false" → Bool false
       - try int_of_string_opt → Int n
       - try float_of_string_opt → Float f
       - otherwise → Symbol token *)
let read_atom src pos =
  ignore src; ignore pos;
  failwith "TODO: read_atom"

(* ------------------------------------------------------------------ *)
(*  Main recursive parser                                                *)
(* ------------------------------------------------------------------ *)

(** [read_expr src pos] — parse ONE expression starting at pos.
    Returns (expr, next_pos).

    HINT: after skip_whitespace, look at src.[pos]:
    - '(' → call read_list at pos+1
    - ')' → parse_error (unexpected close paren)
    - '"' → call read_string at pos+1
    - '\'' → read the next expr, wrap in (List [Symbol "quote"; inner])
    - '`' → quasiquote (same pattern)
    - ',' → unquote (same pattern)
    - anything else → read_atom

    NOTE: read_expr and read_list are mutually recursive → use [and]. *)
let rec read_expr src pos =
  let pos = skip_whitespace src pos in
  let len = String.length src in
  if pos >= len then parse_error pos "Unexpected end of input"
  else
    (* TODO: match src.[pos] with ... *)
    ignore len;
    failwith "TODO: read_expr"

(** [read_list src pos] — pos points just PAST the opening '('.
    Read expressions until ')'.
    Returns (List exprs, position after ')').

    Special case: if we immediately see ')', return (Nil, pos+1).

    HINT: use a recursive helper [collect acc pos] that:
    - skip whitespace
    - if ')' → return (List (List.rev acc), pos+1)
    - else → read one expr, prepend to acc, recurse *)
and read_list src pos =
  ignore src; ignore pos;
  failwith "TODO: read_list"

(* ------------------------------------------------------------------ *)
(*  Public API                                                           *)
(* ------------------------------------------------------------------ *)

(** Parse a single expression. *)
let parse_expr (src : string) : expr =
  let (expr, _) = read_expr src 0 in
  expr

(** Parse a full WAL program (zero or more top-level expressions).

    HINT: loop: skip whitespace, if at end return List.rev acc,
    else read one expr and recurse. *)
let parse_program (src : string) : expr list =
  ignore src;
  failwith "TODO: parse_program"

(** Parse a WAL file from disk.

    HINT:
    1. open_in path
    2. in_channel_length ic  → file size
    3. Bytes.create n, really_input ic src 0 n, close_in ic
    4. call parse_program on (Bytes.to_string src) *)
let parse_file (path : string) : expr list =
  ignore path;
  failwith "TODO: parse_file"

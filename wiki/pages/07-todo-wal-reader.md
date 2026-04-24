# TODO: wal_reader.ml — The Parser

Your job: parse WAL s-expressions from a string into an AST.

File: `lib/wal_reader.ml`

---

## How it works

A **recursive descent parser** — a set of mutually recursive functions, one per grammar rule. No parser generator. No magic.

WAL grammar:
```
program  ::= expr*
expr     ::= atom | list | string | quoted
list     ::= '(' expr* ')'
atom     ::= #t | #f | integer | float | symbol
quoted   ::= '\'' expr        (* shorthand for (quote expr) *)
```

Every function takes `(src : string)` and `(pos : int)` — a cursor into the string — and returns `(result, new_pos)`.

---

## 1. `skip_whitespace`

```ocaml
let rec skip_whitespace src pos = (* TODO *)
```

**Hint:**
```ocaml
let len = String.length src in
if pos >= len then pos
else match src.[pos] with
| ' ' | '\t' | '\n' | '\r' -> skip_whitespace src (pos + 1)
| ';' ->
    (* skip to end of line *)
    let i = ref pos in
    while !i < len && src.[!i] <> '\n' do incr i done;
    skip_whitespace src !i
| _ -> pos
```

---

## 2. `read_atom`

Reads a symbol, number, or boolean token. Stops at delimiters `( ) " ;` or whitespace.

```ocaml
let read_atom src pos = (* TODO *)
```

**Hint:**
```ocaml
(* 1. find end of token *)
let len = String.length src in
let i = ref pos in
while !i < len && not (is_delimiter src.[!i]) do incr i done;
let token = String.sub src pos (!i - pos) in
(* 2. classify *)
match token with
| "#t" | "#true"  -> (Bool true,  !i)
| "#f" | "#false" -> (Bool false, !i)
| _ ->
    match int_of_string_opt token with
    | Some n -> (Int n, !i)
    | None ->
        match float_of_string_opt token with
        | Some f -> (Float f, !i)
        | None   -> (Symbol token, !i)
```

---

## 3. `read_string`

Called when `"` is seen. `pos` points just PAST the opening quote.

```ocaml
let read_string src pos = (* TODO *)
```

**Hint:**
```ocaml
let buf = Buffer.create 32 in
let rec loop i =
  if i >= String.length src then parse_error i "Unterminated string"
  else match src.[i] with
  | '"'  -> (Str (Buffer.contents buf), i + 1)
  | '\\' -> (match src.[i+1] with
             | 'n' -> Buffer.add_char buf '\n'; loop (i+2)
             | 't' -> Buffer.add_char buf '\t'; loop (i+2)
             | c   -> Buffer.add_char buf c;    loop (i+2))
  | c    -> Buffer.add_char buf c; loop (i+1)
in
loop pos
```

---

## 4. `read_expr` and `read_list`

These are mutually recursive — `read_expr` calls `read_list`, and `read_list` calls `read_expr`.

```ocaml
let rec read_expr src pos =
  let pos = skip_whitespace src pos in
  (* TODO: match src.[pos] with *)

and read_list src pos =
  (* TODO *)
```

**Hint for `read_expr`:**
```ocaml
match src.[pos] with
| '(' -> read_list src (pos + 1)
| ')' -> parse_error pos "Unexpected ')'"
| '"' -> read_string src (pos + 1)
| '\'' ->
    let (inner, pos') = read_expr src (pos + 1) in
    (List [Symbol "quote"; inner], pos')
| _   -> read_atom src pos
```

**Hint for `read_list`:**
```ocaml
let rec collect acc pos =
  let pos = skip_whitespace src pos in
  if pos >= String.length src then parse_error pos "Unclosed list"
  else if src.[pos] = ')' then (List (List.rev acc), pos + 1)
  else
    let (expr, pos') = read_expr src pos in
    collect (expr :: acc) pos'
in
collect [] pos
```

---

## 5. `parse_program`

```ocaml
let parse_program (src : string) : expr list = (* TODO *)
```

**Hint:** loop calling `read_expr`, stop when `pos >= String.length src`.

```ocaml
let rec loop acc pos =
  let pos = skip_whitespace src pos in
  if pos >= String.length src then List.rev acc
  else
    let (expr, pos') = read_expr src pos in
    loop (expr :: acc) pos'
in
loop [] 0
```

---

## 6. `parse_file`

```ocaml
let parse_file (path : string) : expr list = (* TODO *)
```

**Hint:**
```ocaml
let ic = open_in path in
let n = in_channel_length ic in
let buf = Bytes.create n in
really_input ic buf 0 n;
close_in ic;
parse_program (Bytes.to_string buf)
```

---

## Tests

```bash
dune test
```

Tests: `test/test_reader.ml`

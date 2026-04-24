# Variants & Pattern Matching

The most important concept in OCaml. Used everywhere in wal-fast.

---

## Variants (Sum Types)

A variant type says "a value can be **one of these shapes**".

```ocaml
type color =
  | Red
  | Green
  | Blue
```

In wal-fast, `wal_ast.ml` defines the entire WAL language as a variant:

```ocaml
type expr =
  | Int of int
  | Float of float
  | Symbol of string
  | List of expr list
  | Bool of bool
  | Nil
```

Every WAL expression — numbers, symbols, lists — is one of these cases. Nothing else can exist.

---

## Pattern Matching on Variants

```ocaml
let describe_expr e =
  match e with
  | Int n    -> Printf.sprintf "integer: %d" n
  | Float f  -> Printf.sprintf "float: %f" f
  | Symbol s -> Printf.sprintf "symbol: %s" s
  | List _   -> "a list"
  | Bool b   -> if b then "true" else "false"
  | Nil      -> "nil"
```

The compiler **forces** you to handle every case. This is why wal-fast has no `null pointer` bugs.

---

## Option Type

The built-in way to represent "maybe a value". No null in OCaml.

```ocaml
type 'a option =
  | Some of 'a
  | None

let find_signal name signals =
  match List.find_opt (fun s -> s.name = name) signals with
  | Some s -> s.value
  | None   -> failwith ("Signal not found: " ^ name)
```

---

## Nested Patterns

You can match deep structure:

```ocaml
match expr with
| List (Symbol "if" :: cond :: then_e :: else_e :: []) ->
    eval_if cond then_e else_e
| List (Symbol "define" :: Symbol name :: body :: []) ->
    eval_define name body
| List (func :: args) ->
    eval_call func args
| _ ->
    eval_atom expr
```

This exact pattern appears in `wal_eval.ml` — the core of wal-fast.

---

## Record Types

When a variant case has many fields, use a record:

```ocaml
type signal = {
  name  : string;
  width : int;
  value : int;
}

let clk = { name = "clk"; width = 1; value = 0 }
let v = clk.value      (* => 0 *)
let clk' = { clk with value = 1 }   (* update one field *)
```

Used in `wal_vcd.ml` and `wal_trace.ml` to represent VCD signals.

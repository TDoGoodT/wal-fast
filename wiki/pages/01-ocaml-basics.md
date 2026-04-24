# OCaml Basics

The essentials you need before anything else.

---

## Types

OCaml is **statically typed** — every value has a type, and the compiler figures it out (type inference). You rarely need to write types explicitly.

```ocaml
let x = 42           (* int *)
let y = 3.14         (* float *)
let s = "hello"      (* string *)
let b = true         (* bool *)
```

> Notice: no `var`, no `:=`, no type annotations needed. Just `let`.

---

## Functions

Everything is a function. Functions are values.

```ocaml
let add x y = x + y          (* takes two ints, returns int *)
let square n = n * n

add 3 4                       (* => 7 *)
square 5                      (* => 25 *)
```

**No parentheses** for calling — just space-separated arguments.

---

## Pattern Matching

OCaml's superpower. Like a `switch` but exhaustive and structural.

```ocaml
let describe n =
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | n when n < 0 -> "negative"
  | _ -> "big"
```

The compiler **warns you** if you miss a case. Use this everywhere.

---

## Let Bindings

```ocaml
let x = 10
let y = x + 5    (* y = 15 *)

(* Local binding *)
let result =
  let a = 3 in
  let b = 4 in
  a + b           (* result = 7 *)
```

`let ... in` scopes a variable locally. Very common in wal-fast.

---

## If / Else

```ocaml
if x > 0 then "positive" else "non-positive"
```

`if` is an **expression** — it returns a value. Both branches must return the same type.

---

## Lists

```ocaml
let nums = [1; 2; 3; 4]      (* note: semicolons, not commas *)
let head = List.hd nums       (* => 1 *)
let tail = List.tl nums       (* => [2;3;4] *)
let longer = 0 :: nums        (* => [0;1;2;3;4] *)
```

---

## Where This Appears in wal-fast

- `wal_ast.ml` — defines the AST using variants (next topic)
- `wal_eval.ml` — uses pattern matching extensively to evaluate expressions
- `wal_reader.ml` — parses s-expressions using recursive functions

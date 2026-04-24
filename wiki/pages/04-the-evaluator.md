# The WAL Evaluator (wal_eval.ml)

The heart of wal-fast. ~1000 lines that run the WAL Lisp language.

---

## What an Evaluator Does

Takes an AST expression + an environment → returns a value.

```
text  →  reader  →  expr  →  eval  →  value
"(+ 1 2)"  →  List[Symbol "+", Int 1, Int 2]  →  Int 3
```

---

## The eval Function

The core recursive function. Pattern matches on every possible expression shape:

```ocaml
let rec eval env expr =
  match expr with
  | Int _ | Float _ | Bool _ | Nil -> expr   (* atoms eval to themselves *)
  | Symbol name -> Wal_env.lookup env name    (* look up variable *)
  | List [] -> Nil
  | List (Symbol "if" :: rest) -> eval_if env rest
  | List (Symbol "define" :: rest) -> eval_define env rest
  | List (Symbol "lambda" :: rest) -> eval_lambda env rest
  | List (Symbol "quote" :: arg :: []) -> arg
  | List (func_expr :: args) -> eval_call env func_expr args
```

Every WAL construct — if, define, lambda, function call — is a pattern here.

---

## Special Forms vs Functions

**Special forms** don't evaluate their arguments before running. `if`, `define`, `lambda`, `quote`.

**Functions** evaluate all arguments first, then call the function.

```ocaml
(* Special form — evaluates condition first, then only one branch *)
(if (= x 0) (/ 1 0) x)   (* safe even when x=0 *)

(* Function — ALL args evaluated before call *)
(+ 1 2)   (* both 1 and 2 evaluated, then + called *)
```

---

## Closures

When you define a lambda, it **captures** the current environment:

```ocaml
(define make-adder
  (lambda (n)
    (lambda (x) (+ x n))))   ; inner lambda captures n

(define add5 (make-adder 5))
(add5 3)   ; => 8
```

In wal-fast, a closure is stored as `Closure (params, body, captured_env)` in `Wal_ast`.

---

## The Environment (wal_env.ml)

A chain of scopes. Each scope is a `(string, expr) Hashtbl.t`.

```
global scope: { "+": builtin_add, "define": ..., "if": ... }
    ↓
function scope: { "n": Int 5 }
    ↓
inner scope: { "x": Int 3 }
```

Looking up `n` from the inner scope walks up the chain until found.

---

## Built-in Functions

Defined in `wal_eval.ml`, bound in the global environment at startup:

```ocaml
(+)  (-)  (*)  (/)         (* arithmetic *)
(=)  (<)  (>)  (<=)  (>=)  (* comparison *)
(and)  (or)  (not)         (* logic *)
(list)  (car)  (cdr)       (* list ops *)
(println)  (print)         (* output *)
(for-each-timestep)        (* VCD iteration — WAL-specific *)
(rising-edge)  (falling-edge)  (* signal edge detection *)
```

---

## Reading This in the Code

Open `lib/wal_eval.ml`. The giant `eval` function starts around line 50. 
Find `eval_call` — it handles all function application.
Find `eval_if` — see how it avoids evaluating the dead branch.

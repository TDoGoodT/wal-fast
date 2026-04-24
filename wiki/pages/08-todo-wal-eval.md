# TODO: wal_eval.ml — The Evaluator

Your job: implement the core `eval` function — the engine of the interpreter.

File: `lib/wal_eval.ml`

---

## The Big Picture

```
eval : env -> expr -> value
```

Takes an expression (parsed AST) + an environment (variable bindings) → returns a value.

Atoms return themselves. Symbols do a lookup. Lists are either special forms or function calls.

---

## 1. Atoms — `expr_to_value`

The simplest part. Literals evaluate to themselves:

```ocaml
| Int n    -> VInt n
| Float f  -> VFloat f
| Bool b   -> VBool b
| Str s    -> VStr s
| Nil      -> VNil
```

---

## 2. `eval` — the main function

```ocaml
let rec eval env expr =
  match expr with
  | Int n   -> VInt n      (* atoms — trivial *)
  | Float f -> VFloat f
  | Bool b  -> VBool b
  | Str s   -> VStr s
  | Nil     -> VNil

  | Symbol name ->          (* variable lookup *)
      Wal_env.lookup_exn name env

  | List [] -> VNil

  | List (Symbol "quote" :: arg :: []) ->   (* don't evaluate arg *)
      expr_to_value arg

  | List (Symbol "if" :: rest) ->
      eval_if env rest

  | List (Symbol "define" :: rest) ->
      eval_define env rest

  | List (Symbol "lambda" :: rest) ->
      eval_lambda env rest

  | List (Symbol "let" :: rest) ->
      eval_let env rest

  | List (Symbol "begin" :: exprs) ->
      eval_begin env exprs

  | List (func_expr :: args) ->          (* function call *)
      eval_call env func_expr args
```

---

## 3. `eval_if`

```ocaml
(* (if cond then_e else_e) or (if cond then_e) *)
```

**Hint:**
```ocaml
match rest with
| [cond; then_e; else_e] ->
    if eval env cond <> VBool false
    then eval env then_e
    else eval env else_e
| [cond; then_e] ->
    if eval env cond <> VBool false
    then eval env then_e
    else VNil
```

In WAL (like Scheme), everything except `#f` is truthy.

---

## 4. `eval_define`

```ocaml
(* (define name expr)
   (define (name params...) body) — shorthand for lambda *)
```

**Hint:**
```ocaml
match rest with
| [Symbol name; body] ->
    let v = eval env body in
    Wal_env.define name v env;
    VNil
| List (Symbol name :: params) :: body ->
    (* shorthand: (define (f x y) body) = (define f (lambda (x y) body)) *)
    let param_names = List.map (function Symbol s -> s | _ -> failwith "bad param") params in
    let closure = VClosure (param_names, List.hd body, env) in
    Wal_env.define name closure env;
    VNil
```

---

## 5. `eval_lambda`

```ocaml
(* (lambda (params...) body) *)
```

**Hint:**
```ocaml
match rest with
| List params :: body :: [] ->
    let param_names = List.map (function Symbol s -> s | _ -> failwith "bad param") params in
    VClosure (param_names, body, env)
```

A closure **captures** the current environment — that's how closures work.

---

## 6. `eval_call` — function application

```ocaml
(* (f arg1 arg2 ...) *)
```

**Hint:**
```ocaml
let func = eval env func_expr in
let arg_vals = List.map (eval env) args in
match func with
| VClosure (params, body, closure_env) ->
    let bindings = List.combine params arg_vals in
    let new_env = Wal_env.extend bindings closure_env in
    eval new_env body
| VBuiltin (_, f) ->
    f arg_vals
| _ -> failwith ("Not a function: " ^ show_value func)
```

---

## 7. `eval_begin`

```ocaml
(* (begin expr1 expr2 ... exprN) — evaluates all, returns last *)
```

**Hint:**
```ocaml
match exprs with
| []     -> VNil
| [e]    -> eval env e
| e :: rest -> ignore (eval env e); eval_begin env rest
```

---

## Checklist

- [ ] `expr_to_value` — atoms
- [ ] `eval` — Symbol lookup
- [ ] `eval` — quote
- [ ] `eval_if` — both with and without else
- [ ] `eval_define` — name form + function shorthand
- [ ] `eval_lambda` — captures env
- [ ] `eval_call` — closures + builtins
- [ ] `eval_begin`
- [ ] `eval_let`, `eval_let_star`, `eval_letrec`
- [ ] `eval_and`, `eval_or`
- [ ] `eval_cond`

---

## Tests

```bash
dune test
```

Tests: `test/test_eval.ml`

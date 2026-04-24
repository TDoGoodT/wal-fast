# TODO: wal_env.ml — The Environment

Your job: implement the scope chain for the WAL interpreter.

File: `lib/wal_env.ml`

---

## What is an Environment?

A chain of scopes. Each scope is a `(string * value) list` — a list of name/value pairs.

```
env = [ inner_scope ; outer_scope ; global_scope ]
      [("z",3)]  ::  [("x",1);("y",2)]  ::  [[]]
```

Looking up `"x"` → not in inner → found in outer → `VInt 1`.

The type is already defined in `wal_ast.ml`:
```ocaml
type env = (string * value) list list
```

---

## 1. `lookup` — search the scope chain

```ocaml
let rec lookup (name : string) (env : env) : value option =
  (* TODO *)
```

**Hint:**
```ocaml
match env with
| []            -> None
| scope :: rest ->
    match List.assoc_opt name scope with
    | Some v -> Some v
    | None   -> lookup name rest
```

`List.assoc_opt key list` — searches a `(key, value) list`, returns `Some v` or `None`.

---

## 2. `lookup_exn` — same but raises on missing

```ocaml
let lookup_exn (name : string) (env : env) : value =
  (* TODO *)
```

**Hint:** call `lookup`, then:
```ocaml
match lookup name env with
| Some v -> v
| None   -> failwith ("Unbound variable: " ^ name)
```

---

## 3. `extend` — push a new scope

```ocaml
let extend (bindings : (string * value) list) (env : env) : env =
  (* TODO *)
```

**Hint:** It's literally one line:
```ocaml
bindings :: env
```

Called when entering a function or `let` — pushes all the new bindings as a fresh scope.

---

## 4. `define` — bind in the innermost scope

```ocaml
let define (name : string) (value : value) (env : env) : env =
  (* TODO *)
```

**Hint:**
```ocaml
match env with
| []            -> [[(name, value)]]
| scope :: rest -> ((name, value) :: scope) :: rest
```

Different from `extend` — adds to the *existing* head scope instead of pushing a new one. Used for top-level `(define x 42)`.

---

## Tests

```bash
dune test
```

Tests live in `test/test_env.ml`. All 4 functions are tested.

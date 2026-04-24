# Modules & the Dune Build System

How OCaml organizes code — and how wal-fast is structured.

---

## Modules

Every `.ml` file is automatically a **module**. The filename = the module name (capitalized).

```
wal_ast.ml    →  module Wal_ast
wal_eval.ml   →  module Wal_eval
wal_vcd.ml    →  module Wal_vcd
```

To use something from another module:

```ocaml
(* In wal_eval.ml *)
let expr = Wal_ast.Int 42
let env  = Wal_env.empty ()
```

---

## open

`open` brings all names from a module into scope — like a Python `from x import *`.

```ocaml
open Wal_ast

let expr = Int 42        (* no Wal_ast. prefix needed *)
```

Use sparingly — can cause name clashes. Prefer explicit `Module.thing`.

---

## Module Interface (.mli files)

A `.mli` file is the **public API** of a module. If it exists, only what's in it is visible to other modules.

wal-fast doesn't use `.mli` files yet — everything is public. Good for learning, can add later.

---

## Dune Build System

Dune is the standard OCaml build tool. Config lives in `dune` files.

### wal-fast's lib/dune:
```scheme
(library
 (name wal_fast)
 (modules wal_ast wal_env wal_eval wal_reader wal_trace wal_vcd))
```

### wal-fast's bin/dune:
```scheme
(executable
 (name main)
 (libraries wal_fast cmdliner))
```

---

## Common Dune Commands

```bash
dune build          # compile everything
dune test           # run all tests
dune exec bin/main.exe -- --help   # run the CLI
dune utop lib       # REPL with all modules loaded
dune build @doc     # generate documentation
dune clean          # delete _build/
```

---

## The _build Directory

Dune puts all compiled output in `_build/`. Never edit files there. Never commit it.

```
_build/
  default/
    lib/
      wal_fast.cma        ← bytecode library
      wal_fast.cmxa       ← native library
    bin/
      main.exe            ← the actual binary
```

---

## wal-fast Module Map

| Module | Job |
|---|---|
| `Wal_ast` | Type definitions — what a WAL expression looks like |
| `Wal_reader` | Parser — turns text into `Wal_ast.expr` |
| `Wal_env` | Environment — variable scopes and bindings |
| `Wal_eval` | Evaluator — runs expressions, applies functions |
| `Wal_vcd` | VCD parser — reads simulation trace files |
| `Wal_trace` | Trace cursor — iterates over timesteps |

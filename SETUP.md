# WAL-FAST — Phase 0 Setup Guide

Everything you need to go from zero to a working OCaml dev environment
and a running wal-fast build. Follow the steps in order the first time.

---

## Prerequisites

| Tool | Why | Version |
|------|-----|---------|
| **opam** | OCaml's package manager — manages compilers and libraries | ≥ 2.1 |
| **OCaml** | The compiler itself, installed via opam | ≥ 5.0 (we use 5.2.0) |
| **dune** | Build system used by wal-fast | ≥ 3.0 |
| **git** | To clone the repo | any |
| **curl** | Used by the opam installer script | any |

> **Linux / macOS only.** WSL2 works fine on Windows.

---

## Step 1 — Install opam

Check if you already have it:

```bash
opam --version
```

If that prints a version number (e.g. `2.1.5`), skip to Step 2.

If not, install it with the official script:

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
```

The script will ask where to put the binary (`/usr/local/bin` is fine) and
may ask for your sudo password to copy it there.

---

## Step 2 — Initialize opam

This sets up the `~/.opam` directory and downloads the package index.
Only needs to be done once per machine.

```bash
opam init
```

When it asks *"Do you want opam to modify ~/.bashrc?"* — say **yes**.
It adds the line that makes `opam env` work automatically in new shells.

Then activate the environment in your current shell:

```bash
eval $(opam env)
```

> **Tip:** Add `eval $(opam env)` to your `~/.bashrc` or `~/.zshrc` so
> every new terminal is ready automatically.

---

## Step 3 — Create an OCaml 5.2.0 switch

A "switch" is an isolated OCaml installation with its own compiler and
packages. Think of it like a Python virtualenv.

```bash
# Check if you already have a 5.2.0 switch
opam switch list

# If 5.2.0 is not listed, create it (takes a few minutes — compiles OCaml)
opam switch create 5.2.0

# Activate the new switch in your current shell
eval $(opam env)
```

Verify the compiler is correct:

```bash
ocaml --version
# Should print: The OCaml toplevel, version 5.2.0
```

---

## Step 4 — Install project dependencies

```bash
opam install dune ocaml-lsp-server ocamlformat alcotest cmdliner
```

What each package does:

| Package | Purpose |
|---------|---------|
| `dune` | Build system |
| `ocaml-lsp-server` | Language server — powers Neovim/VSCode IDE features |
| `ocamlformat` | Auto-formatter (like `gofmt` or `black`) |
| `alcotest` | Test framework used by wal-fast's test suite |
| `cmdliner` | CLI argument parsing (used by `bin/main.ml`) |

---

## Step 5 — Clone and build wal-fast

```bash
git clone https://github.com/TDoGoodT/wal-fast
cd wal-fast

# Build the entire project
dune build
```

A successful build prints nothing. If you see errors, jump to
[Troubleshooting](#troubleshooting) below.

---

## Step 6 — Run the tests

```bash
dune test
```

All tests should pass. You'll see output like:

```
Testing `wal-fast'
  [OK] ...
```

---

## Step 7 — First run

```bash
# The evaluator reads WAL expressions from stdin.
# This evaluates the expression (+ 1 2) and should print 3.
echo '(+ 1 2)' | dune exec bin/main.exe -- repl test.vcd
```

> Until `wal_eval.ml` and the VCD reader are implemented, this will print
> a "not yet implemented" error — that's expected in Phase 0.
> The important thing is that the binary builds and launches.

---

## Neovim Setup (ocamllsp)

`ocaml-lsp-server` gives you jump-to-definition, type hints on hover,
inline errors, and auto-complete in Neovim.

### Find the LSP binary path

```bash
opam exec -- which ocamllsp
# Example output: /home/snir/.opam/5.2.0/bin/ocamllsp
```

Use that path in your Neovim LSP config.

### nvim-lspconfig (Lua)

```lua
-- In your neovim config (e.g. ~/.config/nvim/lua/lsp.lua)
require('lspconfig').ocamllsp.setup({
  cmd = { "/home/snir/.opam/5.2.0/bin/ocamllsp" },
  -- or just "ocamllsp" if ~/.opam/5.2.0/bin is in your PATH
  filetypes = { "ocaml", "ocaml.menhir", "ocaml.interface", "ocaml.ocamllex" },
  root_dir = require('lspconfig.util').root_pattern("dune-project", "*.opam"),
})
```

### Make sure opam's bin is in PATH

Add this to your `~/.bashrc` or `~/.zshrc`:

```bash
eval $(opam env)
```

After that, `ocamllsp` will be found automatically and you won't need the
full path.

### ocamlformat config

wal-fast ships with a `.ocamlformat` config file. Your editor should
pick it up automatically. To format a file manually:

```bash
ocamlformat --inplace lib/wal_ast.ml
```

---

## Project Structure

```
wal-fast/
├── dune-project          # top-level build config & package metadata
├── bin/
│   ├── dune              # declares the 'wal-fast' executable
│   └── main.ml           # CLI entry point (to be written)
├── lib/
│   ├── dune              # declares the 'wal_fast' library
│   ├── wal_ast.ml        # AST types: expr, value, closure, env
│   ├── wal_env.ml        # Lexical environment: lookup, extend, define
│   ├── wal_reader.ml     # Parser: string -> expr  (Phase 1)
│   ├── wal_eval.ml       # Evaluator: expr -> value  (Phase 2)
│   ├── wal_vcd.ml        # VCD file parser  (Phase 3)
│   └── wal_trace.ml      # Signal trace access  (Phase 3)
└── test/
    ├── dune              # declares the test binary
    └── test_wal.ml       # alcotest test suite
```

---

## Useful dune Commands

```bash
dune build                    # compile everything
dune test                     # build + run tests
dune exec bin/main.exe        # run the binary (no args)
dune exec bin/main.exe -- repl test.vcd   # pass args after --
dune utop lib                 # open a REPL with wal_fast loaded
dune clean                    # delete _build/
dune build @doc               # generate HTML docs in _build/default/_doc/
```

---

## Troubleshooting

### `opam: command not found` after install

The installer put the binary somewhere not in your PATH. Try:

```bash
export PATH="$HOME/.local/bin:$PATH"
opam --version
```

Then add that export to your shell config file.

---

### `Error: ocaml version 5.0+ required`

You're on an old switch. Switch to 5.2.0:

```bash
opam switch 5.2.0
eval $(opam env)
```

---

### `dune build` fails with `Uninterpreted extension`

You need dune ≥ 3.0. Check with:

```bash
dune --version
```

If it's old, upgrade:

```bash
opam upgrade dune
```

---

### `Error: Library "alcotest" not found`

The dependency isn't installed in the current switch:

```bash
opam install alcotest
```

---

### `ocamllsp` not found in Neovim

1. Check the LSP is installed: `opam list ocaml-lsp-server`
2. Make sure `eval $(opam env)` runs before Neovim starts
3. Use the full absolute path from `opam exec -- which ocamllsp`

---

### Build succeeds but `dune exec` says `No such file`

Make sure you're passing the `.exe` suffix (dune requires it even on Linux):

```bash
dune exec bin/main.exe          # correct
dune exec bin/main              # wrong
```

---

### Warnings treated as errors

wal-fast enables strict warnings. If you add code with unused variables,
you'll get a compile error. Fix it by prefixing the variable with `_`:

```ocaml
let _unused = something_not_used_yet in
```

---

*Last updated: Phase 0 — environment setup only. No VCD files are needed yet.*

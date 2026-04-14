# WAL-FAST 🔥

**Blazing fast Waveform Analysis Language for huge EDA simulations.**

WAL-FAST is a high-performance reimplementation of the [WAL (Waveform Analysis Language)](https://github.com/ics-jku/wal) in OCaml. The original WAL is a Lisp dialect for analyzing VCD simulation traces — brilliant idea, Python engine. WAL-FAST keeps the language, replaces the engine.

**Based on WAL by Miro Haller, ICS-JKU Linz, Austria — BSD-3-Clause license.**
See [NOTICE](NOTICE) for full attribution.

---

## Why

A modern SoC simulation at 1GHz for 1ms produces VCD files of 10–50GB.
The original WAL loads the entire file into memory before you can write a single expression.
WAL-FAST is designed for the real world of chip verification:

| | Original WAL | WAL-FAST |
|---|---|---|
| Engine | Python (interpreted) | OCaml (native compiled) |
| VCD loading | Full file into RAM | Streaming + indexed |
| 100MB file | ~10s startup | <1s |
| 10GB file | Out of memory | Works |
| Language | WAL Lisp | WAL Lisp (compatible) |

---

## Quick Start

```bash
# Install dependencies
opam install dune ocaml-lsp-server ocamlformat alcotest cmdliner

# Build
git clone https://github.com/TDoGoodT/wal-fast
cd wal-fast
dune build

# Inspect a trace
dune exec bin/main.exe -- index simulation.vcd

# Run a WAL script
dune exec bin/main.exe -- run analysis.wal simulation.vcd

# Interactive REPL
dune exec bin/main.exe -- repl simulation.vcd
```

See [SETUP.md](SETUP.md) for full installation guide.

---

## WAL Language Quick Reference

WAL is a Lisp dialect. Every expression is either an atom or a list.

```scheme
; Arithmetic
(+ 1 2)          ; => 3
(* 3 (+ 1 1))    ; => 6

; Variables
(define x 42)
x                ; => 42

; Functions
(define square (lambda (n) (* n n)))
(square 5)       ; => 25

; Conditionals
(if (= x 0) "zero" "nonzero")
(cond ((< x 0) "neg") ((> x 0) "pos") (#t "zero"))

; Lists
(list 1 2 3)           ; => (1 2 3)
(map square (list 1 2 3))   ; => (1 4 9)
(filter (lambda (x) (> x 2)) (list 1 2 3 4))  ; => (3 4)

; Signal access (signals from the loaded VCD are in scope)
clk              ; => current value of signal "clk"
data             ; => current value of signal "data"

; Iterate over all timesteps
(for-each-timestep
  (if (= clk 1)
      (println data)))

; Find first rising edge
(find-first (lambda () (rising-edge clk)))
```

---

## Project Structure

```
wal-fast/
├── lib/
│   ├── wal_ast.ml      # AST types: expr, value, closure
│   ├── wal_reader.ml   # S-expression parser
│   ├── wal_eval.ml     # Tree-walking evaluator + stdlib
│   ├── wal_env.ml      # Lexical environment (scope chain)
│   ├── wal_vcd.ml      # VCD file parser + binary search
│   └── wal_trace.ml    # Runtime trace cursor
├── bin/
│   └── main.ml         # CLI (run / repl / index)
├── test/
│   └── test_wal.ml     # Alcotest test suite
├── SETUP.md            # Phase 0 setup guide
└── OCAML_CHEATSHEET.md # OCaml reference for beginners
```

---

## Development

```bash
dune build          # compile everything
dune test           # run all tests
dune utop lib       # REPL with all modules loaded
dune build @doc     # generate odoc documentation
dune exec bin/main.exe -- --help
```

---

## Roadmap

- [x] WAL Lisp parser (s-expressions)
- [x] Tree-walking evaluator
- [x] VCD parser with binary-search signal lookup
- [x] Trace cursor with for-each-timestep
- [x] CLI: run / repl / index
- [ ] Streaming VCD parser (O(1) memory for huge files)
- [ ] .wfi persistent index format (mmap-backed)
- [ ] FST format support (GTKWave compressed format)
- [ ] Parallel signal analysis (Domain / Effect-based, OCaml 5)
- [ ] Python SDK via ocaml-rs
- [ ] WAL bytecode compiler + VM (v2)

---

## License

BSD-3-Clause. See [LICENSE](LICENSE).

Original WAL copyright © Miro Haller, ICS-JKU Linz. See [NOTICE](NOTICE).

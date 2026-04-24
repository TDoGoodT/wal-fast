# VCD Files & the Trace Engine

What WAL-FAST actually processes — chip simulation waveforms.

---

## What is a VCD File?

**Value Change Dump** — the standard output format of digital simulation tools (Verilator, ModelSim, iverilog).

A 1GHz SoC simulated for 1ms = **1 billion timesteps** = 10–50GB VCD file.

```vcd
$timescale 1ns $end
$var wire 1 ! clk $end
$var wire 8 " data $end
$enddefinitions $end

#0
1!
b00000000 "
#5
0!
#10
1!
b01001010 "
```

- `#0`, `#5`, `#10` = timestamps
- `1!` = signal `!` (clk) went to 1
- `b01001010 "` = signal `"` (data) = binary 01001010

---

## The VCD Parser (wal_vcd.ml)

Reads the VCD header to build a signal map, then indexes change events.

```ocaml
type signal = {
  id    : string;    (* VCD identifier like "!" *)
  name  : string;    (* human name like "clk" *)
  width : int;       (* bit width *)
}

type vcd = {
  signals  : (string, signal) Hashtbl.t;
  changes  : (int64 * string * string) array;  (* time, id, value *)
  timescale: string;
}
```

---

## Binary Search

WAL-FAST uses binary search to find signal values at any timestamp — no need to scan the whole file.

```ocaml
(* Find the last change to signal `id` at or before timestamp `t` *)
let value_at vcd id t =
  (* binary search changes array for largest timestamp <= t *)
  ...
```

Original WAL: scan from start every time = O(n).
WAL-FAST: binary search = O(log n). For a 10GB file this is the difference between seconds and hours.

---

## The Trace Cursor (wal_trace.ml)

Wraps a VCD and provides a cursor that steps through time:

```ocaml
type cursor = {
  vcd      : Wal_vcd.vcd;
  mutable current_time : int64;
  mutable timesteps    : int64 array;
  mutable pos          : int;
}
```

`for-each-timestep` in WAL moves this cursor forward one step at a time, updating the environment with new signal values.

---

## WAL Signal Access

When a VCD is loaded, all signal names become variables in the WAL environment:

```scheme
; After: (load "sim.vcd")
clk     ; => current value of clk at current timestep
data    ; => current value of data

(for-each-timestep
  (if (= clk 1)
      (println data)))
```

Under the hood, `wal_eval.ml` looks up signal names via the trace cursor.

---

## Why Streaming Matters (Roadmap Item)

Current implementation still loads the full change list into memory. Next step: true streaming where we only keep a sliding window in RAM. That's what makes 50GB files work on a 16GB laptop.

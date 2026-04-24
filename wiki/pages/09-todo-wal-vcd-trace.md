# TODO: wal_vcd.ml & wal_trace.ml — VCD Parser and Trace Engine

Your job: parse chip simulation traces and walk them timestep by timestep.

Files: `lib/wal_vcd.ml`, `lib/wal_trace.ml`

---

## VCD Format Recap

```vcd
$timescale 1ns $end
$var wire 1 ! clk $end      ← signal: id="!", name="clk", width=1
$var wire 8 " data $end
$enddefinitions $end

#0          ← timestamp
1!          ← signal "!" changed to 1
b00000000 " ← signal """ changed to 00000000
#5
0!
#10
1!
b01001010 "
```

---

## wal_vcd.ml — Data Structures

```ocaml
type signal = {
  id     : string;   (* VCD identifier, e.g. "!" *)
  name   : string;   (* human name, e.g. "clk" *)
  width  : int;
}

type change = {
  time   : int64;
  id     : string;
  value  : string;   (* "0", "1", "x", "z", or binary like "01001010" *)
}

type vcd = {
  by_id    : (string, signal) Hashtbl.t;
  by_name  : (string, signal) Hashtbl.t;
  changes  : change array;   (* sorted by time *)
  timescale: string;
}
```

---

## TODO: `parse_vcd`

```ocaml
let parse_vcd (src : string) : vcd = (* TODO *)
```

**Approach:**
1. Tokenize the string (split on whitespace)
2. Walk tokens. When you see `$var` → read `wire WIDTH ID NAME ... $end` into a signal
3. When you see `$enddefinitions $end` → switch to reading changes
4. Changes: `#N` sets current time. `1!` or `0!` = 1-bit change. `bVALUE ID` = multi-bit

**Hint sketch:**
```ocaml
let tokens = String.split_on_char ' ' src |> List.concat_map (String.split_on_char '\n') |> List.filter (fun s -> s <> "") in
let by_id   = Hashtbl.create 16 in
let by_name = Hashtbl.create 16 in
let changes = ref [] in
(* walk tokens ... *)
```

---

## TODO: `value_at`

Binary search the changes array for the last change to signal `id` at or before `time`.

```ocaml
let value_at (vcd : vcd) (id : string) (time : int64) : string option =
  (* TODO: binary search vcd.changes *)
```

**Hint:**
```ocaml
(* Filter only changes for this id, then binary search *)
(* Or: binary search by time, then scan backwards for matching id *)
let arr = vcd.changes in
let lo = ref 0 and hi = ref (Array.length arr - 1) in
let result = ref None in
while !lo <= !hi do
  let mid = (!lo + !hi) / 2 in
  if arr.(mid).time <= time then begin
    if arr.(mid).id = id then result := Some arr.(mid).value;
    lo := mid + 1
  end else
    hi := mid - 1
done;
!result
```

---

## TODO: `all_times`

Collect all unique timestamps from the changes array, sorted:

```ocaml
let all_times (vcd : vcd) : int64 array =
  (* TODO *)
```

**Hint:**
```ocaml
let tbl = Hashtbl.create 64 in
Array.iter (fun c -> Hashtbl.replace tbl c.time ()) vcd.changes;
let times = Hashtbl.fold (fun t () acc -> t :: acc) tbl [] in
let sorted = List.sort Int64.compare times in
Array.of_list sorted
```

---

## wal_trace.ml — The Cursor

```ocaml
type cursor = {
  vcd   : Wal_vcd.vcd;
  mutable idx   : int;
  times : int64 array;
}
```

### TODO: `load` — create a cursor from a VCD file

```ocaml
let load (path : string) : cursor =
  (* TODO: parse_vcd, all_times, return cursor at idx=0 *)
```

### TODO: `current_time`
```ocaml
let current_time tr = tr.times.(tr.idx)   (* or 0L if empty *)
```

### TODO: `step` — advance one timestep
```ocaml
let step tr =
  if tr.idx < Array.length tr.times - 1 then tr.idx <- tr.idx + 1
```

### TODO: `for_each_timestep` — the main WAL loop

```ocaml
let for_each_timestep (tr : cursor) (f : cursor -> unit) : unit =
  (* TODO: reset idx to 0, loop calling f tr then step, until at_end *)
```

**Hint:**
```ocaml
tr.idx <- 0;
let len = Array.length tr.times in
while tr.idx < len do
  f tr;
  if tr.idx < len - 1 then tr.idx <- tr.idx + 1
  else tr.idx <- len
done
```

### TODO: `signal_value` — get signal value at current timestep

```ocaml
let signal_value (tr : cursor) (name : string) : value =
  (* TODO: find signal by name, call value_at, convert to value *)
```

---

## Checklist

- [ ] `parse_vcd` — header + changes
- [ ] `value_at` — binary search
- [ ] `all_times` — unique sorted timestamps
- [ ] `find_signal` — Hashtbl.find_opt by name
- [ ] `signal_names` — all keys from by_name
- [ ] `load` — cursor from file
- [ ] `current_time`, `num_timesteps`, `step`, `reset`, `at_end`
- [ ] `for_each_timestep`
- [ ] `signal_value` + `bind_signals_to_env`
- [ ] `map_timesteps`, `find_first`

---

## Tests

```bash
dune test
```

Tests: `test/test_vcd.ml`, `test/test_trace.ml`

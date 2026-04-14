(** wal_trace.ml
    The runtime trace object — the central "cursor" that WAL programs
    use to navigate a loaded simulation.

    LEARNING NOTE — Mutable State in OCaml
    =======================================
    OCaml is mostly immutable, but sometimes you genuinely need mutation —
    like a "current time pointer" that steps forward as you iterate.
    We use [mutable] record fields for this. They behave like variables
    you can reassign with the [<-] operator:

        tr.idx <- tr.idx + 1    (* set mutable field idx to idx+1 *)

    Contrast with functional update (creates a NEW record, leaves original):

        { tr with idx = tr.idx + 1 }   (* NOT what we want here! *)

    Because [t] has a mutable [idx] field, all functions that "move the
    cursor" modify the record IN PLACE — the caller's [tr] value reflects
    the change immediately. *)

[@@@warning "-27-50"]

open Wal_ast

(** The trace: a loaded VCD plus a current-time cursor. *)
type t = {
  vcd          : Wal_vcd.vcd;
  mutable idx  : int;       (** index into [times] array — current position *)
  times        : int array; (** all unique timestamps, sorted ascending      *)
}


(** [load path] — parse a VCD file and build a trace starting at time 0.

    WHAT IT SHOULD DO:
    1. Parse the VCD file at [path] to get a [vcd] record.
    2. Extract all unique sorted timestamps into an int array [times].
    3. Return a [t] record with idx = 0 (start at beginning).

    ALGORITHM:
    1. Call Wal_vcd.parse_vcd path  →  vcd
    2. Call Wal_vcd.all_times vcd   →  times  (already sorted int array)
    3. Return { vcd; idx = 0; times }

    OCAML PATTERNS:
    - Record construction: { field = value; field2 = value2; ... }
    - Function calls: Wal_vcd.parse_vcd path

    LEARNING NOTE:
    This is essentially a constructor function — it initialises a fresh
    [t] with the cursor pointing at the very first timestep (index 0). *)
let load (path : string) : t =
  ignore path;
  failwith "TODO: load — parse VCD, extract times, return { vcd; idx=0; times }"


(** [current_time tr] — the simulation timestamp the cursor is currently at.

    WHAT IT SHOULD DO:
    Return the integer timestamp at index [tr.idx] in [tr.times].
    If the times array is empty (no events in the VCD), return 0.

    ALGORITHM:
    1. If Array.length tr.times = 0  →  return 0
    2. Otherwise return  tr.times.(tr.idx)

    OCAML PATTERNS:
    - Array length: Array.length arr
    - Array indexing: arr.(i)  — note the dot-paren syntax, not arr[i]

    LEARNING NOTE:
    The guard against empty arrays is important! Indexing an empty array
    raises Invalid_argument at runtime. Always check length first. *)
let current_time (tr : t) : int =
  ignore tr;
  failwith "TODO: current_time — return tr.times.(tr.idx), or 0 if empty"


(** [num_timesteps tr] — how many unique timestamps exist in the trace.

    WHAT IT SHOULD DO:
    Return the total number of entries in [tr.times].

    ALGORITHM:
    - Array.length tr.times

    OCAML PATTERNS:
    - Array.length : 'a array -> int *)
let num_timesteps (tr : t) : int =
  ignore tr;
  failwith "TODO: num_timesteps — Array.length tr.times"


(** [step tr] — advance the cursor by exactly one timestep.
    If already at the last timestep, do nothing (safe no-op).

    WHAT IT SHOULD DO:
    Increment [tr.idx] by 1, but only if we are NOT already at the end.
    "At the end" means  tr.idx >= Array.length tr.times - 1.

    ALGORITHM:
    1. If tr.idx < Array.length tr.times - 1
       then tr.idx <- tr.idx + 1
       else do nothing

    OCAML PATTERNS:
    - Mutable field write: tr.idx <- new_value
    - Reading mutable field: tr.idx  (no special syntax to read)

    LEARNING NOTE:
    This is one of the few places in OCaml where we mutate state.
    The [<-] operator is ONLY valid on [mutable] record fields. *)
let step (tr : t) : unit =
  ignore tr;
  failwith "TODO: step — increment tr.idx if not at end"


(** [seek tr time] — move the cursor to the timestep closest to [time]
    (the first timestep whose timestamp is >= [time]).

    WHAT IT SHOULD DO:
    Scan through [tr.times] and set [tr.idx] to the first index i such that
    tr.times.(i) >= time.  If all times are less than [time], stop at the
    last index.

    ALGORITHM (linear scan):
    1. n = Array.length tr.times
    2. i = ref 0
    3. While !i < n - 1  AND  tr.times.(!i) < time:
         incr i              (* move forward *)
    4. tr.idx <- !i

    OCAML PATTERNS:
    - ref cell: let i = ref 0 in
    - Dereference: !i
    - Increment: incr i  (equivalent to i := !i + 1)
    - While loop: while condition do ... done
    - Logical AND: &&

    LEARNING NOTE:
    A binary search over [tr.times] would give O(log n) here, but linear
    scan is simpler and usually fine (seeking is rare, stepping is common).
    The condition [!i < n - 1] keeps us in-bounds — we stop at the last
    valid index even if [time] is beyond all timestamps. *)
let seek (tr : t) (time : int) : unit =
  ignore tr; ignore time;
  failwith "TODO: seek — linear scan tr.times to find first index >= time"


(** [reset tr] — move the cursor back to timestep 0 (the beginning).

    WHAT IT SHOULD DO:
    Set tr.idx to 0.

    ALGORITHM:
    - tr.idx <- 0

    LEARNING NOTE:
    Simple but important — every iteration function (for_each_timestep,
    map_timesteps, find_first) calls reset before and after the loop so
    the trace is always left in a clean state. *)
let reset (tr : t) : unit =
  ignore tr;
  failwith "TODO: reset — set tr.idx <- 0"


(** [at_end tr] — true if the cursor is at or past the last timestep.

    WHAT IT SHOULD DO:
    Return true iff there are no more timesteps to visit after the current one.

    ALGORITHM:
    - tr.idx >= Array.length tr.times - 1

    OCAML PATTERNS:
    - Boolean expression: just return it directly, no if/then needed

    LEARNING NOTE:
    This uses >= rather than = to be safe: if idx somehow exceeds the
    last index (shouldn't happen, but defensive), we still report at_end. *)
let at_end (tr : t) : bool =
  ignore tr;
  failwith "TODO: at_end — return tr.idx >= Array.length tr.times - 1"


(** [all_times tr] — return the full sorted array of timestamps.

    WHAT IT SHOULD DO:
    Return [tr.times] directly — it's already a sorted int array.

    ALGORITHM:
    - Just return tr.times

    LEARNING NOTE:
    This is a simple accessor. The times array was computed once at
    load time by Wal_vcd.all_times and never changes. *)
let all_times (tr : t) : int array =
  ignore tr;
  failwith "TODO: all_times — return tr.times"


(** [signal_value tr name] — get the value of signal [name] at the current time.
    Returns VNil if the signal doesn't exist in the VCD.

    WHAT IT SHOULD DO:
    1. Look up the signal by name in the VCD.
    2. If not found, return VNil.
    3. If found, query its value at the current time.
    4. Convert the Wal_vcd.vcd_value to a Wal_ast.value.

    ALGORITHM:
    1. Use Wal_vcd.find_signal tr.vcd name → Option.t
    2. Pattern match:
       | None      -> VNil
       | Some sig_ ->
           t   = current_time tr
           raw = Wal_vcd.value_at sig_ t
           Convert raw using:
             Wal_vcd.X      -> VSymbol "x"
             Wal_vcd.Z      -> VSymbol "z"
             Wal_vcd.Bit n  -> VInt n
             Wal_vcd.Bits s -> try VInt (int_of_string ("0b" ^ s))
                               with _ -> VStr s
             Wal_vcd.Real f -> VFloat f

    OCAML PATTERNS:
    - Nested match (match inside a match arm)
    - try/with for catching exceptions
    - String concatenation: "0b" ^ s
    - Qualified constructors: Wal_vcd.X, Wal_vcd.Bit n

    LEARNING NOTE:
    The "0b" prefix trick converts a binary string like "01001101" into
    the integer 0b01001101 = 77.  int_of_string understands 0b/0x/0o prefixes.
    We wrap it in try/with because a very wide bus might overflow int. *)
let signal_value (tr : t) (name : string) : value =
  ignore tr; ignore name;
  failwith "TODO: signal_value — lookup signal, call value_at, convert vcd_value to value"


(** [bind_signals_to_env tr env] — build a new env with all current signal values.

    WHAT IT SHOULD DO:
    For each signal name in the VCD, query its value at the current time,
    then extend [env] with those (name, value) bindings.

    ALGORITHM:
    1. names = Wal_vcd.signal_names tr.vcd   (list of all signal name strings)
    2. bindings = List.map (fun name -> (name, signal_value tr name)) names
    3. Wal_env.extend bindings env

    OCAML PATTERNS:
    - List.map : ('a -> 'b) -> 'a list -> 'b list
    - Tuple construction: (name, value)
    - Wal_env.extend : (string * value) list -> env -> env

    LEARNING NOTE:
    This is called once per timestep in evaluation. All signal names
    become variables in the WAL environment — so a WAL program can
    write (+ clk data) and the names clk and data resolve to their
    current simulation values. *)
let bind_signals_to_env (tr : t) (env : Wal_ast.env) : Wal_ast.env =
  ignore tr; ignore env;
  failwith "TODO: bind_signals_to_env — map signal names to values, extend env"


(** [for_each_timestep tr f] — iterate over every timestep, calling [f tr] at each.

    WHAT IT SHOULD DO:
    Reset to start, call [f tr] at each timestep in order, then reset again.
    This is the core of (for-each-timestep ...) in WAL.

    ALGORITHM:
    1. reset tr
    2. n = Array.length tr.times
    3. for _ = 0 to n - 1 do
         f tr;
         step tr
       done
    4. reset tr   (* leave trace in clean state *)

    OCAML PATTERNS:
    - for loop: for i = lo to hi do ... done
    - Unit sequencing: expr1; expr2

    LEARNING NOTE:
    We use [_] as the loop variable because we don't need the index —
    [tr.idx] tracks position internally. Note we call step AFTER f so
    that f sees the current timestep before we advance. *)
let for_each_timestep (tr : t) (f : t -> unit) : unit =
  ignore tr; ignore f;
  failwith "TODO: for_each_timestep — reset, loop calling f tr then step, reset"


(** [map_timesteps tr f] — like for_each_timestep but collects results into a list.

    WHAT IT SHOULD DO:
    Call [f tr] at each timestep and return all results as a list,
    in chronological order.

    ALGORITHM:
    1. reset tr
    2. n = Array.length tr.times
    3. result = ref []
    4. for _ = 0 to n - 1 do
         result := f tr :: !result;
         step tr
       done
    5. reset tr
    6. List.rev !result   (* reverse because we prepended *)

    OCAML PATTERNS:
    - ref list: let result = ref [] in
    - Prepend: result := new_elem :: !result
    - List.rev reverses the accumulated list back to forward order

    LEARNING NOTE:
    Prepending to a list then reversing is the idiomatic OCaml pattern
    for building lists in a loop. It's O(n) total, unlike appending
    which would be O(n²). *)
let map_timesteps (tr : t) (f : t -> 'a) : 'a list =
  ignore tr; ignore f;
  failwith "TODO: map_timesteps — reset, loop prepending f tr to result, reset, List.rev"


(** [find_first tr pred] — find the first timestep where [pred tr] is true.
    Returns [Some time] at that timestep, or [None] if predicate never holds.
    Stops early as soon as the predicate is satisfied.

    WHAT IT SHOULD DO:
    Walk timesteps from the start; as soon as [pred tr] returns true,
    capture the current time and stop the loop.

    ALGORITHM:
    1. reset tr
    2. n = Array.length tr.times
    3. result = ref None
    4. i = ref 0
    5. while !i < n && !result = None do
         if pred tr then result := Some (current_time tr);
         step tr;
         incr i
       done
    6. reset tr
    7. return !result

    OCAML PATTERNS:
    - Option ref: let result = ref None in
    - Comparison with None: !result = None
    - Some wrapping: Some (current_time tr)
    - while loop with two conditions: !i < n && !result = None

    LEARNING NOTE:
    The [&& !result = None] in the loop condition is the early-exit
    mechanism — once we find a match, the loop stops because the condition
    becomes false. This is "streaming" behaviour: we don't evaluate all
    timesteps if we find the answer early. *)
let find_first (tr : t) (pred : t -> bool) : int option =
  ignore tr; ignore pred;
  failwith "TODO: find_first — reset, while loop checking pred, early exit on match, reset"

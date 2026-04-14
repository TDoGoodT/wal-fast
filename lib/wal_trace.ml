(** wal_trace.ml
    The runtime trace object — the central "cursor" that WAL programs
    use to navigate a loaded simulation.

    LEARNING NOTE:
    This module introduces mutable state in OCaml.
    OCaml is mostly immutable, but sometimes you genuinely need mutation —
    like a "current time pointer" that steps forward as you iterate.
    We use [mutable] record fields for this. They behave like variables
    you can reassign with the <- operator. *)

open Wal_ast

(** The trace: a loaded VCD plus a current-time cursor. *)
type t = {
  vcd          : Wal_vcd.vcd;
  mutable idx  : int;       (** index into [times] array — current position *)
  times        : int array; (** all unique timestamps, sorted ascending      *)
}

(** Load a VCD file and build a trace. *)
let load (path : string) : t =
  let vcd   = Wal_vcd.parse_vcd path in
  let times = Wal_vcd.all_times vcd in
  { vcd; idx = 0; times }

(** Current simulation timestamp. *)
let current_time (tr : t) : int =
  if Array.length tr.times = 0 then 0
  else tr.times.(tr.idx)

(** Total number of unique timestamps. *)
let num_timesteps (tr : t) : int =
  Array.length tr.times

(** Advance the cursor by one timestep. No-op at end. *)
let step (tr : t) : unit =
  if tr.idx < Array.length tr.times - 1 then
    tr.idx <- tr.idx + 1

(** Seek to the timestep at or just after [time]. *)
let seek (tr : t) (time : int) : unit =
  (* Linear scan for now; could binary-search tr.times for speed *)
  let n = Array.length tr.times in
  let i = ref 0 in
  while !i < n - 1 && tr.times.(!i) < time do
    incr i
  done;
  tr.idx <- !i

(** Reset to the first timestep. *)
let reset (tr : t) : unit =
  tr.idx <- 0

(** True if the cursor is at the last timestep. *)
let at_end (tr : t) : bool =
  tr.idx >= Array.length tr.times - 1

(** Return all timestamps as an array. *)
let all_times (tr : t) : int array =
  tr.times

(** Get the value of a named signal at the current time.
    Returns VNil if the signal does not exist. *)
let signal_value (tr : t) (name : string) : value =
  match Wal_vcd.find_signal tr.vcd name with
  | None      -> VNil
  | Some sig_ ->
    let t   = current_time tr in
    let raw = Wal_vcd.value_at sig_ t in
    (* Convert vcd_value -> wal value *)
    match raw with
    | Wal_vcd.X        -> VSymbol "x"
    | Wal_vcd.Z        -> VSymbol "z"
    | Wal_vcd.Bit  n   -> VInt n
    | Wal_vcd.Bits s   ->
      (* Parse binary string to int for convenience *)
      (try VInt (int_of_string ("0b" ^ s))
       with _ -> VStr s)
    | Wal_vcd.Real f   -> VFloat f

(** Bind all signal values at the current time into an environment.
    This is called at each timestep so WAL programs can reference
    signals directly by name. *)
let bind_signals_to_env (tr : t) (env : Wal_ast.env) : Wal_ast.env =
  let names = Wal_vcd.signal_names tr.vcd in
  let bindings = List.map (fun name ->
    let v = signal_value tr name in
    (name, v)
  ) names in
  Wal_env.extend bindings env

(** Iterate over every timestep, calling [f tr] at each one.
    The trace cursor is advanced automatically.
    This is the core of (for-each-timestep ...) in WAL. *)
let for_each_timestep (tr : t) (f : t -> unit) : unit =
  reset tr;
  let n = Array.length tr.times in
  for _ = 0 to n - 1 do
    f tr;
    step tr
  done;
  reset tr

(** Collect results across all timesteps.
    Like for_each_timestep but accumulates a list of results. *)
let map_timesteps (tr : t) (f : t -> 'a) : 'a list =
  reset tr;
  let n      = Array.length tr.times in
  let result = ref [] in
  for _ = 0 to n - 1 do
    result := f tr :: !result;
    step tr
  done;
  reset tr;
  List.rev !result

(** Find the first timestep where [pred tr] returns true.
    Returns [Some time] or [None]. Stops early — streaming behavior. *)
let find_first (tr : t) (pred : t -> bool) : int option =
  reset tr;
  let n      = Array.length tr.times in
  let result = ref None in
  let i      = ref 0 in
  while !i < n && !result = None do
    if pred tr then result := Some (current_time tr);
    step tr;
    incr i
  done;
  reset tr;
  !result

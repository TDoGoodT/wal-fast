(** wal_vcd.ml
    VCD (Value Change Dump) file parser.

    VCD is the universal output format of hardware simulators (Icarus, Verilator,
    ModelSim, etc.). It is a plain-text format that records every signal change
    during simulation, tagged with a timestamp.

    LEARNING NOTE — Data Structures in this module
    ================================================
    - Hashtbl: OCaml's mutable hash table (like Python dict / Java HashMap).
      Create: Hashtbl.create initial_capacity
      Insert: Hashtbl.replace tbl key value
      Lookup: Hashtbl.find_opt tbl key  →  Some v | None
      Iterate: Hashtbl.iter (fun key value -> ...) tbl

    - Mutable record fields: updated with  record.field <- new_value

    - Array vs List: we store signal changes as arrays (not lists) so we can
      do binary search (O log n) rather than linear scan (O n).

    - String.split_on_char : char -> string -> string list
      Splits a string into tokens. Used for simple tokenisation.

    VCD FORMAT OVERVIEW:
    ─────────────────────
    $timescale 1ns $end
    $scope module cpu $end
    $var wire 1 ! clk $end
    $var wire 8 # data [7:0] $end
    $upscope $end
    $enddefinitions $end
    #0           <- timestamp
    b00000000 #  <- value change: bits "00000000" for signal "#"
    0!           <- value change: "0" for signal "!"
    #10
    1!
    b00000001 #
*)

[@@@warning "-27-50"]

(** The possible values a VCD signal can take. *)
type vcd_value =
  | X                (** unknown / uninitialized *)
  | Z                (** high-impedance           *)
  | Bit  of int      (** 0 or 1 (1-bit signal)   *)
  | Bits of string   (** multi-bit: "01001101"    *)
  | Real of float    (** real-valued signal       *)

(** One value-change event on a signal. *)
type change = {
  time  : int;
  value : vcd_value;
}

(** A single signal in the VCD. *)
type signal = {
  id      : string;        (** Short VCD identifier (e.g. "!", "#", "$$") *)
  name    : string;        (** Human name (e.g. "clk", "cpu.data")        *)
  width   : int;           (** Bit width                                   *)
  mutable changes : change array;  (** All value changes, sorted by time  *)
}

(** The parsed VCD file. *)
type vcd = {
  timescale : string;                       (** e.g. "1 ns"           *)
  signals   : (string, signal) Hashtbl.t;  (** id -> signal           *)
  by_name   : (string, signal) Hashtbl.t;  (** name -> signal         *)
}

(** Pretty print a vcd_value back to a string. *)
let show_value = function
  | X        -> "x"
  | Z        -> "z"
  | Bit  n   -> string_of_int n
  | Bits s   -> "b" ^ s
  | Real f   -> string_of_float f

(* ------------------------------------------------------------------ *)
(*  Parser internals — keep these as-is, they are infrastructure       *)
(* ------------------------------------------------------------------ *)

(** Read all lines from a file into a string list. *)
let read_lines path =
  let ic   = open_in path in
  let lines = ref [] in
  (try
    while true do
      lines := input_line ic :: !lines
    done
  with End_of_file -> ());
  close_in ic;
  List.rev !lines

(** Tokenize a VCD file: split on whitespace, flatten all lines. *)
let tokenize lines =
  lines
  |> List.concat_map (fun line ->
       String.split_on_char ' ' line
       |> List.concat_map (String.split_on_char '\t')
       |> List.filter (fun s -> String.length s > 0))

(** Consume tokens until we see "$end", returning the consumed tokens. *)
let rec collect_until_end acc = function
  | []              -> (List.rev acc, [])
  | "$end" :: rest  -> (List.rev acc, rest)
  | tok    :: rest  -> collect_until_end (tok :: acc) rest

(** Parse "$var wire WIDTH ID NAME ... $end" into a signal stub.
    We collect tokens until $end, then pull out the pieces we need. *)
let parse_var tokens =
  let (body, rest) = collect_until_end [] tokens in
  (* body = ["wire"/"reg"/..., "WIDTH", "ID", "NAME", ...] *)
  match body with
  | _kind :: width_s :: id :: name :: _ ->
    let width = int_of_string_opt width_s |> Option.value ~default:1 in
    (* Strip bus index from name: "data[7:0]" -> "data" *)
    let name  = match String.index_opt name '[' with
      | Some i -> String.sub name 0 i
      | None   -> name
    in
    let sig_ = { id; name; width; changes = [||] } in
    (Some sig_, rest)
  | _ ->
    (None, rest)

(** Parse a single value-change token.
    Scalar:  "1!"  -> id="!", value=Bit 1
    Vector:  "b01001101 #" -> id="#", value=Bits "01001101"
    Real:    "r3.14 %" -> id="%", value=Real 3.14 *)
let parse_scalar_change tok =
  if String.length tok < 2 then None
  else
    let c  = tok.[0] in
    let id = String.sub tok 1 (String.length tok - 1) in
    let v  = match c with
      | '0' -> Some (Bit 0)
      | '1' -> Some (Bit 1)
      | 'x' | 'X' -> Some X
      | 'z' | 'Z' -> Some Z
      | _   -> None
    in
    match v with
    | Some value -> Some (id, value)
    | None       -> None


(* ------------------------------------------------------------------ *)
(*  Main parser — YOUR JOB                                             *)
(* ------------------------------------------------------------------ *)

(** [parse_vcd path] — parse a VCD file and return a fully-populated [vcd].

    WHAT IT SHOULD DO:
    Read a VCD file, extract signal declarations and value-change events,
    and return a [vcd] record with:
      - timescale string
      - two hash tables: signals (by id) and by_name (by name)
      - each signal's [changes] field filled with a sorted array of events

    HIGH-LEVEL ALGORITHM:
    1. Read the file → list of lines
    2. Tokenize → flat list of string tokens
    3. Walk the token list with a recursive [process] function,
       maintaining mutable state:
         - cur_time   : int ref        — current timestamp
         - timescale  : string ref     — timescale from $timescale ... $end
         - sigs_by_id : Hashtbl       — signal records indexed by short VCD id
         - sigs_by_nm : Hashtbl       — same records indexed by human name
         - changes    : Hashtbl       — id → change list ref (accumulates events)
    4. After walking all tokens, convert each change list to a sorted array
       and update the signal records.
    5. Return { timescale; signals = sigs_by_id; by_name = sigs_by_nm }

    DETAILED ALGORITHM for [process tokens]:
    Pattern match on the token list (this is a recursive function over a list):

    | []  ->  ()    (* done *)

    | "$timescale" :: rest ->
        collect_until_end [] rest  →  (body, rest')
        timescale := String.concat " " body
        process rest'

    | "$var" :: rest ->
        parse_var rest  →  (Some sig_, rest')   or   (None, rest')
        If Some sig_:
          Hashtbl.replace sigs_by_id sig_.id sig_
          Hashtbl.replace sigs_by_nm sig_.name sig_
          ensure_changes sig_.id
        process rest'

    | "$scope" :: rest  |  "$upscope" :: rest
    | "$enddefinitions" :: rest  |  "$dumpvars" :: rest ->
        collect_until_end [] rest  →  (_, rest')
        process rest'

    | "$end" :: rest ->
        process rest   (* stray $end, skip *)

    | tok :: rest  when tok.[0] = '#' ->
        (* Timestamp token: "#1000" means time = 1000 *)
        t_str = String.sub tok 1 (String.length tok - 1)
        int_of_string_opt t_str  →  Some n  →  cur_time := n
        process rest

    | "b" :: bits :: id :: rest ->
        (* Rare: "b" as its own token, then the bit string, then the id *)
        add_change id (Bits bits)
        process rest

    | tok :: rest  when tok.[0] = 'b'  AND  String.length tok > 1 ->
        (* Common: "b01010101" as one token, next token is the signal id *)
        bits = String.sub tok 1 (String.length tok - 1)
        match rest with
        | id :: rest' -> add_change id (Bits bits); process rest'
        | []          -> ()

    | tok :: rest  when tok.[0] = 'r'  AND  String.length tok > 1 ->
        (* Real value: "r3.14" followed by signal id *)
        fstr = String.sub tok 1 (String.length tok - 1)
        match rest with
        | id :: rest' ->
            float_of_string_opt fstr  →  Some f  →  add_change id (Real f)
            process rest'
        | [] -> ()

    | tok :: rest ->
        (* Scalar change: "0!", "1#", "x$", "z%" *)
        parse_scalar_change tok  →  Some (id, value)  →  add_change id value
        process rest

    AFTER process completes:
    Hashtbl.iter over [changes]:
      For each id  →  lst_ref:
        arr = Array.of_list (List.rev !lst_ref)
        Array.sort (fun a b -> compare a.time b.time) arr
        Look up signal in sigs_by_id:
          s' = { s with changes = arr }     (* record update syntax! *)
          Hashtbl.replace sigs_by_id id s'
          Hashtbl.replace sigs_by_nm s.name s'

    OCAML PATTERNS:
    - Hashtbl.create n, Hashtbl.replace, Hashtbl.find, Hashtbl.mem,
      Hashtbl.find_opt, Hashtbl.iter
    - ref cells: let x = ref init_val in ... x := new_val ... !x
    - Pattern matching with guards: | tok :: rest when tok.[0] = '#' -> ...
    - String.length tok > 0 && tok.[0] = '#'  as guard
    - String.sub str start length
    - Array.of_list, Array.sort
    - Record update: { s with changes = arr }  creates a new record with
      all fields from s except changes, which is set to arr

    HELPER FUNCTIONS to define inside parse_vcd:

    let ensure_changes id =
      if not (Hashtbl.mem changes id) then
        Hashtbl.add changes id (ref [])

    let add_change id value =
      ensure_changes id;
      let lst = Hashtbl.find changes id in
      lst := { time = !cur_time; value } :: !lst

    LEARNING NOTE:
    The two-phase approach (accumulate as lists, then convert to arrays)
    is intentional: prepending to a list is O(1), but appending to an
    array would be O(n). We pay one O(n log n) sort per signal at the end.
    For a VCD with millions of events this matters. *)
let parse_vcd (path : string) : vcd =
  ignore path;
  failwith "TODO: parse_vcd — read lines, tokenize, walk tokens accumulating \
            changes in Hashtbl of list ref, then convert to sorted arrays"


(* ------------------------------------------------------------------ *)
(*  Query API — YOUR JOB                                               *)
(* ------------------------------------------------------------------ *)

(** [value_at sig_ time] — binary search for signal value at given time.

    WHAT IT SHOULD DO:
    Find the last change in sig_.changes that occurred at or before [time].
    Return X if no changes exist or all changes are after [time].

    ALGORITHM:
    1. Let arr = sig_.changes, n = Array.length arr
    2. If n = 0, return X
    3. If arr.(0).time > time, return X  (every change is after [time])
    4. Binary search: lo = ref 0, hi = ref (n - 1)
       Loop while !lo < !hi:
         mid = (!lo + !hi + 1) / 2      ← note: +1 avoids infinite loop
         if arr.(mid).time <= time
           then lo := mid
           else hi := mid - 1
    5. Return arr.(!lo).value

    OCAML PATTERNS:
    - Array indexing: arr.(i)
    - ref cells for mutable loop vars: let lo = ref 0 in
    - Mutation: lo := mid,  hi := mid - 1
    - Dereference: !lo, !hi
    - while loop: while !lo < !hi do ... done
    - begin...end block groups multiple expressions after then/else

    LEARNING NOTE:
    This is O(log n) binary search. The [+1] in mid calculation is crucial:
    when lo = hi - 1, without +1 we'd get mid = lo, and if arr.(lo).time <= time
    we'd set lo := lo — infinite loop! With +1, mid = hi, and we always
    make progress. This is the standard "upper bound" binary search idiom. *)
let value_at (sig_ : signal) (time : int) : vcd_value =
  ignore sig_; ignore time;
  failwith "TODO: value_at — binary search sig_.changes for last change <= time, return X if none"


(** [all_times vcd] — return a sorted array of ALL unique timestamps
    across ALL signals in the VCD.

    WHAT IT SHOULD DO:
    Collect every timestamp that appears in any signal's change list,
    deduplicate, sort ascending, and return as an int array.

    ALGORITHM:
    1. Create a Hashtbl [set] mapping int → unit  (used as a set)
    2. Hashtbl.iter over vcd.signals:
         For each signal, Array.iter over its changes:
           Hashtbl.replace set c.time ()
    3. Collect keys: Hashtbl.fold (fun t () acc -> t :: acc) set []
    4. Convert to array: Array.of_list times
    5. Sort: Array.sort compare arr
    6. Return arr

    OCAML PATTERNS:
    - Hashtbl as a set: (string/int, unit) Hashtbl.t
    - Hashtbl.replace deduplicates (replacing is fine, value is ())
    - Hashtbl.fold : (key -> value -> acc -> acc) -> tbl -> init -> acc
    - Array.iter : ('a -> unit) -> 'a array -> unit
    - Array.sort compare arr  — sorts in-place using polymorphic compare

    LEARNING NOTE:
    Using a Hashtbl as a set is idiomatic OCaml for deduplication without
    a separate Set module. The unit value is a zero-byte placeholder.
    Array.sort sorts IN PLACE (mutates arr), unlike List.sort which returns
    a new list. *)
let all_times (vcd : vcd) : int array =
  ignore vcd;
  failwith "TODO: all_times — collect all change timestamps into a Hashtbl set, sort, return array"


(** [find_signal vcd name] — look up a signal by its human-readable name.
    Returns [Some signal] if found, [None] if no signal has that name.

    WHAT IT SHOULD DO:
    Query the [by_name] hash table in [vcd].

    ALGORITHM:
    - Hashtbl.find_opt vcd.by_name name

    OCAML PATTERNS:
    - Hashtbl.find_opt : ('a, 'b) Hashtbl.t -> 'a -> 'b option
    - Returns Some v if found, None if absent — never raises Not_found *)
let find_signal (vcd : vcd) (name : string) : signal option =
  ignore vcd; ignore name;
  failwith "TODO: find_signal — Hashtbl.find_opt vcd.by_name name"


(** [signal_names vcd] — list all human-readable signal names in the VCD.

    WHAT IT SHOULD DO:
    Return a list of all keys in [vcd.by_name].

    ALGORITHM:
    - Hashtbl.fold (fun name _ acc -> name :: acc) vcd.by_name []

    OCAML PATTERNS:
    - Hashtbl.fold : (key -> val -> acc -> acc) -> tbl -> init -> acc
    - [_] ignores the signal value, we only want the name (key)

    LEARNING NOTE:
    The order is unspecified (hash tables are unordered). That's fine —
    callers who need sorted names can call List.sort on the result. *)
let signal_names (vcd : vcd) : string list =
  ignore vcd;
  failwith "TODO: signal_names — Hashtbl.fold collecting keys from vcd.by_name"

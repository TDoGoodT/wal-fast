(** wal_vcd.ml
    VCD (Value Change Dump) file parser.

    VCD is the universal output format of hardware simulators (Icarus, Verilator,
    ModelSim, etc.). It is a plain-text format that records every signal change
    during simulation, tagged with a timestamp.

    LEARNING NOTE:
    This module introduces:
    - Hashtbl: OCaml's mutable hash table (like Python dict)
    - Mutable record fields
    - Array vs List: we use arrays here for O(log n) binary search at query time
    - String scanning without a full parser library
    - Reading files line by line with input_line

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
(*  Parser internals                                                     *)
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

(** Main VCD parser.
    Returns a [vcd] record with all signals and their change history. *)
let parse_vcd (path : string) : vcd =
  let lines  = read_lines path in
  let tokens = tokenize lines in

  (* Mutable state during parse *)
  let timescale  = ref "1ns" in
  let sigs_by_id : (string, signal) Hashtbl.t = Hashtbl.create 64 in
  let sigs_by_nm : (string, signal) Hashtbl.t = Hashtbl.create 64 in
  (* Accumulate changes as lists, then convert to sorted arrays at end *)
  let changes    : (string, change list ref) Hashtbl.t = Hashtbl.create 64 in
  let cur_time   = ref 0 in

  let ensure_changes id =
    if not (Hashtbl.mem changes id) then
      Hashtbl.add changes id (ref [])
  in

  let add_change id value =
    ensure_changes id;
    let lst = Hashtbl.find changes id in
    lst := { time = !cur_time; value } :: !lst
  in

  (* Walk the token stream *)
  let rec process = function
    | [] -> ()

    | "$timescale" :: rest ->
      let (body, rest') = collect_until_end [] rest in
      timescale := String.concat " " body;
      process rest'

    | "$var" :: rest ->
      let (sig_opt, rest') = parse_var rest in
      (match sig_opt with
      | Some s ->
        Hashtbl.replace sigs_by_id s.id s;
        Hashtbl.replace sigs_by_nm s.name s;
        ensure_changes s.id
      | None -> ());
      process rest'

    | "$scope" :: rest ->
      let (_, rest') = collect_until_end [] rest in
      process rest'

    | "$upscope" :: rest ->
      let (_, rest') = collect_until_end [] rest in
      process rest'

    | "$enddefinitions" :: rest ->
      let (_, rest') = collect_until_end [] rest in
      process rest'

    | "$dumpvars" :: rest ->
      let (_, rest') = collect_until_end [] rest in
      process rest'

    | "$end" :: rest -> process rest

    | tok :: rest when String.length tok > 0 && tok.[0] = '#' ->
      (* Timestamp *)
      let t = String.sub tok 1 (String.length tok - 1) in
      (match int_of_string_opt t with
      | Some n -> cur_time := n
      | None   -> ());
      process rest

    | "b" :: bits :: id :: rest ->
      add_change id (Bits bits);
      process rest

    | tok :: rest when String.length tok > 1 && tok.[0] = 'b' ->
      (* "b01010101" followed by id token *)
      let bits = String.sub tok 1 (String.length tok - 1) in
      (match rest with
      | id :: rest' -> add_change id (Bits bits); process rest'
      | []          -> ())

    | tok :: rest when String.length tok > 1 && tok.[0] = 'r' ->
      (* Real value *)
      let fstr = String.sub tok 1 (String.length tok - 1) in
      (match rest with
      | id :: rest' ->
        (match float_of_string_opt fstr with
        | Some f -> add_change id (Real f)
        | None   -> ());
        process rest'
      | [] -> ())

    | tok :: rest ->
      (* Scalar change: "0!", "1#", "x$", "z%" *)
      (match parse_scalar_change tok with
      | Some (id, value) -> add_change id value
      | None             -> ());
      process rest
  in
  process tokens;

  (* Convert change lists to sorted arrays *)
  Hashtbl.iter (fun id lst_ref ->
    let arr = Array.of_list (List.rev !lst_ref) in
    (* Already in time order since we reversed; but sort to be safe *)
    Array.sort (fun a b -> compare a.time b.time) arr;
    match Hashtbl.find_opt sigs_by_id id with
    | Some s ->
      (* Replace the signal with one that has the changes filled in.
         LEARNING NOTE: Record update syntax { s with changes = arr }
         creates a NEW record with all fields from s, but changes replaced. *)
      let s' = { s with changes = arr } in
      Hashtbl.replace sigs_by_id id s';
      Hashtbl.replace sigs_by_nm s.name s'
    | None -> ()
  ) changes;

  { timescale = !timescale; signals = sigs_by_id; by_name = sigs_by_nm }

(* ------------------------------------------------------------------ *)
(*  Query API                                                            *)
(* ------------------------------------------------------------------ *)

(** Binary search: find the value of [signal] at [time].
    Returns the value of the last change AT OR BEFORE [time].

    LEARNING NOTE:
    Binary search in OCaml arrays is manual — we track lo/hi indices
    and narrow down. This gives O(log n) lookup vs O(n) linear scan.
    For signals with millions of changes this is critical. *)
let value_at (sig_ : signal) (time : int) : vcd_value =
  let arr = sig_.changes in
  let n   = Array.length arr in
  if n = 0 then X
  else if arr.(0).time > time then X
  else begin
    let lo = ref 0 in
    let hi = ref (n - 1) in
    while !lo < !hi do
      let mid = (!lo + !hi + 1) / 2 in
      if arr.(mid).time <= time
      then lo := mid
      else hi := mid - 1
    done;
    arr.(!lo).value
  end

(** List all unique timestamps across all signals. *)
let all_times (vcd : vcd) : int array =
  let set = Hashtbl.create 1024 in
  Hashtbl.iter (fun _ sig_ ->
    Array.iter (fun c -> Hashtbl.replace set c.time ()) sig_.changes
  ) vcd.signals;
  let times = Hashtbl.fold (fun t () acc -> t :: acc) set [] in
  let arr   = Array.of_list times in
  Array.sort compare arr;
  arr

(** Look up a signal by name. *)
let find_signal (vcd : vcd) (name : string) : signal option =
  Hashtbl.find_opt vcd.by_name name

(** List all signal names. *)
let signal_names (vcd : vcd) : string list =
  Hashtbl.fold (fun name _ acc -> name :: acc) vcd.by_name []

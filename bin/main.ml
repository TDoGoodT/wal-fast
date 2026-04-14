(** main.ml
    WAL-FAST command-line interface.

    Subcommands:
      wal-fast run   <script.wal> <trace.vcd>   -- evaluate a WAL script
      wal-fast repl  <trace.vcd>                -- interactive REPL
      wal-fast index <trace.vcd>                -- show trace info

    LEARNING NOTE:
    Cmdliner is the standard OCaml library for building CLI tools.
    It uses a declarative style: you describe your arguments/flags,
    and Cmdliner generates parsing, --help, and man pages for you. *)

open Wal_fast

(* ------------------------------------------------------------------ *)
(*  Shared helpers                                                       *)
(* ------------------------------------------------------------------ *)

(** Load a trace, printing a friendly error and exiting on failure. *)
let load_trace path =
  try Wal_trace.load path
  with e ->
    Printf.eprintf "Error loading VCD file %s:\n  %s\n" path
      (Printexc.to_string e);
    exit 1

(** Build the base WAL environment with all stdlib bindings + trace signals. *)
let make_env trace =
  let env = Wal_eval.initial_env () in
  Wal_trace.bind_signals_to_env trace env

(** Evaluate a WAL expression string, printing the result. *)
let eval_and_print env expr_str =
  try
    let exprs = Wal_reader.parse_program expr_str in
    let result = ref Wal_ast.VNil in
    List.iter (fun expr ->
      result := Wal_eval.eval expr env
    ) exprs;
    let s = Wal_ast.show_value !result in
    if s <> "()" then print_endline s
  with
  | Wal_reader.Parse_error (msg, pos) ->
    Printf.eprintf "Parse error at position %d: %s\n" pos msg
  | Failure msg ->
    Printf.eprintf "Error: %s\n" msg
  | e ->
    Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e)

(* ------------------------------------------------------------------ *)
(*  run subcommand                                                       *)
(* ------------------------------------------------------------------ *)

let cmd_run script_path vcd_path =
  let trace = load_trace vcd_path in
  let env   = make_env trace in
  (try
    let exprs = Wal_reader.parse_file script_path in
    List.iter (fun expr ->
      let v = Wal_eval.eval expr env in
      let s = Wal_ast.show_value v in
      if s <> "()" then print_endline s
    ) exprs
  with
  | Wal_reader.Parse_error (msg, pos) ->
    Printf.eprintf "Parse error in %s at position %d: %s\n"
      script_path pos msg; exit 1
  | Failure msg ->
    Printf.eprintf "Error: %s\n" msg; exit 1
  | e ->
    Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e); exit 1)

(* ------------------------------------------------------------------ *)
(*  repl subcommand                                                      *)
(* ------------------------------------------------------------------ *)

let cmd_repl vcd_path =
  let trace = load_trace vcd_path in
  let env   = ref (make_env trace) in
  Printf.printf "WAL-FAST REPL — loaded %s\n" vcd_path;
  Printf.printf "Signals: %d   Timesteps: %d\n"
    (Hashtbl.length trace.vcd.signals)
    (Wal_trace.num_timesteps trace);
  Printf.printf "Type (quit) or Ctrl-D to exit.\n\n";
  (try
    while true do
      print_string "wal> ";
      flush stdout;
      let line = input_line stdin in
      if String.trim line = "" then ()
      else if String.trim line = "(quit)" then raise Exit
      else begin
        (* Update signal bindings at current time before each eval *)
        env := Wal_trace.bind_signals_to_env trace !env;
        eval_and_print !env line
      end
    done
  with
  | Exit | End_of_file -> print_endline "\nBye!")

(* ------------------------------------------------------------------ *)
(*  index subcommand                                                     *)
(* ------------------------------------------------------------------ *)

let cmd_index vcd_path =
  let trace = load_trace vcd_path in
  let times = Wal_trace.all_times trace in
  let n     = Array.length times in
  Printf.printf "File      : %s\n" vcd_path;
  Printf.printf "Timescale : %s\n" trace.vcd.timescale;
  Printf.printf "Signals   : %d\n" (Hashtbl.length trace.vcd.signals);
  Printf.printf "Timesteps : %d\n" n;
  if n > 0 then begin
    Printf.printf "Time range: %d — %d\n" times.(0) times.(n - 1);
    Printf.printf "\nSignals:\n";
    Wal_vcd.signal_names trace.vcd
    |> List.sort compare
    |> List.iter (fun name ->
         match Wal_vcd.find_signal trace.vcd name with
         | None   -> ()
         | Some s ->
           Printf.printf "  %-30s  %d-bit  %d changes\n"
             name s.width (Array.length s.changes))
  end

(* ------------------------------------------------------------------ *)
(*  Cmdliner wiring                                                      *)
(* ------------------------------------------------------------------ *)

open Cmdliner

let vcd_arg =
  Arg.(required & pos 0 (some file) None &
       info [] ~docv:"TRACE.VCD" ~doc:"VCD simulation trace file.")

let script_arg =
  Arg.(required & pos 0 (some file) None &
       info [] ~docv:"SCRIPT.WAL" ~doc:"WAL analysis script.")

let vcd_arg1 =
  Arg.(required & pos 1 (some file) None &
       info [] ~docv:"TRACE.VCD" ~doc:"VCD simulation trace file.")

(* run *)
let run_cmd =
  let doc = "Evaluate a WAL script against a VCD trace." in
  let term = Term.(const (fun s v -> cmd_run s v) $ script_arg $ vcd_arg1) in
  Cmd.v (Cmd.info "run" ~doc) term

(* repl *)
let repl_cmd =
  let doc = "Start an interactive WAL REPL loaded with a VCD trace." in
  let term = Term.(const cmd_repl $ vcd_arg) in
  Cmd.v (Cmd.info "repl" ~doc) term

(* index *)
let index_cmd =
  let doc = "Show summary information about a VCD trace." in
  let term = Term.(const cmd_index $ vcd_arg) in
  Cmd.v (Cmd.info "index" ~doc) term

(* root *)
let main_cmd =
  let doc = "Blazing fast Waveform Analysis Language for huge EDA simulations." in
  let info = Cmd.info "wal-fast" ~version:"0.1.0" ~doc in
  Cmd.group info [run_cmd; repl_cmd; index_cmd]

let () = exit (Cmd.eval main_cmd)

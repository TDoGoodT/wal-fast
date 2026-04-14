[@@@warning "-27-39-50"]
(* wal_ast.ml
    The Abstract Syntax Tree for the WAL (Waveform Analysis Language).

    WAL is a Lisp dialect, so the AST is beautifully simple:
    every expression is either an atom or a list of expressions.

    LEARNING NOTE:
    This is your first taste of OCaml's killer feature — Algebraic Data Types (ADTs).
    Instead of a class hierarchy or tagged unions, we declare every possible shape
    of a WAL value in one place. The compiler will then warn us if we ever forget
    to handle a case. No null pointer exceptions. No runtime surprises. *)

(** A WAL expression — the fundamental unit of the language. *)
type expr =
  | Int    of int          (* 42, -7                          *)
  | Float  of float        (* 3.14                            *)
  | Bool   of bool         (* #t  #f                          *)
  | Str    of string       (* "hello"                         *)
  | Symbol of string       (* clk, rising-edge, my-signal     *)
  | Nil                    (* empty list: ()                  *)
  | List   of expr list    (* (f arg1 arg2), (define x 42)    *)

(** A WAL value — what expressions evaluate TO at runtime.
    Mostly the same shapes, but adds Closure for functions
    and keeps the distinction clean between code and data. *)
type value =
  | VInt     of int
  | VFloat   of float
  | VBool    of bool
  | VStr     of string
  | VSymbol  of string           (* unevaluated symbol (quoted) *)
  | VNil                         (* () — the empty list         *)
  | VList    of value list
  | VSignal  of string           (* a hardware signal name      *)
  | VClosure of closure

and closure = {
  params : string list;          (* parameter names             *)
  body   : expr;                 (* the unevaluated body        *)
  env    : env;                  (* captured lexical environment *)
}

(** The environment: a chain of scopes, each mapping names to values.
    We forward-declare it here so [closure] can reference it.
    The real implementation lives in wal_env.ml. *)
and env = (string * value) list list

(* ------------------------------------------------------------------ *)
(*  Pretty-printing                                                      *)
(* ------------------------------------------------------------------ *)

(** [show_value v] — pretty-print a value back to WAL syntax.
    Useful for the REPL and error messages.

    WHAT IT SHOULD DO:
    Convert every variant of [value] into its textual WAL representation.
    Examples:
      VInt 42       -> "42"
      VBool true    -> "#t"
      VBool false   -> "#f"
      VStr "hi"     -> "\"hi\""   (with quotes, using %S format)
      VNil          -> "()"
      VList [VInt 1; VInt 2] -> "(1 2)"
      VSignal "clk" -> "#<signal:clk>"
      VClosure _    -> "#<closure>"

    ALGORITHM / APPROACH:
    Use [let rec] because VList contains values recursively.
    Pattern-match on every constructor. For VList, use:
      String.concat " " (List.map show_value vs)
    to convert each element and join with spaces, then wrap in "(" and ")".

    OCAML PATTERNS:
    - [function] is shorthand for [fun x -> match x with]
    - [Printf.sprintf "%S" s] formats s as a quoted OCaml string literal
      (i.e., it adds the surrounding double-quotes and escapes special chars)
    - [String.concat sep lst] joins a list of strings with [sep] between them
    - [List.map f lst] transforms every element of lst with f

    EDGE CASES:
    - VBool has TWO cases: true and false. Match them separately.
    - VClosure doesn't need to show its internals — just "#<closure>". *)
let rec show_value = function
  | VInt n       -> failwith "TODO"
  | VFloat f     -> failwith "TODO"
  | VBool true   -> failwith "TODO"
  | VBool false  -> failwith "TODO"
  | VStr s       -> failwith "TODO"
  | VSymbol s    -> failwith "TODO"
  | VNil         -> failwith "TODO"
  | VList vs     -> failwith "TODO"
  | VSignal s    -> failwith "TODO"
  | VClosure _   -> failwith "TODO"

(** [show_expr e] — pretty-print an expression (for debugging).

    WHAT IT SHOULD DO:
    Same as show_value but for the [expr] type instead of [value].
    Examples:
      Int 7        -> "7"
      Bool true    -> "#t"
      Symbol "clk" -> "clk"
      Nil          -> "()"
      List [Symbol "f"; Int 1] -> "(f 1)"

    ALGORITHM / APPROACH:
    Identical structure to show_value. Pattern-match every [expr] variant.
    For [List exprs], map show_expr over the list and join with spaces.

    OCAML PATTERNS:
    - Again, use [let rec] because List contains exprs recursively.
    - The [function] keyword works the same way here. *)
let rec show_expr = function
  | Int n        -> failwith "TODO"
  | Float f      -> failwith "TODO"
  | Bool true    -> failwith "TODO"
  | Bool false   -> failwith "TODO"
  | Str s        -> failwith "TODO"
  | Symbol s     -> failwith "TODO"
  | Nil          -> failwith "TODO"
  | List exprs   -> failwith "TODO"

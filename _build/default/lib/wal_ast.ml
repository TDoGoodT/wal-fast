(** wal_ast.ml
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

(** Pretty-print a value back to WAL syntax.
    Useful for the REPL and error messages. *)
let rec show_value = function
  | VInt n       -> string_of_int n
  | VFloat f     -> string_of_float f
  | VBool true   -> "#t"
  | VBool false  -> "#f"
  | VStr s       -> Printf.sprintf "%S" s
  | VSymbol s    -> s
  | VNil         -> "()"
  | VList vs     -> "(" ^ String.concat " " (List.map show_value vs) ^ ")"
  | VSignal s    -> Printf.sprintf "#<signal:%s>" s
  | VClosure _   -> "#<closure>"

(** Pretty-print an expression (for debugging). *)
let rec show_expr = function
  | Int n        -> string_of_int n
  | Float f      -> string_of_float f
  | Bool true    -> "#t"
  | Bool false   -> "#f"
  | Str s        -> Printf.sprintf "%S" s
  | Symbol s     -> s
  | Nil          -> "()"
  | List exprs   -> "(" ^ String.concat " " (List.map show_expr exprs) ^ ")"

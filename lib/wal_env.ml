(** wal_env.ml
    The lexical environment: maps symbol names to values.

    LEARNING NOTE:
    An "environment" in a Lisp interpreter is just a stack of scopes.
    Each scope is a list of (name, value) pairs.
    When you look up a name, you search from innermost scope outward.
    When you enter a (let ...) or call a function, you push a new scope.
    When you leave, you pop it.

    We use OCaml's immutable lists — no mutation, no bugs from aliasing.
    A child scope is literally [new_scope :: parent_env].
    The parent is untouched. Closures capture a snapshot of the env
    at the moment they are defined — for free, because lists are immutable. *)

open Wal_ast

(** The empty environment — a single empty scope. *)
let empty : env = [[]]

(** Look up a name. Returns [Some value] or [None].

    LEARNING NOTE: [List.assoc_opt] searches a [(key, value) list]
    for the first matching key. We search each scope in order. *)
let rec lookup (name : string) (env : env) : value option =
  match env with
  | []            -> None
  | scope :: rest ->
    match List.assoc_opt name scope with
    | Some v -> Some v
    | None   -> lookup name rest

(** Look up a name, raising an error if not found. *)
let lookup_exn (name : string) (env : env) : value =
  match lookup name env with
  | Some v -> v
  | None   -> failwith (Printf.sprintf "Unbound symbol: %s" name)

(** Extend the environment with a new scope containing the given bindings.

    LEARNING NOTE: [(name, value) :: bindings] prepends a pair to a list.
    [new_scope :: env] prepends the new scope to the scope stack. *)
let extend (bindings : (string * value) list) (env : env) : env =
  bindings :: env

(** Bind a single name in the current (innermost) scope.

    LEARNING NOTE: This is used for [define] at the top level.
    We replace the head scope with an updated version. *)
let define (name : string) (value : value) (env : env) : env =
  match env with
  | []            -> [[(name, value)]]
  | scope :: rest -> ((name, value) :: scope) :: rest

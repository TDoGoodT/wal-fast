[@@@warning "-27-39-50"]
(* wal_env.ml
    The lexical environment: maps symbol names to values.

    LEARNING NOTE — What is an "environment"?
    ==========================================
    An environment in a Lisp interpreter is a *stack of scopes*.
    Each scope is a list of (name, value) pairs — think of it as a
    mini dictionary.

    When you look up a name, you search from the innermost scope outward.
    When you enter a function call or a (let ...) form, you *push* a new scope.
    When you leave, you pop it — but in practice with immutable lists, you
    just stop using it and the old env is still intact.

    We represent env as:   (string * value) list list
    i.e. a list of scopes, each scope being a list of (name, value) pairs.

    Example:
      outer scope:  [("x", VInt 1); ("y", VInt 2)]
      inner scope:  [("z", VInt 3)]
      env = [ [("z", 3)]; [("x", 1); ("y", 2)] ]

    Looking up "x" → not in inner → found in outer → VInt 1.
    Looking up "z" → found in inner → VInt 3.

    OCaml concepts to notice:
    - [type env = ...] is defined in wal_ast.ml (we just [open] it here)
    - [List.assoc_opt k lst] searches a [(key,value) list] for key k
    - [match ... with | Some v -> ... | None -> ...] is OCaml's null safety
    - Immutable lists: extending the env never mutates existing scopes *)

open Wal_ast

(** The empty environment — a single empty scope. *)
let empty : env = [[]]

(** [lookup name env] searches the scope chain for [name].
    Returns [Some value] if found, [None] if not.

    WHAT IT SHOULD DO:
    Walk through the list of scopes (outermost to innermost — remember,
    head of the list is the innermost scope). For each scope, check if
    [name] appears. Return as soon as you find it.

    ALGORITHM:
    Recursive match on the env list:
      - [] (no scopes left)    → None
      - scope :: rest          → try List.assoc_opt name scope
                                 if Some v → return Some v
                                 if None   → recurse on rest

    HINT: Use a recursive match:
      match env with
      | []            -> None
      | scope :: rest -> match List.assoc_opt name scope with
                         | Some v -> Some v
                         | None   -> lookup name rest

    OCAML PATTERNS:
    - [List.assoc_opt : 'a -> ('a * 'b) list -> 'b option]
      searches an association list (list of pairs) by key, safely.
    - Nested match: the inner match on [List.assoc_opt] result.
    - [let rec] for recursion.

    EDGE CASES:
    - What if the same name appears in both an inner and outer scope?
      The inner one should win — and it will naturally, because you
      search the head scope first. *)
let rec lookup (name : string) (env : env) : value option =
  (* TODO: implement me!
     match env with
     | []            -> ???
     | scope :: rest -> match List.assoc_opt name scope with
                        | Some v -> ???
                        | None   -> ??? *)
  ignore name; ignore env;
  failwith "TODO: lookup"

(** [lookup_exn name env] — same as lookup but raises an error if not found.

    WHAT IT SHOULD DO:
    A convenience wrapper around [lookup] that converts [None] into a
    raised exception. This is the "exception-throwing" variant used by
    code that expects the name to be defined.

    ALGORITHM:
    Call [lookup name env] and pattern-match the result:
      - Some v → return v
      - None   → failwith (an informative message like "Unbound variable: <name>")

    HINT: match lookup name env with Some v -> v | None -> failwith ...

    OCAML PATTERNS:
    - This is the classic "option to exception" conversion pattern.
    - You'll see it often: try the safe version, then unwrap or fail. *)
let lookup_exn (name : string) (env : env) : value =
  (* TODO: match lookup name env with Some v -> ... | None -> failwith ... *)
  ignore name; ignore env;
  failwith "TODO: lookup_exn"

(** [extend bindings env] — push a new scope onto the env.

    WHAT IT SHOULD DO:
    Create a new environment with [bindings] as the innermost (head) scope,
    and the existing [env] scopes following behind.

    Example:
      extend [("x", VInt 1); ("y", VInt 2)] outer_env
      → [("x",1);("y",2)] :: outer_env

    ALGORITHM:
    It's literally one line — prepend [bindings] to [env] with the [::] cons operator.

    HINT: bindings :: env

    OCAML PATTERNS:
    - [x :: xs] prepends element x to list xs.
    - This is pure and non-mutating: the original env is unchanged.
      The caller gets a new env with an extra scope on top.

    WHY THIS MATTERS:
    This is how function calls and let-bindings create local scopes.
    When we apply a function, we extend the closure's environment with
    the argument bindings, evaluate the body, then "discard" the extension
    by simply not using it anymore. Immutability makes this free. *)
let extend (bindings : (string * value) list) (env : env) : env =
  (* TODO: bindings :: env *)
  ignore bindings; ignore env;
  failwith "TODO: extend"

(** [define name value env] — bind [name] in the innermost scope.

    WHAT IT SHOULD DO:
    Unlike [extend] (which pushes a whole new scope), [define] adds a
    single binding to the *existing* head scope. This is used for
    top-level [define] forms — they add to the global scope, not create
    a new one.

    Example:
      define "x" (VInt 42) [[("y", VInt 1)]; [("z", VInt 2)]]
      → [[("x", VInt 42); ("y", VInt 1)]; [("z", VInt 2)]]
      ^^ "x" added to the head scope, not a new scope pushed

    ALGORITHM:
    Match on [env]:
      - [] (shouldn't happen with a well-formed env) → create a fresh scope
      - scope :: rest → prepend (name, value) to scope, keep rest
        i.e. ((name, value) :: scope) :: rest

    HINT:
      match env with
      | []            -> [[(name, value)]]          (* shouldn't happen *)
      | scope :: rest -> ((name, value) :: scope) :: rest

    OCAML PATTERNS:
    - Record update syntax (not needed here, but related concept).
    - Pattern matching on list structure with :: is idiomatic OCaml.

    WHY DEFINE VS EXTEND?
    [extend] adds a fresh scope (used for function calls / let-bindings).
    [define] mutates the existing top scope (used for top-level definitions).
    This distinction is important for the REPL — you want (define x 1)
    followed by (define y (+ x 1)) to both live in the same global scope. *)
let define (name : string) (value : value) (env : env) : env =
  (* TODO *)
  ignore name; ignore value; ignore env;
  failwith "TODO: define"

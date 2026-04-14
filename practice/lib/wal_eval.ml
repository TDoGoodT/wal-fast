[@@@warning "-27-50"]
(* wal_eval.ml
    Tree-walking evaluator: takes an AST expression and an environment,
    returns a runtime value.

    LEARNING NOTE:
    "Tree-walking" means we traverse the AST directly, node by node, without
    compiling to bytecode or native code first.  It's the simplest possible
    interpreter architecture.  The trade-off: easy to understand and modify,
    but slower than a VM or compiled approach.

    The key recursive function is [eval : expr -> env -> value].
    It pattern-matches on every variant of [expr] and either returns a
    value immediately (atoms) or recursively evaluates sub-expressions. *)

open Wal_ast
open Wal_env

(* ------------------------------------------------------------------ *)
(*  Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(** [eval_list exprs env] — evaluate a list of expressions left-to-right
    and return the list of results.

    LEARNING NOTE:
    [List.map (fun e -> eval e env) exprs] would work but is not guaranteed
    to evaluate left-to-right in OCaml (the language standard leaves
    evaluation order of function arguments unspecified).  Using an explicit
    fold or map with a helper that sequences effects is safer for side-
    effecting languages.  For our pure-ish evaluator, [List.map] is fine,
    but we make it explicit anyway for clarity.

    WHAT IT SHOULD DO:
    Take a list of expressions and an environment, evaluate each expression
    in the environment, and return the list of resulting values in order.

    ALGORITHM:
    Use List.map with a lambda: List.map (fun e -> eval e env) exprs
    This applies [eval] to each expression with the same environment.

    OCAML PATTERNS:
    - [List.map f lst] applies f to each element of lst, collecting results.
    - Since eval and eval_list are in a [let rec ... and ...] block,
      they can call each other freely.

    NOTE: This function is part of a mutually recursive group with [apply]
    and [eval]. Look for the [let rec ... and ... and ...] syntax below. *)
(* Built-in function table — populated by register() calls later.
   Declared here so apply (in the let rec block below) can reference it. *)
let builtins : (string, value list -> value) Hashtbl.t =
  Hashtbl.create 64

(* eval_list, apply, and eval are mutually recursive.
   LEARNING NOTE: [let rec f x = … and g y = …] is OCaml's syntax for
   mutual recursion. Both f and g are in scope inside each other's body. *)

(** [eval_list exprs env] — evaluate each expression and return all values.

    WHAT IT SHOULD DO:
    Map [eval] over [exprs] with the given [env].

    ALGORITHM:
    List.map (fun e -> eval e env) exprs

    OCAML PATTERNS:
    - List.map applies a function to every element of a list.
    - This is part of a [let rec ... and ...] mutual recursion block. *)
let rec eval_list exprs env =
  failwith "TODO: eval_list — hint: List.map (fun e -> eval e env) exprs"

(** [apply func args] — call [func] with [args], returning the result.

    WHAT IT SHOULD DO:
    This is the function call mechanism. Two cases:
    1. [func] is a built-in sentinel closure → look up and call OCaml function
    2. [func] is a user-defined closure → bind params to args, eval body

    ALGORITHM:
    Match on [func]:
    Case 1 — built-in sentinel:
      VClosure { params = []; body = Symbol sentinel; env = _ }
      when the sentinel starts with "__builtin__:" →
        extract the name (String.sub sentinel 12 ...)
        look up in [builtins] hashtable → call it with [args]
    Case 2 — normal closure:
      VClosure { params; body; env = closure_env } →
        check List.length params = List.length args (else failwith arity error)
        let bindings = List.combine params args  (* zip param names with arg values *)
        let call_env = extend bindings closure_env  (* new scope with args bound *)
        eval body call_env  (* evaluate the body in the new scope *)
    Case 3 — not a function → failwith

    OCAML PATTERNS:
    - [when] guards in match patterns for conditional matching.
    - [String.sub s start len] extracts a substring.
    - [String.length s > 12 && String.sub s 0 12 = "__builtin__:"]
      checks the sentinel prefix.
    - [Hashtbl.find_opt tbl key] looks up a key, returns option.
    - [List.combine ["a";"b"] [v1;v2]] → [("a",v1);("b",v2)].

    LEXICAL SCOPING NOTE:
    When calling a closure, we extend the environment that was captured
    at *definition* time (closure_env), NOT the environment at call time.
    This is the essence of lexical scoping / closures. *)
and apply (func : value) (args : value list) : value =
  match func with
  (* Built-in sentinel: params=[], body=Symbol "__builtin__:<name>" *)
  | VClosure { params = []; body = Symbol sentinel; env = _ }
    when String.length sentinel > 12 &&
         String.sub sentinel 0 12 = "__builtin__:" ->
    let name = String.sub sentinel 12 (String.length sentinel - 12) in
    (match Hashtbl.find_opt builtins name with
     | Some f -> f args
     | None   -> failwith ("Unknown built-in: " ^ name))
  | VClosure { params; body; env = closure_env } ->
    (* LEARNING NOTE:
       This is the heart of lexical scoping.  When we call a closure we
       extend the environment that was captured at *definition* time, not
       the environment at *call* time.  That is what makes closures
       "close over" their defining scope. *)
    if List.length params <> List.length args then
      failwith (Printf.sprintf
        "Arity mismatch: expected %d args, got %d"
        (List.length params) (List.length args));
    let bindings = List.combine params args in
    let call_env = extend bindings closure_env in
    eval body call_env
  | _ ->
    failwith (Printf.sprintf "Not a function: %s" (show_value func))

(** [eval expr env] — evaluate [expr] in [env], returning a [value].

    WHAT IT SHOULD DO:
    This is the heart of the interpreter. It dispatches on every possible
    kind of expression and returns the appropriate value.

    LEARNING NOTE:
    The big [match] below is the evaluator's "dispatch table". Each branch
    handles one syntactic form. Branches that start with
    [List (Symbol "keyword" :: …)] are *special forms* — they don't evaluate
    all their sub-expressions normally (e.g. [if] only evaluates one branch).
    Everything else falls through to ordinary function application at the
    bottom.

    KEY CASES TO IMPLEMENT:

    ATOMS (self-evaluating — they return themselves as values):
      Int n    → VInt n
      Float f  → VFloat f
      Bool b   → VBool b
      Str s    → VStr s
      Nil      → VNil

    SYMBOL LOOKUP:
      Symbol name →
        Look up [name] in [env] with [lookup name env].
        If found (Some v) → return v.
        If not found (None) → return VSignal name.
        (WAL treats unknown symbols as hardware signal references.)

    SPECIAL FORMS — these do NOT evaluate all arguments normally:
      (quote e)          → call expr_to_value e  (suppress evaluation)
      (quasiquote ...)   → call expr_to_value on the whole expression
      (define name body) → eval body, return VNil (caller handles env)
      (define (f params) body) → build closure, return VNil
      (lambda (params) body)   → build and return a VClosure
      (let ((x v) ...) body)   → eval all RHS in current env, extend, eval body
      (let* ((x v) ...) body)  → eval each RHS in extended env (sequential)
      (letrec ...)             → mutual recursion with placeholder trick
      (if cond then)           → eval cond; if truthy eval then, else VNil
      (if cond then else)      → eval cond; eval one branch
      (cond ...)               → multi-way if; call eval_cond
      (begin e1 e2 ... en)     → eval all, return last value
      (and e1 e2 ...)          → short-circuit; call eval_and
      (or  e1 e2 ...)          → short-circuit; call eval_or
      (not e)                  → negate: VBool false/VNil → true, else false
      (when cond body...)      → like (if cond (begin body...) ())
      (unless cond body...)    → like (if (not cond) (begin body...) ())

    FUNCTION APPLICATION (the "else" case for any unknown list):
      List (head :: arg_exprs) →
        eval head to get func
        eval each arg_expr to get args
        apply func args

    OCAML PATTERNS:
    - [List (Symbol "define" :: ...)] matches a list starting with "define".
    - [_ :: _] matches any non-empty list.
    - Guards: [| branch when condition ->] for extra conditions.
    - [let _ = ... in ...] to evaluate for side effects and discard result.
    - [List.map], [List.combine], [List.length] are your friends.

    EDGE CASES:
    - (begin) with no expressions → VNil
    - (if cond then) with no else → VNil when cond is false
    - Only #f and () (VNil) are falsy; 0, "", VInt 0 are truthy!
    - (list) — empty list call — handled as List [] → VNil. *)
and eval (expr : expr) (env : env) : value =
  match expr with

  (* ---------------------------------------------------------------- *)
  (*  Literals — self-evaluating: they evaluate to themselves.         *)
  (* ---------------------------------------------------------------- *)

  | Int n    -> failwith "TODO: Int n -> VInt n"
  | Float f  -> failwith "TODO: Float f -> VFloat f"
  | Bool b   -> failwith "TODO: Bool b -> VBool b"
  | Str s    -> failwith "TODO: Str s -> VStr s"
  | Nil      -> failwith "TODO: Nil -> VNil"

  (* ---------------------------------------------------------------- *)
  (*  Symbol lookup                                                    *)
  (* ---------------------------------------------------------------- *)

  (* LEARNING NOTE:
     When we see a bare symbol like [clk] or [my-signal], we first look it
     up in the environment.  If it's not there, we return a [VSignal] — a
     sentinel meaning "this is probably a hardware signal name; look it up
     in the trace at runtime".  This lets WAL scripts refer to signals by
     name without pre-declaring them. *)
  | Symbol name ->
    (* TODO: lookup name env, return the value if found,
       or VSignal name if not found. *)
    ignore name;
    failwith "TODO: Symbol lookup"

  (* ---------------------------------------------------------------- *)
  (*  Special forms                                                    *)
  (* ---------------------------------------------------------------- *)

  (* (quote x) → x unevaluated
     LEARNING NOTE:
     [quote] suppresses evaluation.  '(1 2 3) → a list value, not a call. *)
  | List [Symbol "quote"; e] ->
    (* TODO: call expr_to_value e *)
    ignore e;
    failwith "TODO: quote"

  (* (quasiquote e) — simple passthrough (no unquote expansion here) *)
  | List (Symbol "quasiquote" :: _) ->
    (* TODO: call expr_to_value expr  (the whole expr, not just args) *)
    failwith "TODO: quasiquote"

  (* (define name expr) — bind name in the current scope *)
  | List [Symbol "define"; Symbol name; body] ->
    (* LEARNING NOTE:
       [define] is special: it *modifies* the environment (adds a binding)
       rather than just returning a value.  We return VNil as the "result"
       of a define expression, matching Scheme convention. *)
    (* TODO:
       1. eval body env  (to get the value, though we discard it here;
          the REPL/run loop uses run_program to thread env forward)
       2. return VNil *)
    ignore name; ignore body;
    failwith "TODO: define name"

  (* (define (name params…) body) — function definition shorthand *)
  | List (Symbol "define" :: List (Symbol name :: param_exprs) :: body_exprs) ->
    (* TODO:
       1. Extract param names: List.map (function Symbol s -> s | e -> failwith ...) param_exprs
       2. Build body: if [body_exprs] is a single expr use it; else wrap in (begin ...)
       3. Build a VClosure { params; body; env }
       4. Return VNil (caller handles env extension via run_program) *)
    ignore name; ignore param_exprs; ignore body_exprs;
    failwith "TODO: define function shorthand"

  (* (lambda (params…) body) → closure value
     LEARNING NOTE:
     A [lambda] captures the current environment in a [VClosure] record.
     The body is stored unevaluated; it's evaluated only when the closure
     is called via [apply]. *)
  | List (Symbol "lambda" :: List param_exprs :: body_exprs) ->
    (* TODO:
       1. Extract param names with List.map (function Symbol s -> s | ...) param_exprs
       2. Build body: single expr or wrap in (begin ...)
       3. Return VClosure { params; body; env }   ← capture current env! *)
    ignore param_exprs; ignore body_exprs;
    failwith "TODO: lambda"

  (* (let ((x v) …) body) — parallel let binding
     LEARNING NOTE:
     In a parallel let, all right-hand sides are evaluated in the *current*
     env (before any bindings are added).  This matches Scheme's [let].
     Contrast with [let*] where each binding is visible to the next. *)
  | List (Symbol "let" :: List bindings :: body_exprs) ->
    (* TODO:
       1. Map over bindings: each is List [Symbol name; rhs] → (name, eval rhs env)
       2. extend pairs env → new_env
       3. Evaluate body in new_env *)
    ignore bindings; ignore body_exprs;
    failwith "TODO: let"

  (* (let* ((x v) …) body) — sequential let binding *)
  | List (Symbol "let*" :: List bindings :: body_exprs) ->
    (* LEARNING NOTE:
       [let*] threads the environment through each binding in sequence,
       so (let* ((x 1) (y (+ x 1))) y) → 2 works.

       TODO:
       Use List.fold_left to thread env:
         List.fold_left (fun e -> function
           | List [Symbol name; rhs] -> let v = eval rhs e in extend [(name, v)] e
           | binding -> failwith ...) env bindings
       Then eval the body in the final env. *)
    ignore bindings; ignore body_exprs;
    failwith "TODO: let*"

  (* (letrec ((f (lambda …)) …) body) — recursive bindings
     LEARNING NOTE:
     [letrec] lets bindings refer to each other (e.g. mutual recursion).
     We implement it by first binding all names to VNil, then evaluating
     the right-hand sides in the extended env, then patching — a standard
     trick for immutable environments. *)
  | List (Symbol "letrec" :: List bindings :: body_exprs) ->
    (* TODO:
       1. Extract names from bindings.
       2. Bind all names to VNil in a placeholder_env (extend names→VNil env).
       3. Evaluate each RHS in placeholder_env.
       4. Build final env with (extend pairs env).
       5. Eval body in new_env.
       NOTE: This doesn't support true mutual recursion perfectly (closures
       capture placeholder_env), but works for most practical uses. *)
    ignore bindings; ignore body_exprs;
    failwith "TODO: letrec"

  (* (if cond then else?) — conditional expression
     LEARNING NOTE:
     [if] is special because only ONE branch is evaluated, not both.
     This is called "short-circuit" evaluation.  In WAL (like Scheme),
     only #f is false; everything else (including 0, "", ()) is truthy. *)
  | List [Symbol "if"; cond_expr; then_expr] ->
    (* TODO:
       eval cond_expr env, match result:
         VBool false | VNil → VNil   (no else branch → return nil)
         _ → eval then_expr env *)
    ignore cond_expr; ignore then_expr;
    failwith "TODO: if (no else)"

  | List [Symbol "if"; cond_expr; then_expr; else_expr] ->
    (* TODO:
       eval cond_expr env, match result:
         VBool false | VNil → eval else_expr env
         _ → eval then_expr env *)
    ignore cond_expr; ignore then_expr; ignore else_expr;
    failwith "TODO: if (with else)"

  (* (cond (test expr) … (else expr))
     LEARNING NOTE:
     [cond] is a multi-way conditional.  We test each clause in order and
     evaluate the body of the first truthy one. *)
  | List (Symbol "cond" :: clauses) ->
    (* TODO: call eval_cond clauses env *)
    ignore clauses;
    failwith "TODO: cond"

  (* (begin e1 e2 … en) → value of en
     LEARNING NOTE:
     [begin] evaluates expressions in order for side effects and returns
     the last one.  It's the sequencing construct. *)
  | List (Symbol "begin" :: exprs) ->
    (* TODO:
       match exprs with
         [] → VNil
         _  → eval all expressions, return the value of the last one.
       Use a recursive helper or eval_begin below. *)
    ignore exprs;
    failwith "TODO: begin"

  (* (and e1 e2 …) — short-circuit conjunction
     LEARNING NOTE:
     Returns the last truthy value or #f on first false. *)
  | List (Symbol "and" :: exprs) ->
    (* TODO: call eval_and exprs env (VBool true) *)
    ignore exprs;
    failwith "TODO: and"

  (* (or e1 e2 …) — short-circuit disjunction *)
  | List (Symbol "or" :: exprs) ->
    (* TODO: call eval_or exprs env *)
    ignore exprs;
    failwith "TODO: or"

  (* (not e) *)
  | List [Symbol "not"; e] ->
    (* TODO:
       eval e env, match:
         VBool false | VNil → VBool true
         _ → VBool false *)
    ignore e;
    failwith "TODO: not"

  (* (when cond body…) — execute body only if cond is true *)
  | List (Symbol "when" :: cond_expr :: body_exprs) ->
    (* TODO:
       eval cond_expr env, match:
         VBool false | VNil → VNil
         _ → eval body (wrap in begin if multiple exprs) in env *)
    ignore cond_expr; ignore body_exprs;
    failwith "TODO: when"

  (* (unless cond body…) — execute body only if cond is false *)
  | List (Symbol "unless" :: cond_expr :: body_exprs) ->
    (* TODO:
       eval cond_expr env, match:
         VBool false | VNil → eval body in env
         _ → VNil *)
    ignore cond_expr; ignore body_exprs;
    failwith "TODO: unless"

  (* ---------------------------------------------------------------- *)
  (*  Function application                                             *)
  (* ---------------------------------------------------------------- *)

  (* LEARNING NOTE:
     The "else" branch: any list not matching a special form is treated
     as a function call.  We evaluate the head to get a function, evaluate
     all arguments, then call [apply]. *)
  | List [] -> VNil

  | List (head :: arg_exprs) ->
    (* TODO:
       1. let func = eval head env
       2. let args = List.map (fun e -> eval e env) arg_exprs
       3. apply func args *)
    ignore head; ignore arg_exprs;
    failwith "TODO: function application"

(** [eval_cond clauses env] — worker for the [cond] special form.

    WHAT IT SHOULD DO:
    Walk through cond clauses in order. Each clause is a list:
      - (else body...) → always execute body
      - (test body...) → if test is truthy, execute body
      - (test)         → if test is truthy, return the test value itself

    ALGORITHM:
    Recursive match on clauses:
      [] → VNil  (no clause matched)
      List (Symbol "else" :: body) :: _ → eval_begin body env
      List (test :: body) :: rest →
        match eval test env with
          VBool false | VNil → eval_cond rest env  (try next clause)
          v → (match body with [] → v | _ → eval_begin body env)
      bad_clause → failwith

    OCAML PATTERNS:
    - Nested pattern matching: match on list structure, then on eval result.
    - The (cond (test)) shorthand (no body) returns the test value itself. *)
and eval_cond clauses env =
  ignore clauses; ignore env;
  failwith "TODO: eval_cond"

(** [eval_begin exprs env] — evaluate a sequence, returning the last value.

    WHAT IT SHOULD DO:
    Evaluate each expression in order, returning only the final result.
    Side effects of all expressions happen; only the last value is kept.

    ALGORITHM:
    Recursive match:
      [] → VNil
      [last] → eval last env
      e :: rest → let _ = eval e env in eval_begin rest env

    OCAML PATTERNS:
    - [let _ = expr] evaluates expr for side effects and discards the result.
    - Tail recursion: eval_begin rest env is in tail position here. *)
and eval_begin exprs env =
  ignore exprs; ignore env;
  failwith "TODO: eval_begin"

(** [eval_and exprs env last] — short-circuit AND worker.

    WHAT IT SHOULD DO:
    Evaluate expressions left to right. Return the LAST value if all are truthy.
    Return VBool false on the first falsy value (#f or nil).
    [last] carries the most recently seen truthy value (starts as VBool true).

    ALGORITHM:
    Recursive match on exprs:
      [] → last  (all were truthy; return the last one)
      e :: rest →
        match eval e env with
          VBool false → VBool false  (short-circuit)
          VNil        → VBool false  (nil is falsy in AND)
          v           → eval_and rest env v  (truthy; keep going)

    EDGE CASE:
    (and) with no arguments returns #t (truthy identity). That's why
    [last] starts as [VBool true] at the call site. *)
and eval_and exprs env last =
  ignore exprs; ignore env; ignore last;
  failwith "TODO: eval_and"

(** [eval_or exprs env] — short-circuit OR worker.

    WHAT IT SHOULD DO:
    Evaluate expressions left to right. Return the first truthy value.
    Return VBool false if all are falsy.

    ALGORITHM:
    Recursive match on exprs:
      [] → VBool false  (no truthy value found)
      e :: rest →
        match eval e env with
          VBool false | VNil → eval_or rest env  (falsy; try next)
          v                  → v  (found a truthy value; return it immediately) *)
and eval_or exprs env =
  ignore exprs; ignore env;
  failwith "TODO: eval_or"

(** [expr_to_value e] — convert an expression to a value without evaluating.
    Used by [quote].

    WHAT IT SHOULD DO:
    Convert every [expr] variant into the corresponding [value] variant,
    without evaluating anything. This is how (quote (1 2 3)) returns a
    list value rather than trying to call 1 as a function.

    ALGORITHM:
    Simple recursive structural mapping:
      Int n      → VInt n
      Float f    → VFloat f
      Bool b     → VBool b
      Str s      → VStr s
      Symbol s   → VSymbol s   ← NOTE: Symbol becomes VSymbol, not VSignal!
      Nil        → VNil
      List exprs → VList (List.map expr_to_value exprs)

    LEARNING NOTE:
    [quote] returns its argument as data, not code.  We recursively convert
    the expr tree into the corresponding value tree.

    KEY DISTINCTION:
    In the evaluator, [Symbol s] → look up in env or become VSignal.
    But in [expr_to_value], [Symbol s] → VSymbol s (it's data, not a variable). *)
and expr_to_value : expr -> value = function
  | Int n      -> failwith "TODO"
  | Float f    -> failwith "TODO"
  | Bool b     -> failwith "TODO"
  | Str s      -> failwith "TODO"
  | Symbol s   -> failwith "TODO"
  | Nil        -> failwith "TODO"
  | List exprs -> failwith "TODO"

(* ------------------------------------------------------------------ *)
(*  Built-in functions                                                  *)
(* ------------------------------------------------------------------ *)

(* LEARNING NOTE:
   Built-in functions are OCaml closures stored in the initial environment.
    They accept [value list] and return [value].  We wrap them in [VClosure]
    — actually, we use a simpler representation: a plain OCaml function.

    To integrate with the evaluator we need a new value variant.  But we
    don't have one!  Instead, we use a small trick: we define built-ins as
    [VClosure] values whose [body] is a special sentinel.  That's fragile.

    Better: we add a [VBuiltin] variant… but since we can't modify wal_ast.ml
    here, we encode built-ins as closures over empty params that the [apply]
    function recognises through a table lookup.

    ACTUAL approach: we use a functional environment where built-in names
    are bound to [VClosure] values with a dummy body of [Symbol "__builtin__"]
    plus a parallel hashtable from name → OCaml function.  When [apply] sees
    [Symbol "__builtin__:<name>"] it dispatches to the hashtable.

    Actually the cleanest approach without changing wal_ast: represent
    built-ins as [VClosure]s with [params=[]] and [body = Symbol "<builtin:name>"]
    and intercept them in [apply].

    We implement this below. *)

let register name f =
  Hashtbl.replace builtins name f

(* Arithmetic helpers *)
let arith_op op_i op_f name args =
  match args with
  | [] -> failwith (name ^ ": requires arguments")
  | _ ->
    let has_float = List.exists (function VFloat _ -> true | _ -> false) args in
    if has_float then
      let fs = List.map (function
        | VFloat f -> f
        | VInt n   -> float_of_int n
        | v -> failwith (name ^ ": not a number: " ^ show_value v)) args in
      VFloat (List.fold_left op_f (List.hd fs) (List.tl fs))
    else
      let ns = List.map (function
        | VInt n -> n
        | v -> failwith (name ^ ": not a number: " ^ show_value v)) args in
      VInt (List.fold_left op_i (List.hd ns) (List.tl ns))

let () =
  (* Arithmetic *)
  register "+" (fun args ->
    match args with
    | [] -> VInt 0
    | _  -> arith_op ( + ) ( +. ) "+" args);

  register "-" (fun args ->
    match args with
    | [] -> failwith "-: requires at least one argument"
    | [VInt n]   -> VInt (-n)
    | [VFloat f] -> VFloat (-.f)
    | _ -> arith_op ( - ) ( -. ) "-" args);

  register "*" (fun args ->
    match args with
    | [] -> VInt 1
    | _  -> arith_op ( * ) ( *. ) "*" args);

  register "/" (fun args ->
    match args with
    | [] -> failwith "/: requires at least one argument"
    | [VInt n]   -> VInt (1 / n)
    | [VFloat f] -> VFloat (1.0 /. f)
    | _ ->
      let has_float = List.exists (function VFloat _ -> true | _ -> false) args in
      if has_float then
        let fs = List.map (function
          | VFloat f -> f
          | VInt n   -> float_of_int n
          | v -> failwith ("/: not a number: " ^ show_value v)) args in
        VFloat (List.fold_left ( /. ) (List.hd fs) (List.tl fs))
      else
        let ns = List.map (function
          | VInt n -> n
          | v -> failwith ("/: not a number: " ^ show_value v)) args in
        VInt (List.fold_left ( / ) (List.hd ns) (List.tl ns)));

  register "mod" (fun args ->
    match args with
    | [VInt a; VInt b] -> VInt (a mod b)
    | [a; b] -> failwith (Printf.sprintf "mod: expected integers, got %s %s"
                  (show_value a) (show_value b))
    | _ -> failwith "mod: requires exactly 2 arguments");

  (* Comparison — works for int, float, string using compare *)
  let cmp_op (op : int -> int -> bool) name args =
    match args with
    | [VInt a;   VInt b]   -> VBool (op (compare a b) 0)
    | [VFloat a; VFloat b] -> VBool (op (compare a b) 0)
    | [VInt a;   VFloat b] -> VBool (op (compare (float_of_int a) b) 0)
    | [VFloat a; VInt b]   -> VBool (op (compare a (float_of_int b)) 0)
    | [VStr a;   VStr b]   -> VBool (op (String.compare a b) 0)
    | _ -> failwith (name ^ ": type error") in

  (* LEARNING NOTE:
     We partially apply [cmp_op] to produce comparison built-ins.
     OCaml's polymorphic comparison [( = )] works on most types including
     int, float, string.  For float we use (=) which does structural equality. *)
  register "="  (cmp_op ( = ) "=");
  register "<"  (cmp_op ( < ) "<");
  register ">"  (cmp_op ( > ) ">");
  register "<=" (cmp_op ( <= ) "<=");
  register ">=" (cmp_op ( >= ) ">=");

  (* I/O *)
  register "print" (fun args ->
    List.iter (fun v -> print_string (show_value v)) args;
    VNil);

  register "println" (fun args ->
    List.iter (fun v -> print_string (show_value v)) args;
    print_newline ();
    VNil);

  register "display" (fun args ->
    List.iter (fun v ->
      match v with
      | VStr s -> print_string s
      | _      -> print_string (show_value v)) args;
    VNil);

  register "newline" (fun _ -> print_newline (); VNil);

  (* List operations *)
  register "list" (fun args -> VList args);

  register "car" (fun args ->
    match args with
    | [VList (h :: _)] -> h
    | [VList []]       -> failwith "car: empty list"
    | [v]              -> failwith ("car: not a list: " ^ show_value v)
    | _                -> failwith "car: requires exactly one argument");

  register "cdr" (fun args ->
    match args with
    | [VList (_ :: t)] -> VList t
    | [VList []]       -> failwith "cdr: empty list"
    | [v]              -> failwith ("cdr: not a list: " ^ show_value v)
    | _                -> failwith "cdr: requires exactly one argument");

  register "cons" (fun args ->
    match args with
    | [h; VList t] -> VList (h :: t)
    | [h; VNil]    -> VList [h]
    | [_; v]       -> failwith ("cons: second arg must be list, got " ^ show_value v)
    | _            -> failwith "cons: requires exactly 2 arguments");

  register "append" (fun args ->
    let lists = List.map (function
      | VList l -> l
      | VNil    -> []
      | v -> failwith ("append: not a list: " ^ show_value v)) args in
    VList (List.concat lists));

  register "length" (fun args ->
    match args with
    | [VList l] -> VInt (List.length l)
    | [VNil]    -> VInt 0
    | [v]       -> failwith ("length: not a list: " ^ show_value v)
    | _         -> failwith "length: requires exactly one argument");

  register "null?" (fun args ->
    match args with
    | [VNil]    -> VBool true
    | [VList []]-> VBool true
    | [_]       -> VBool false
    | _         -> failwith "null?: requires exactly one argument");

  register "pair?" (fun args ->
    match args with
    | [VList (_ :: _)] -> VBool true
    | _                -> VBool false);

  register "list?" (fun args ->
    match args with
    | [VList _] | [VNil] -> VBool true
    | _                  -> VBool false);

  register "number?" (fun args ->
    match args with
    | [VInt _ | VFloat _] -> VBool true
    | [_]                 -> VBool false
    | _                   -> failwith "number?: requires 1 argument");

  register "string?" (fun args ->
    match args with
    | [VStr _] -> VBool true
    | [_]      -> VBool false
    | _        -> failwith "string?: requires 1 argument");

  register "boolean?" (fun args ->
    match args with
    | [VBool _] -> VBool true
    | [_]       -> VBool false
    | _         -> failwith "boolean?: requires 1 argument");

  register "symbol?" (fun args ->
    match args with
    | [VSymbol _] -> VBool true
    | [_]         -> VBool false
    | _           -> failwith "symbol?: requires 1 argument");

  register "procedure?" (fun args ->
    match args with
    | [VClosure _] -> VBool true
    | [_]          -> VBool false
    | _            -> failwith "procedure?: requires 1 argument");

  (* Higher-order list functions *)
  register "map" (fun args ->
    (* LEARNING NOTE:
       [map] takes a function and a list.  We call [apply] on each element.
       Note that [apply] lives in this same [let rec … and …] block, so this
       closure (created *after* [apply] is defined) can safely call it. *)
    match args with
    | [f; VList lst] ->
      VList (List.map (fun v -> apply f [v]) lst)
    | [_f; VNil] -> VNil
    | _ -> failwith "map: requires function and list");

  register "filter" (fun args ->
    match args with
    | [f; VList lst] ->
      VList (List.filter (fun v ->
        match apply f [v] with
        | VBool false | VNil -> false
        | _                  -> true) lst)
    | [_f; VNil] -> VNil
    | _ -> failwith "filter: requires function and list");

  register "fold-left" (fun args ->
    match args with
    | [f; init; VList lst] ->
      List.fold_left (fun acc v -> apply f [acc; v]) init lst
    | [_f; init; VNil] -> init
    | _ -> failwith "fold-left: requires function, initial value, and list");

  register "fold-right" (fun args ->
    match args with
    | [f; init; VList lst] ->
      List.fold_right (fun v acc -> apply f [v; acc]) lst init
    | [_f; init; VNil] -> init
    | _ -> failwith "fold-right: requires function, initial value, and list");

  register "for-each" (fun args ->
    match args with
    | [f; VList lst] ->
      List.iter (fun v -> let _ = apply f [v] in ()) lst; VNil
    | [_f; VNil] -> VNil
    | _ -> failwith "for-each: requires function and list");

  register "reverse" (fun args ->
    match args with
    | [VList l] -> VList (List.rev l)
    | [VNil]    -> VNil
    | _         -> failwith "reverse: requires one list argument");

  register "assoc" (fun args ->
    match args with
    | [key; VList pairs] ->
      (try
        let pair = List.find (function
          | VList (k :: _) -> k = key
          | _ -> false) pairs in
        pair
       with Not_found -> VBool false)
    | _ -> failwith "assoc: requires key and association list");

  (* String operations *)
  register "string-append" (fun args ->
    let strs = List.map (function
      | VStr s -> s
      | v -> failwith ("string-append: not a string: " ^ show_value v)) args in
    VStr (String.concat "" strs));

  register "string-length" (fun args ->
    match args with
    | [VStr s] -> VInt (String.length s)
    | _        -> failwith "string-length: requires one string");

  register "substring" (fun args ->
    match args with
    | [VStr s; VInt start; VInt len] ->
      VStr (String.sub s start len)
    | _ -> failwith "substring: requires string, start, length");

  register "number->string" (fun args ->
    match args with
    | [VInt n]   -> VStr (string_of_int n)
    | [VFloat f] -> VStr (string_of_float f)
    | _          -> failwith "number->string: requires a number");

  register "string->number" (fun args ->
    match args with
    | [VStr s] ->
      (match int_of_string_opt s with
       | Some n -> VInt n
       | None ->
         match float_of_string_opt s with
         | Some f -> VFloat f
         | None   -> VBool false)
    | _ -> failwith "string->number: requires a string");

  register "string->symbol" (fun args ->
    match args with
    | [VStr s] -> VSymbol s
    | _        -> failwith "string->symbol: requires a string");

  register "symbol->string" (fun args ->
    match args with
    | [VSymbol s] -> VStr s
    | _           -> failwith "symbol->string: requires a symbol");

  (* Math *)
  register "abs" (fun args ->
    match args with
    | [VInt n]   -> VInt (abs n)
    | [VFloat f] -> VFloat (Float.abs f)
    | _          -> failwith "abs: requires a number");

  register "max" (fun args ->
    match args with
    | [] -> failwith "max: requires arguments"
    | first :: rest ->
      List.fold_left (fun acc v ->
        match acc, v with
        | VInt a,   VInt b   -> if b > a then VInt b   else VInt a
        | VFloat a, VFloat b -> if b > a then VFloat b else VFloat a
        | VInt a,   VFloat b -> let fa = float_of_int a in if b > fa then VFloat b else VFloat fa
        | VFloat a, VInt b   -> let fb = float_of_int b in if fb > a then VFloat fb else VFloat a
        | _ -> failwith "max: type error") first rest);

  register "min" (fun args ->
    match args with
    | [] -> failwith "min: requires arguments"
    | first :: rest ->
      List.fold_left (fun acc v ->
        match acc, v with
        | VInt a,   VInt b   -> if b < a then VInt b   else VInt a
        | VFloat a, VFloat b -> if b < a then VFloat b else VFloat a
        | VInt a,   VFloat b -> let fa = float_of_int a in if b < fa then VFloat b else VFloat fa
        | VFloat a, VInt b   -> let fb = float_of_int b in if fb < a then VFloat fb else VFloat a
        | _ -> failwith "min: type error") first rest);

  register "floor" (fun args ->
    match args with
    | [VFloat f] -> VFloat (Float.round (f -. 0.5))  (* floor *)
    | [VInt n]   -> VInt n
    | _ -> failwith "floor: requires a number");

  register "sqrt" (fun args ->
    match args with
    | [VFloat f] -> VFloat (sqrt f)
    | [VInt n]   -> VFloat (sqrt (float_of_int n))
    | _ -> failwith "sqrt: requires a number");

  register "expt" (fun args ->
    match args with
    | [VInt b; VInt e]   -> VFloat (Float.pow (float_of_int b) (float_of_int e))
    | [VFloat b; VFloat e] -> VFloat (Float.pow b e)
    | [VInt b; VFloat e] -> VFloat (Float.pow (float_of_int b) e)
    | [VFloat b; VInt e] -> VFloat (Float.pow b (float_of_int e))
    | _ -> failwith "expt: requires two numbers");

  (* Logical (value-level, not short-circuit special forms) *)
  register "equal?" (fun args ->
    match args with
    | [a; b] -> VBool (a = b)
    | _      -> failwith "equal?: requires 2 arguments");

  (* Error *)
  register "error" (fun args ->
    match args with
    | VStr msg :: rest ->
      let details = String.concat " " (List.map show_value rest) in
      failwith (if details = "" then msg else msg ^ " " ^ details)
    | _ -> failwith "error: requires a message string");

  register "exit" (fun args ->
    let code = match args with
      | [VInt n] -> n
      | []       -> 0
      | _        -> 1 in
    exit code)

(** [builtin_sentinel name] — create a dummy [VClosure] that signals
    to [apply] to dispatch to the builtins table.

    LEARNING NOTE:
    Since [value] has no [VBuiltin] variant, we encode built-ins as closures
    whose [body] is [Symbol "__builtin__:<name>"].  The [apply] function
    checks for this pattern before doing normal closure application. *)
let builtin_sentinel name =
  VClosure { params = []; body = Symbol ("__builtin__:" ^ name); env = empty }

(* ------------------------------------------------------------------ *)
(*  Patch apply to handle built-in sentinels                           *)
(* ------------------------------------------------------------------ *)

(* We need to redefine apply to handle built-in sentinels.
   OCaml's [let rec … and …] block must be written as one connected block.
   Below is the re-entrant version that supersedes the forward declaration. *)

(* ------------------------------------------------------------------ *)
(*  Initial environment                                                 *)
(* ------------------------------------------------------------------ *)

(** [initial_env ()] — build the environment that every program starts with.

    WHAT IT SHOULD DO:
    Build an environment containing all the built-in function bindings.
    Each built-in name is mapped to a "sentinel" closure that [apply]
    will intercept and dispatch to the actual OCaml function.

    ALGORITHM:
    1. Use [Hashtbl.fold] to iterate over the [builtins] table and collect
       (name, builtin_sentinel name) pairs.
    2. Call [extend bindings empty] to create the initial env.

    LEARNING NOTE:
    The initial environment contains all the built-in functions.
    We bind each name to a sentinel closure; [apply] intercepts those
    and dispatches to the OCaml function in [builtins].

    OCAML PATTERNS:
    - [Hashtbl.fold f tbl acc] folds over all key-value pairs in a hashtable.
      f gets called as [f key value accumulator].
    - [(name, builtin_sentinel name) :: acc] builds up the bindings list. *)
let initial_env () : env =
  let bindings = Hashtbl.fold (fun name _f acc ->
    (name, builtin_sentinel name) :: acc
  ) builtins [] in
  extend bindings empty

(* ------------------------------------------------------------------ *)
(*  Re-entrant apply (replaces the forward-declared one above)         *)
(* ------------------------------------------------------------------ *)

(** This module-level let shadows the [apply] defined in the [let rec] block.
    It adds built-in dispatch and is the version called by user code. *)

(* We cannot shadow mutual-rec functions after the fact in OCaml without
   a ref trick or a module.  Instead we patch apply via a ref. *)

let apply_ref : (value -> value list -> value) ref =
  ref (fun _f _args -> failwith "evaluator not initialised")

let () =
  apply_ref := (fun func args ->
    match func with
    | VClosure { params = []; body = Symbol sentinel; env = _ }
      when String.length sentinel > 12 &&
           String.sub sentinel 0 12 = "__builtin__:" ->
      let name = String.sub sentinel 12 (String.length sentinel - 12) in
      (match Hashtbl.find_opt builtins name with
       | Some f -> f args
       | None   -> failwith ("Unknown built-in: " ^ name))
    | VClosure { params; body; env = closure_env } ->
      if List.length params <> List.length args then
        failwith (Printf.sprintf
          "Arity mismatch: expected %d args, got %d"
          (List.length params) (List.length args));
      let bindings = List.combine params args in
      let call_env = extend bindings closure_env in
      eval body call_env
    | _ ->
      failwith (Printf.sprintf "Not a function: %s" (show_value func)))

(* ------------------------------------------------------------------ *)
(*  Public eval that routes apply through the ref                      *)
(* ------------------------------------------------------------------ *)

(** [run expr env] — the public entry-point evaluator.

    WHAT IT SHOULD DO:
    This is a re-implementation of [eval] that uses [!apply_ref] for
    function calls instead of the internal [apply]. This ensures built-in
    dispatch works correctly even after the [let rec] block closes.

    LEARNING NOTE:
    This re-enters the evaluator with the patched apply dispatch.
    For production code you'd restructure with proper modules; this
    ref trick is a learning simplification.

    STRUCTURE:
    This function mirrors [eval] above in structure — same cases,
    same logic — but calls [!apply_ref func args] instead of [apply func args]
    in the function application case, and calls [run_begin], [run_cond],
    [run_and], [run_or] instead of [eval_begin] etc.

    Think of [run] as the "production" version and [eval] as the "teaching" version.
    You do NOT need to re-implement all the logic — study [eval] above,
    then note that [run] is structurally identical. *)
let rec run (expr : expr) (env : env) : value =
  match expr with
  | Int n    -> VInt n
  | Float f  -> VFloat f
  | Bool b   -> VBool b
  | Str s    -> VStr s
  | Nil      -> VNil

  | Symbol name ->
    (match lookup name env with
     | Some v -> v
     | None   -> VSignal name)

  | List [Symbol "quote"; e] -> expr_to_value e
  | List (Symbol "quasiquote" :: _) -> expr_to_value expr

  | List [Symbol "define"; Symbol name; body] ->
    let v = run body env in
    let _ = v in
    let _ = name in
    VNil  (* top-level define: caller uses run_program *)

  | List (Symbol "define" :: List (Symbol _name :: _param_exprs) :: _body_exprs) ->
    VNil  (* handled by run_program *)

  | List (Symbol "lambda" :: List param_exprs :: body_exprs) ->
    let params = List.map (function
      | Symbol s -> s
      | e -> failwith ("lambda: bad param " ^ show_expr e)) param_exprs in
    let body = match body_exprs with
      | [b] -> b
      | _   -> List (Symbol "begin" :: body_exprs) in
    VClosure { params; body; env }

  | List (Symbol "let" :: List bindings :: body_exprs) ->
    let pairs = List.map (function
      | List [Symbol n; rhs] -> (n, run rhs env)
      | e -> failwith ("let: bad binding " ^ show_expr e)) bindings in
    let new_env = extend pairs env in
    run_begin body_exprs new_env

  | List (Symbol "let*" :: List bindings :: body_exprs) ->
    let new_env = List.fold_left (fun e -> function
      | List [Symbol n; rhs] -> extend [(n, run rhs e)] e
      | b -> failwith ("let*: bad binding " ^ show_expr b)) env bindings in
    run_begin body_exprs new_env

  | List (Symbol "letrec" :: List bindings :: body_exprs) ->
    let names = List.map (function
      | List [Symbol n; _] -> n
      | e -> failwith ("letrec: bad binding " ^ show_expr e)) bindings in
    let ph_env = extend (List.map (fun n -> (n, VNil)) names) env in
    let vals = List.map (function
      | List [Symbol _; rhs] -> run rhs ph_env
      | e -> failwith ("letrec: bad binding " ^ show_expr e)) bindings in
    let new_env = extend (List.combine names vals) env in
    run_begin body_exprs new_env

  | List [Symbol "if"; c; t] ->
    (match run c env with
     | VBool false | VNil -> VNil
     | _                  -> run t env)

  | List [Symbol "if"; c; t; e] ->
    (match run c env with
     | VBool false | VNil -> run e env
     | _                  -> run t env)

  | List (Symbol "cond" :: clauses) ->
    run_cond clauses env

  | List (Symbol "begin" :: exprs) ->
    run_begin exprs env

  | List (Symbol "and" :: exprs) ->
    run_and exprs env (VBool true)

  | List (Symbol "or" :: exprs) ->
    run_or exprs env

  | List [Symbol "not"; e] ->
    (match run e env with
     | VBool false | VNil -> VBool true
     | _                  -> VBool false)

  | List (Symbol "when" :: c :: body) ->
    (match run c env with
     | VBool false | VNil -> VNil
     | _ -> run_begin body env)

  | List (Symbol "unless" :: c :: body) ->
    (match run c env with
     | VBool false | VNil -> run_begin body env
     | _ -> VNil)

  | List [] -> VNil

  | List (head :: arg_exprs) ->
    let func = run head env in
    let args = List.map (fun e -> run e env) arg_exprs in
    !apply_ref func args

and run_begin exprs env =
  match exprs with
  | []     -> VNil
  | [last] -> run last env
  | e :: rest -> let _ = run e env in run_begin rest env

and run_cond clauses env =
  match clauses with
  | [] -> VNil
  | List (Symbol "else" :: body) :: _ -> run_begin body env
  | List (test :: body) :: rest ->
    (match run test env with
     | VBool false | VNil -> run_cond rest env
     | v -> match body with [] -> v | _ -> run_begin body env)
  | c :: _ -> failwith ("cond: bad clause " ^ show_expr c)

and run_and exprs env last =
  match exprs with
  | [] -> last
  | e :: rest ->
    (match run e env with
     | VBool false as f -> f
     | VNil             -> VBool false
     | v                -> run_and rest env v)

and run_or exprs env =
  match exprs with
  | [] -> VBool false
  | e :: rest ->
    (match run e env with
     | VBool false | VNil -> run_or rest env
     | v                  -> v)

(** [run_program exprs env] — evaluate a list of top-level expressions,
    threading the environment forward through [define] forms.

    WHAT IT SHOULD DO:
    Evaluate every expression in [exprs] in sequence. For [define] forms,
    update the environment so later expressions see the new binding.
    Return the value of the last expression and the final environment.

    ALGORITHM:
    Use [List.fold_left] to accumulate (last_value, env) across all exprs:
      - For (define name body): eval body, bind name in env → (v, new_env)
      - For (define (f params) body): build closure, self-referential env trick
        1. Create a placeholder closure with the current env.
        2. Define name → placeholder in new_env.
        3. Rebuild the closure capturing new_env (so recursive calls work).
        4. Redefine name → real closure in final_env.
      - For anything else: eval in current env → (v, env unchanged)

    LEARNING NOTE:
    This is the main loop for loading a .wal file.  The fold accumulates an
    updated env so that definitions earlier in the file are visible to later
    expressions.

    OCAML PATTERNS:
    - [List.fold_left f init lst]: fold from left, accumulating state.
    - f receives (accumulator, current_element) and returns new accumulator.
    - Our accumulator is [(last_value, env)] — a tuple.
    - [let (_, e) = acc in ...] destructures the accumulator. *)
let run_program (exprs : expr list) (env : env) : value * env =
  List.fold_left (fun (_, e) expr ->
    match expr with
    | List [Symbol "define"; Symbol name; body] ->
      let v = run body e in
      let new_env = define name v e in
      (v, new_env)
    | List (Symbol "define" :: List (Symbol name :: param_exprs) :: body_exprs) ->
      let params = List.map (function
        | Symbol s -> s
        | p -> failwith ("define: bad param " ^ show_expr p)) param_exprs in
      let body = match body_exprs with
        | [b] -> b
        | _   -> List (Symbol "begin" :: body_exprs) in
      (* Create closure, then extend env so recursive calls see themselves.
         We define in new_env first with a placeholder, then rebuild. *)
      let placeholder = VClosure { params; body; env = e } in
      let new_env = define name placeholder e in
      (* Now rebuild the closure capturing new_env so self-reference works. *)
      let v = VClosure { params; body; env = new_env } in
      let final_env = define name v new_env in
      (v, final_env)
    | _ ->
      let v = run expr e in
      (v, e)
  ) (VNil, env) exprs

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
    but we make it explicit anyway for clarity. *)
(* eval_list, apply, and eval are mutually recursive.
   LEARNING NOTE: [let rec f x = … and g y = …] is OCaml's syntax for
   mutual recursion. Both f and g are in scope inside each other's body. *)
let rec eval_list exprs env =
  List.map (fun e -> eval e env) exprs

and apply (func : value) (args : value list) : value =
  match func with
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

    LEARNING NOTE:
    The big [match] below is the evaluator's "dispatch table".  Each branch
    handles one syntactic form.  Branches that start with
    [List (Symbol "keyword" :: …)] are *special forms* — they don't evaluate
    all their sub-expressions normally (e.g. [if] only evaluates one branch).
    Everything else falls through to ordinary function application at the
    bottom. *)
and eval (expr : expr) (env : env) : value =
  match expr with

  (* ---------------------------------------------------------------- *)
  (*  Literals — self-evaluating: they evaluate to themselves.         *)
  (* ---------------------------------------------------------------- *)

  | Int n    -> VInt n
  | Float f  -> VFloat f
  | Bool b   -> VBool b
  | Str s    -> VStr s
  | Nil      -> VNil

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
    (match lookup name env with
     | Some v -> v
     | None   -> VSignal name)

  (* ---------------------------------------------------------------- *)
  (*  Special forms                                                    *)
  (* ---------------------------------------------------------------- *)

  (* (quote x) → x unevaluated
     LEARNING NOTE:
     [quote] suppresses evaluation.  '(1 2 3) → a list value, not a call. *)
  | List [Symbol "quote"; e] ->
    expr_to_value e

  (* (quasiquote e) — simple passthrough (no unquote expansion here) *)
  | List (Symbol "quasiquote" :: _) ->
    expr_to_value expr

  (* (define name expr) — bind name in the current scope *)
  | List [Symbol "define"; Symbol name; body] ->
    (* LEARNING NOTE:
       [define] is special: it *modifies* the environment (adds a binding)
       rather than just returning a value.  We return VNil as the "result"
       of a define expression, matching Scheme convention. *)
    let _ = eval body env in
    (* We can't mutate env (it's immutable), so we return VNil.
       The REPL / run loop must use [define name val env] to thread the
       updated env forward.  Here we just evaluate the body for side effects
       and return VNil — the caller (run_program) handles env threading. *)
    VNil

  (* (define (name params…) body) — function definition shorthand *)
  | List (Symbol "define" :: List (Symbol name :: param_exprs) :: body_exprs) ->
    let params = List.map (function
      | Symbol s -> s
      | e -> failwith ("define: bad param " ^ show_expr e)) param_exprs in
    let body = match body_exprs with
      | [b] -> b
      | _   -> List (Symbol "begin" :: body_exprs) in
    let _ = VClosure { params; body; env } in
    let _ = name in VNil  (* same: caller threads env *)

  (* (lambda (params…) body) → closure value
     LEARNING NOTE:
     A [lambda] captures the current environment in a [VClosure] record.
     The body is stored unevaluated; it's evaluated only when the closure
     is called via [apply]. *)
  | List (Symbol "lambda" :: List param_exprs :: body_exprs) ->
    let params = List.map (function
      | Symbol s -> s
      | e -> failwith ("lambda: bad parameter " ^ show_expr e)) param_exprs in
    let body = match body_exprs with
      | [b] -> b
      | _   -> List (Symbol "begin" :: body_exprs) in
    VClosure { params; body; env }

  (* (let ((x v) …) body) — parallel let binding
     LEARNING NOTE:
     In a parallel let, all right-hand sides are evaluated in the *current*
     env (before any bindings are added).  This matches Scheme's [let].
     Contrast with [let*] where each binding is visible to the next. *)
  | List (Symbol "let" :: List bindings :: body_exprs) ->
    let pairs = List.map (function
      | List [Symbol name; rhs] -> (name, eval rhs env)
      | e -> failwith ("let: bad binding " ^ show_expr e)) bindings in
    let new_env = extend pairs env in
    let body = match body_exprs with
      | [b] -> b
      | _   -> List (Symbol "begin" :: body_exprs) in
    eval body new_env

  (* (let* ((x v) …) body) — sequential let binding *)
  | List (Symbol "let*" :: List bindings :: body_exprs) ->
    (* LEARNING NOTE:
       [let*] threads the environment through each binding in sequence,
       so (let* ((x 1) (y (+ x 1))) y) → 2 works. *)
    let new_env = List.fold_left (fun e -> function
      | List [Symbol name; rhs] ->
        let v = eval rhs e in
        extend [(name, v)] e
      | binding ->
        failwith ("let*: bad binding " ^ show_expr binding)
    ) env bindings in
    let body = match body_exprs with
      | [b] -> b
      | _   -> List (Symbol "begin" :: body_exprs) in
    eval body new_env

  (* (letrec ((f (lambda …)) …) body) — recursive bindings
     LEARNING NOTE:
     [letrec] lets bindings refer to each other (e.g. mutual recursion).
     We implement it by first binding all names to VNil, then evaluating
     the right-hand sides in the extended env, then patching — a standard
     trick for immutable environments. *)
  | List (Symbol "letrec" :: List bindings :: body_exprs) ->
    let names = List.map (function
      | List [Symbol name; _] -> name
      | e -> failwith ("letrec: bad binding " ^ show_expr e)) bindings in
    (* Bind all names to VNil as placeholders. *)
    let placeholder_env = extend (List.map (fun n -> (n, VNil)) names) env in
    (* Now evaluate each RHS in the placeholder env. *)
    let vals = List.map (function
      | List [Symbol _name; rhs] -> eval rhs placeholder_env
      | e -> failwith ("letrec: bad binding " ^ show_expr e)) bindings in
    let pairs = List.combine names vals in
    let new_env = extend pairs env in
    let body = match body_exprs with
      | [b] -> b
      | _   -> List (Symbol "begin" :: body_exprs) in
    eval body new_env

  (* (if cond then else?) — conditional expression
     LEARNING NOTE:
     [if] is special because only ONE branch is evaluated, not both.
     This is called "short-circuit" evaluation.  In WAL (like Scheme),
     only #f is false; everything else (including 0, "", ()) is truthy. *)
  | List [Symbol "if"; cond_expr; then_expr] ->
    (match eval cond_expr env with
     | VBool false | VNil -> VNil
     | _                  -> eval then_expr env)

  | List [Symbol "if"; cond_expr; then_expr; else_expr] ->
    (match eval cond_expr env with
     | VBool false | VNil -> eval else_expr env
     | _                  -> eval then_expr env)

  (* (cond (test expr) … (else expr))
     LEARNING NOTE:
     [cond] is a multi-way conditional.  We test each clause in order and
     evaluate the body of the first truthy one. *)
  | List (Symbol "cond" :: clauses) ->
    eval_cond clauses env

  (* (begin e1 e2 … en) → value of en
     LEARNING NOTE:
     [begin] evaluates expressions in order for side effects and returns
     the last one.  It's the sequencing construct. *)
  | List (Symbol "begin" :: exprs) ->
    (match exprs with
     | [] -> VNil
     | _  ->
       let rec go = function
         | [last] -> eval last env
         | e :: rest -> let _ = eval e env in go rest
         | [] -> VNil
       in
       go exprs)

  (* (and e1 e2 …) — short-circuit conjunction
     LEARNING NOTE:
     Returns the last truthy value or #f on first false. *)
  | List (Symbol "and" :: exprs) ->
    eval_and exprs env (VBool true)

  (* (or e1 e2 …) — short-circuit disjunction *)
  | List (Symbol "or" :: exprs) ->
    eval_or exprs env

  (* (not e) *)
  | List [Symbol "not"; e] ->
    (match eval e env with
     | VBool false | VNil -> VBool true
     | _                  -> VBool false)

  (* (when cond body…) — execute body only if cond is true *)
  | List (Symbol "when" :: cond_expr :: body_exprs) ->
    (match eval cond_expr env with
     | VBool false | VNil -> VNil
     | _ ->
       let body = match body_exprs with
         | [b] -> b
         | _   -> List (Symbol "begin" :: body_exprs) in
       eval body env)

  (* (unless cond body…) — execute body only if cond is false *)
  | List (Symbol "unless" :: cond_expr :: body_exprs) ->
    (match eval cond_expr env with
     | VBool false | VNil ->
       let body = match body_exprs with
         | [b] -> b
         | _   -> List (Symbol "begin" :: body_exprs) in
       eval body env
     | _ -> VNil)

  (* ---------------------------------------------------------------- *)
  (*  Function application                                             *)
  (* ---------------------------------------------------------------- *)

  (* LEARNING NOTE:
     The "else" branch: any list not matching a special form is treated
     as a function call.  We evaluate the head to get a function, evaluate
     all arguments, then call [apply]. *)
  | List [] -> VNil

  | List (head :: arg_exprs) ->
    let func = eval head env in
    let args = List.map (fun e -> eval e env) arg_exprs in
    apply func args

(** [eval_cond clauses env] — worker for the [cond] special form. *)
and eval_cond clauses env =
  match clauses with
  | [] -> VNil
  | List (Symbol "else" :: body) :: _ ->
    eval_begin body env
  | List (test :: body) :: rest ->
    (match eval test env with
     | VBool false | VNil -> eval_cond rest env
     | v ->
       (match body with
        | [] -> v  (* (cond (test)) — just return the test value *)
        | _  -> eval_begin body env))
  | clause :: _ ->
    failwith ("cond: bad clause " ^ show_expr clause)

and eval_begin exprs env =
  match exprs with
  | []     -> VNil
  | [last] -> eval last env
  | e :: rest -> let _ = eval e env in eval_begin rest env

and eval_and exprs env last =
  match exprs with
  | [] -> last
  | e :: rest ->
    (match eval e env with
     | VBool false as f -> f
     | VNil             -> VBool false
     | v                -> eval_and rest env v)

and eval_or exprs env =
  match exprs with
  | [] -> VBool false
  | e :: rest ->
    (match eval e env with
     | VBool false | VNil -> eval_or rest env
     | v                  -> v)

(** [expr_to_value e] — convert an expression to a value without evaluating.
    Used by [quote].

    LEARNING NOTE:
    [quote] returns its argument as data, not code.  We recursively convert
    the expr tree into the corresponding value tree. *)
and expr_to_value : expr -> value = function
  | Int n      -> VInt n
  | Float f    -> VFloat f
  | Bool b     -> VBool b
  | Str s      -> VStr s
  | Symbol s   -> VSymbol s
  | Nil        -> VNil
  | List exprs -> VList (List.map expr_to_value exprs)

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

let builtins : (string, value list -> value) Hashtbl.t =
  Hashtbl.create 64

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

    LEARNING NOTE:
    The initial environment contains all the built-in functions.
    We bind each name to a sentinel closure; [apply] intercepts those
    and dispatches to the OCaml function in [builtins]. *)
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

    LEARNING NOTE:
    This re-enters the evaluator with the patched apply dispatch.
    For production code you'd restructure with proper modules; this
    ref trick is a learning simplification. *)
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

    LEARNING NOTE:
    This is the main loop for loading a .wal file.  The fold accumulates an
    updated env so that definitions earlier in the file are visible to later
    expressions. *)
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
      let v = VClosure { params; body; env = e } in
      let new_env = define name v e in
      (v, new_env)
    | _ ->
      let v = run expr e in
      (v, e)
  ) (VNil, env) exprs

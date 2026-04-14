(** test_wal.ml
    Alcotest test suite for wal-fast.

    LEARNING NOTE:
    Alcotest is OCaml's standard unit testing library.
    Tests are organized into "suites" (lists of test cases).
    Each case uses [Alcotest.check testable_type "description" expected actual].

    Key testables:
      Alcotest.int      -- compare ints
      Alcotest.string   -- compare strings
      Alcotest.bool     -- compare bools
      Alcotest.(list int) -- compare int lists
*)

open Wal_fast

(* ------------------------------------------------------------------ *)
(*  Helpers                                                              *)
(* ------------------------------------------------------------------ *)

(** Parse a single expression and return it as a string for comparison. *)
let parse_show src =
  Wal_ast.show_expr (Wal_reader.parse_expr src)

(** Evaluate a WAL string in a fresh environment and show the result. *)
let eval_str src =
  let env   = Wal_eval.initial_env () in
  let exprs = Wal_reader.parse_program src in
  let v     = List.fold_left (fun _ e -> Wal_eval.eval e env) Wal_ast.VNil exprs in
  Wal_ast.show_value v

(** Evaluate and return the integer result. *)
let eval_int src =
  match Wal_reader.parse_program src
        |> List.fold_left (fun _ e -> Wal_eval.eval e (Wal_eval.initial_env ()))
             Wal_ast.VNil
  with
  | Wal_ast.VInt n -> n
  | v -> failwith ("Expected VInt, got: " ^ Wal_ast.show_value v)

(** Evaluate and return the bool result. *)
let eval_bool src =
  match Wal_reader.parse_program src
        |> List.fold_left (fun _ e -> Wal_eval.eval e (Wal_eval.initial_env ()))
             Wal_ast.VNil
  with
  | Wal_ast.VBool b -> b
  | v -> failwith ("Expected VBool, got: " ^ Wal_ast.show_value v)

(* ------------------------------------------------------------------ *)
(*  Reader tests                                                         *)
(* ------------------------------------------------------------------ *)

let test_parse_int () =
  Alcotest.(check string) "parse int" "42" (parse_show "42")

let test_parse_neg_int () =
  Alcotest.(check string) "parse negative int" "-7" (parse_show "-7")

let test_parse_float () =
  Alcotest.(check string) "parse float" "3.14" (parse_show "3.14")

let test_parse_bool_true () =
  Alcotest.(check string) "parse #t" "#t" (parse_show "#t")

let test_parse_bool_false () =
  Alcotest.(check string) "parse #f" "#f" (parse_show "#f")

let test_parse_string () =
  Alcotest.(check string) "parse string" {|"hello"|} (parse_show {|"hello"|})

let test_parse_symbol () =
  Alcotest.(check string) "parse symbol" "clk" (parse_show "clk")

let test_parse_hyphen_symbol () =
  Alcotest.(check string) "parse hyphen symbol" "rising-edge"
    (parse_show "rising-edge")

let test_parse_empty_list () =
  Alcotest.(check string) "parse empty list" "()" (parse_show "()")

let test_parse_simple_list () =
  Alcotest.(check string) "parse list" "(+ 1 2)" (parse_show "(+ 1 2)")

let test_parse_nested_list () =
  Alcotest.(check string) "parse nested" "(if (= x 0) (+ 1 2) y)"
    (parse_show "(if (= x 0) (+ 1 2) y)")

let test_parse_quote_shorthand () =
  Alcotest.(check string) "parse quote shorthand" "(quote x)"
    (parse_show "'x")

let test_parse_comment_ignored () =
  (* Comments should be stripped; only the expression remains *)
  Alcotest.(check string) "comment ignored" "42"
    (parse_show "; this is a comment\n42")

let test_parse_program_multiple () =
  let exprs = Wal_reader.parse_program "(+ 1 2)\n(* 3 4)" in
  Alcotest.(check int) "program has 2 exprs" 2 (List.length exprs)

(* ------------------------------------------------------------------ *)
(*  Evaluator tests                                                      *)
(* ------------------------------------------------------------------ *)

let test_eval_addition () =
  Alcotest.(check int) "addition" 3 (eval_int "(+ 1 2)")

let test_eval_nested_arith () =
  Alcotest.(check int) "nested arith" 14 (eval_int "(+ (* 2 3) (* 2 4))")

let test_eval_subtraction () =
  Alcotest.(check int) "subtraction" 5 (eval_int "(- 10 5)")

let test_eval_division () =
  Alcotest.(check int) "division" 3 (eval_int "(/ 9 3)")

let test_eval_modulo () =
  Alcotest.(check int) "modulo" 1 (eval_int "(mod 10 3)")

let test_eval_if_true () =
  Alcotest.(check int) "if true branch" 1 (eval_int "(if #t 1 2)")

let test_eval_if_false () =
  Alcotest.(check int) "if false branch" 2 (eval_int "(if #f 1 2)")

let test_eval_eq_true () =
  Alcotest.(check bool) "= true" true (eval_bool "(= 5 5)")

let test_eval_eq_false () =
  Alcotest.(check bool) "= false" false (eval_bool "(= 5 6)")

let test_eval_let () =
  Alcotest.(check int) "let binding" 42
    (eval_int "(let ((x 40) (y 2)) (+ x y))")

let test_eval_let_star () =
  Alcotest.(check int) "let* sequential" 6
    (eval_int "(let* ((x 2) (y (* x 3))) y)")

let test_eval_define_and_use () =
  Alcotest.(check int) "define then use" 10
    (eval_int "(define z 10) z")

let test_eval_lambda () =
  Alcotest.(check int) "lambda application" 9
    (eval_int "((lambda (x) (* x x)) 3)")

let test_eval_named_function () =
  Alcotest.(check int) "named function" 25
    (eval_int "(define square (lambda (x) (* x x))) (square 5)")

let test_eval_recursive_function () =
  Alcotest.(check int) "recursive factorial" 120
    (eval_int {|
      (define fact (lambda (n)
        (if (= n 0)
            1
            (* n (fact (- n 1))))))
      (fact 5)
    |})

let test_eval_map () =
  Alcotest.(check string) "map doubles" "(2 4 6)"
    (eval_str "(map (lambda (x) (* x 2)) (list 1 2 3))")

let test_eval_filter () =
  Alcotest.(check string) "filter evens" "(2 4)"
    (eval_str "(filter (lambda (x) (= (mod x 2) 0)) (list 1 2 3 4 5))")

let test_eval_length () =
  Alcotest.(check int) "list length" 3
    (eval_int "(length (list 1 2 3))")

let test_eval_car () =
  Alcotest.(check int) "car" 1
    (eval_int "(car (list 1 2 3))")

let test_eval_cdr () =
  Alcotest.(check string) "cdr" "(2 3)"
    (eval_str "(cdr (list 1 2 3))")

let test_eval_and () =
  Alcotest.(check bool) "and true" true  (eval_bool "(and #t #t)")

let test_eval_and_short () =
  Alcotest.(check bool) "and short-circuit" false (eval_bool "(and #t #f)")

let test_eval_or () =
  Alcotest.(check bool) "or" true (eval_bool "(or #f #t)")

let test_eval_cond () =
  Alcotest.(check int) "cond" 2
    (eval_int "(cond ((= 1 2) 1) ((= 2 2) 2) (#t 3))")

(* ------------------------------------------------------------------ *)
(*  VCD binary-search test                                              *)
(* ------------------------------------------------------------------ *)

let test_vcd_value_at () =
  let changes : Wal_vcd.change array = [|
    { time = 0;  value = Wal_vcd.Bit 0 };
    { time = 10; value = Wal_vcd.Bit 1 };
    { time = 20; value = Wal_vcd.Bit 0 };
    { time = 30; value = Wal_vcd.Bit 1 };
  |] in
  let sig_ : Wal_vcd.signal =
    { id = "!"; name = "clk"; width = 1; changes } in
  (* Before first change: unknown *)
  Alcotest.(check bool) "before first" true
    (Wal_vcd.value_at sig_ (-1) = Wal_vcd.X);
  (* Exact match *)
  Alcotest.(check bool) "exact t=10" true
    (Wal_vcd.value_at sig_ 10 = Wal_vcd.Bit 1);
  (* Between changes: returns last seen value *)
  Alcotest.(check bool) "between t=15" true
    (Wal_vcd.value_at sig_ 15 = Wal_vcd.Bit 1);
  (* After last *)
  Alcotest.(check bool) "after last t=100" true
    (Wal_vcd.value_at sig_ 100 = Wal_vcd.Bit 1)

(* ------------------------------------------------------------------ *)
(*  Test registration                                                    *)
(* ------------------------------------------------------------------ *)

let reader_tests = [
  "parse int",            `Quick, test_parse_int;
  "parse negative int",   `Quick, test_parse_neg_int;
  "parse float",          `Quick, test_parse_float;
  "parse #t",             `Quick, test_parse_bool_true;
  "parse #f",             `Quick, test_parse_bool_false;
  "parse string",         `Quick, test_parse_string;
  "parse symbol",         `Quick, test_parse_symbol;
  "parse hyphen symbol",  `Quick, test_parse_hyphen_symbol;
  "parse empty list",     `Quick, test_parse_empty_list;
  "parse simple list",    `Quick, test_parse_simple_list;
  "parse nested list",    `Quick, test_parse_nested_list;
  "parse quote shorthand",`Quick, test_parse_quote_shorthand;
  "comment ignored",      `Quick, test_parse_comment_ignored;
  "program multiple",     `Quick, test_parse_program_multiple;
]

let eval_tests = [
  "addition",             `Quick, test_eval_addition;
  "nested arith",         `Quick, test_eval_nested_arith;
  "subtraction",          `Quick, test_eval_subtraction;
  "division",             `Quick, test_eval_division;
  "modulo",               `Quick, test_eval_modulo;
  "if true",              `Quick, test_eval_if_true;
  "if false",             `Quick, test_eval_if_false;
  "= true",               `Quick, test_eval_eq_true;
  "= false",              `Quick, test_eval_eq_false;
  "let binding",          `Quick, test_eval_let;
  "let* sequential",      `Quick, test_eval_let_star;
  "define and use",       `Quick, test_eval_define_and_use;
  "lambda",               `Quick, test_eval_lambda;
  "named function",       `Quick, test_eval_named_function;
  "recursive factorial",  `Quick, test_eval_recursive_function;
  "map",                  `Quick, test_eval_map;
  "filter",               `Quick, test_eval_filter;
  "length",               `Quick, test_eval_length;
  "car",                  `Quick, test_eval_car;
  "cdr",                  `Quick, test_eval_cdr;
  "and true",             `Quick, test_eval_and;
  "and short-circuit",    `Quick, test_eval_and_short;
  "or",                   `Quick, test_eval_or;
  "cond",                 `Quick, test_eval_cond;
]

let vcd_tests = [
  "value_at binary search", `Quick, test_vcd_value_at;
]

let () =
  Alcotest.run "wal-fast" [
    "reader", reader_tests;
    "eval",   eval_tests;
    "vcd",    vcd_tests;
  ]

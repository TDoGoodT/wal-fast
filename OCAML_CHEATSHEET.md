# OCaml Cheat Sheet — wal-fast Edition

A practical reference for Snir, learning OCaml for the first time through
the wal-fast project. Every section has runnable examples. When in doubt,
paste into `dune utop lib` and experiment.

---

## Table of Contents

1. [Types and Values](#1-types-and-values)
2. [Pattern Matching](#2-pattern-matching)
3. [Functions](#3-functions)
4. [Let Bindings](#4-let-bindings)
5. [Lists](#5-lists)
6. [Option Type](#6-option-type)
7. [Result Type](#7-result-type)
8. [Strings and IO](#8-strings-and-io)
9. [Modules](#9-modules)
10. [Useful Stdlib Modules](#10-useful-stdlib-modules)
11. [Exceptions](#11-exceptions)
12. [Dune Build Commands](#12-dune-build-commands)
13. [OCaml REPL (utop)](#13-ocaml-repl-utop)
14. [Common Gotchas for Beginners](#14-common-gotchas-for-beginners)
15. [WAL-FAST Specific Patterns](#15-wal-fast-specific-patterns)

---

## 1. Types and Values

### Primitive types

```ocaml
(* int — whole numbers, no size suffix needed *)
let age : int = 42
let negative : int = -7

(* float — always written with a dot *)
let pi : float = 3.14159
let one : float = 1.0   (* not just 1 — that would be int *)

(* bool *)
let yes : bool = true
let no  : bool = false

(* string — double quotes only, single quotes are for char *)
let greeting : string = "hello, world"

(* char — single character, single quotes *)
let letter : char = 'A'

(* unit — the "nothing" type, returned by side-effecting functions *)
let nothing : unit = ()
```

### Type annotations (optional but helpful while learning)

```ocaml
(* OCaml infers types, but you can annotate for clarity *)
let x : int = 5
let f (n : int) : string = string_of_int n
```

### Type aliases

```ocaml
(* Give an existing type a new name *)
type name   = string
type signal = string   (* used in wal_ast.ml *)
type scope  = (string * value) list   (* also from wal_ast.ml *)
```

### Records (like structs)

```ocaml
(* Define the type *)
type point = {
  x : int;
  y : int;
}

(* Create a value *)
let origin = { x = 0; y = 0 }
let p      = { x = 3; y = 4 }

(* Access a field *)
let px = p.x   (* 3 *)

(* Create a modified copy (record update syntax) *)
let q = { p with y = 10 }   (* { x = 3; y = 10 } *)

(* Records are IMMUTABLE by default. p.x <- 5 is an error. *)
```

The `closure` type in `wal_ast.ml` is a record:

```ocaml
(* From wal_ast.ml — this is a real record in the project *)
type closure = {
  params : string list;   (* parameter names  *)
  body   : expr;          (* unevaluated body *)
  env    : env;           (* captured scope   *)
}
```

### Variants / Algebraic Data Types (ADTs)

This is OCaml's superpower. One type that can be several different things.

```ocaml
(* Simple enum — no data attached *)
type color = Red | Green | Blue

(* With data attached — each constructor can carry different types *)
type color_ext =
  | Red
  | Green
  | Blue
  | Custom of int * int * int   (* RGB values *)

let my_color = Custom (255, 128, 0)
let pure_red = Red
```

The `expr` type in `wal_ast.ml` is exactly this pattern:

```ocaml
(* From wal_ast.ml — the WAL AST *)
type expr =
  | Int    of int       (* 42        *)
  | Float  of float     (* 3.14      *)
  | Bool   of bool      (* #t  #f    *)
  | Str    of string    (* "hello"   *)
  | Symbol of string    (* clk       *)
  | Nil                 (* ()        *)
  | List   of expr list (* (+ 1 2)   *)
```

### Tuples

```ocaml
(* Group values without naming them *)
let pair  : int * string       = (42, "hello")
let triple: int * string * bool = (1, "hi", true)

(* Access via pattern matching (see section 2) *)
let (n, s) = pair   (* n = 42, s = "hello" *)
let fst_of = fst pair    (* 42  — only works on pairs *)
let snd_of = snd pair    (* "hello" *)
```

---

## 2. Pattern Matching

Pattern matching is how you inspect values in OCaml. Think of it as a
powerful switch/case that the compiler checks for you.

### Basic match

```ocaml
let describe_int n =
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | _ -> "many"   (* _ is wildcard: matches anything, discards it *)
```

### Matching a variant (ADT)

```ocaml
type color = Red | Green | Blue | Custom of int * int * int

let color_name c =
  match c with
  | Red            -> "red"
  | Green          -> "green"
  | Blue           -> "blue"
  | Custom (r,g,b) -> Printf.sprintf "rgb(%d,%d,%d)" r g b
```

### Matching expr — the wal-fast way

```ocaml
open Wal_ast

let eval_literal expr =
  match expr with
  | Int n    -> Printf.printf "integer: %d\n" n
  | Float f  -> Printf.printf "float: %f\n" f
  | Bool b   -> Printf.printf "bool: %b\n" b
  | Str s    -> Printf.printf "string: %s\n" s
  | Symbol s -> Printf.printf "symbol: %s\n" s
  | Nil      -> Printf.printf "nil\n"
  | List _   -> Printf.printf "compound expression\n"
```

### Wildcard and variable patterns

```ocaml
match x with
| 0     -> "zero"           (* literal match *)
| n     -> "got " ^ string_of_int n  (* bind to n *)
| _     -> "ignored"        (* discard — use when you don't need the value *)
```

### Nested patterns

```ocaml
(* Match a list that starts with a specific symbol *)
match expr with
| List (Symbol "define" :: Symbol name :: body :: []) ->
    Printf.printf "define %s\n" name
| List (Symbol "+" :: args) ->
    Printf.printf "addition with %d args\n" (List.length args)
| _ ->
    Printf.printf "something else\n"
```

### Guard clauses (when)

```ocaml
let classify n =
  match n with
  | n when n < 0  -> "negative"
  | 0             -> "zero"
  | n when n < 10 -> "small"
  | _             -> "large"
```

### Exhaustiveness — the compiler has your back

```ocaml
type shape = Circle | Square | Triangle

let area shape =
  match shape with
  | Circle   -> 3.14
  | Square   -> 1.0
  (* Forgot Triangle! The compiler warns: "this pattern-matching is not exhaustive" *)
```

Add the missing case to silence the warning. Never use `| _ ->` to silence
it — that hides real bugs.

### Or-patterns

```ocaml
match c with
| Red | Blue -> "cool color"
| Green      -> "earthy"
| Custom _   -> "custom"
```

---

## 3. Functions

### Basic definition

```ocaml
(* let name param1 param2 = body *)
let add x y = x + y

let greet name = "Hello, " ^ name ^ "!"

(* Call it *)
let result = add 3 4        (* 7 *)
let msg    = greet "Snir"   (* "Hello, Snir!" *)
```

### Anonymous functions (lambdas)

```ocaml
(* fun param -> body *)
let double = fun x -> x * 2

(* Same thing, just shorter syntax *)
let double x = x * 2

(* Inline in a call *)
List.map (fun x -> x * 2) [1; 2; 3]   (* [2; 4; 6] *)
```

### Currying and partial application

Every OCaml function takes exactly one argument. Multi-arg functions are
actually functions that return functions.

```ocaml
let add x y = x + y
(* add : int -> int -> int
   That means: takes int, returns (int -> int) *)

let add5 = add 5          (* partially applied — add5 : int -> int *)
let result = add5 3       (* 8 *)

(* This is why List.map (fun x -> x * 2) works — you partially apply *)
let double_all = List.map (fun x -> x * 2)   (* 'int list -> int list' *)
```

### The pipe operator |>

Sends a value into a function. Reads left-to-right, like a Unix pipe.

```ocaml
(* Without pipe — reads inside-out *)
let result = String.length (String.uppercase_ascii "hello")

(* With pipe — reads left-to-right *)
let result = "hello"
  |> String.uppercase_ascii
  |> String.length

(* Very useful for chaining list operations *)
let big_evens =
  [1; 2; 3; 4; 5; 6; 7; 8]
  |> List.filter (fun x -> x mod 2 = 0)
  |> List.map    (fun x -> x * 10)
  |> List.rev
(* [80; 60; 40; 20] *)
```

### The @@ operator (reverse application)

Avoids parentheses around a whole expression.

```ocaml
(* These are identical *)
print_endline (string_of_int 42)
print_endline @@ string_of_int 42
```

### Labeled arguments

Names arguments so call-site order doesn't matter.

```ocaml
(* Define with ~ prefix *)
let greet ~name ~greeting = greeting ^ ", " ^ name ^ "!"

(* Call — order doesn't matter *)
let msg1 = greet ~name:"Snir" ~greeting:"Hello"
let msg2 = greet ~greeting:"Hey" ~name:"Snir"
```

### Optional arguments

```ocaml
(* ?name makes the argument optional, wrapped in option *)
let connect ?(port = 8080) host =
  Printf.sprintf "%s:%d" host port

let c1 = connect "localhost"          (* "localhost:8080" *)
let c2 = connect ~port:9090 "server"  (* "server:9090" *)
```

---

## 4. Let Bindings

### Top-level

```ocaml
(* Binds a name in the current module scope *)
let x = 42
let pi = 3.14159
let message = "hello"
```

### Local bindings (let ... in)

```ocaml
let result =
  let a = 3 in
  let b = 4 in
  a + b           (* result = 7; a and b don't exist outside *)
```

### Recursive functions (let rec)

By default `let` definitions can't call themselves. Add `rec` to allow it.

```ocaml
let rec factorial n =
  if n <= 0 then 1
  else n * factorial (n - 1)

(* Mutually recursive — use 'and' *)
let rec is_even n = if n = 0 then true  else is_odd  (n - 1)
and     is_odd  n = if n = 0 then false else is_even (n - 1)
```

### Destructuring in let

```ocaml
(* Unpack a tuple *)
let (x, y) = (3, 4)

(* Unpack a record *)
let { x; y } = some_point
(* now x and y are bound *)

(* In function arguments *)
let dist { x; y } = sqrt (float_of_int (x*x + y*y))
```

### let with multiple returns (using tuples)

```ocaml
(* OCaml has no multiple return — use a tuple *)
let min_and_max lst =
  (List.fold_left min max_int lst,
   List.fold_left max min_int lst)

let (lo, hi) = min_and_max [3; 1; 4; 1; 5; 9]
```

---

## 5. Lists

Lists are **immutable** singly-linked lists. Every element has the same type.

### Creating lists

```ocaml
let empty = []
let nums  = [1; 2; 3; 4]         (* semicolons separate, NOT commas *)
let words = ["hello"; "world"]

(* Cons operator :: prepends an element *)
let more = 0 :: nums              (* [0; 1; 2; 3; 4] *)
let head :: tail = nums           (* head = 1, tail = [2;3;4] — via pattern *)
```

### Pattern matching lists

```ocaml
let rec sum lst =
  match lst with
  | []        -> 0                  (* base case: empty list *)
  | x :: rest -> x + sum rest       (* x is head, rest is tail *)

(* Matching specific structure *)
match lst with
| []           -> "empty"
| [x]          -> "one element"
| [x; y]       -> "exactly two"
| x :: y :: _  -> "at least two, starts with x then y"
```

### Common List functions

```ocaml
List.length [1;2;3]                     (* 3 *)
List.rev    [1;2;3]                     (* [3;2;1] *)
List.append [1;2] [3;4]                 (* [1;2;3;4] — same as @ *)
[1;2] @ [3;4]                           (* [1;2;3;4] *)

List.map    (fun x -> x * 2) [1;2;3]   (* [2;4;6] *)
List.filter (fun x -> x > 2) [1;2;3;4] (* [3;4] *)

(* fold_left: accumulate from left, strict left-to-right *)
List.fold_left  (fun acc x -> acc + x) 0 [1;2;3]  (* 6 *)

(* fold_right: accumulate from right (useful for building lists) *)
List.fold_right (fun x acc -> x :: acc) [1;2;3] []  (* [1;2;3] *)

List.mem   3 [1;2;3]                    (* true — element membership *)
List.assoc "key" [("key", 42)]          (* 42 — key-value lookup *)
List.assoc_opt "key" [("x", 1)]         (* None — safe version *)

List.iter  (fun x -> print_int x) [1;2;3]  (* prints 123, returns unit *)
List.for_all (fun x -> x > 0) [1;2;3]  (* true *)
List.exists  (fun x -> x > 2) [1;2;3]  (* true *)
List.concat [[1;2];[3;4]]               (* [1;2;3;4] *)
List.concat_map (fun x -> [x; x]) [1;2] (* [1;1;2;2] *)
```

### The environment in wal_env.ml uses lists

```ocaml
(* env is (string * value) list list — a list of scopes *)
(* Each scope is a (string * value) list — an association list *)

(* Lookup walks the scope stack using List.assoc_opt *)
let rec lookup name env =
  match env with
  | []            -> None
  | scope :: rest ->
    match List.assoc_opt name scope with
    | Some v -> Some v
    | None   -> lookup name rest
```

---

## 6. Option Type

Represents a value that might not exist. The safe alternative to null.

```ocaml
(* The type definition — built into OCaml *)
type 'a option = None | Some of 'a

(* 'a means "option of any type" — it's generic *)
let maybe_int  : int option    = Some 42
let no_int     : int option    = None
let maybe_str  : string option = Some "hello"
```

### Pattern matching options

```ocaml
let describe opt =
  match opt with
  | None   -> "nothing here"
  | Some x -> "got: " ^ string_of_int x

(* In wal_env.ml *)
match List.assoc_opt name scope with
| Some v -> Some v         (* found it *)
| None   -> lookup name rest  (* keep searching *)
```

### Option helper functions

```ocaml
Option.map (fun x -> x + 1) (Some 5)   (* Some 6 *)
Option.map (fun x -> x + 1) None        (* None *)

Option.bind (Some 5) (fun x -> if x > 0 then Some x else None)  (* Some 5 *)
Option.bind None     (fun x -> Some (x + 1))                     (* None *)

Option.value (Some 42) ~default:0   (* 42 *)
Option.value None      ~default:0   (* 0  *)

Option.is_some (Some 1)  (* true *)
Option.is_none None      (* true *)

(* Get the value or raise an exception *)
Option.get (Some 42)     (* 42 *)
Option.get None          (* raises Invalid_argument *)
```

### Why not exceptions?

Use `option` when "not found" is a normal, expected situation (like
`lookup` in `wal_env.ml`). Use exceptions for truly unexpected failures.

---

## 7. Result Type

Like option, but the error case carries information.

```ocaml
(* Built into OCaml *)
type ('a, 'e) result = Ok of 'a | Error of 'e

(* Examples *)
let good : (int, string) result = Ok 42
let bad  : (int, string) result = Error "something went wrong"
```

### Pattern matching result

```ocaml
let handle res =
  match res with
  | Ok value    -> Printf.printf "Success: %d\n" value
  | Error msg   -> Printf.printf "Error: %s\n" msg
```

### Result helper functions

```ocaml
Result.map  (fun x -> x * 2) (Ok 5)          (* Ok 10 *)
Result.map  (fun x -> x * 2) (Error "oops")  (* Error "oops" *)

Result.bind (Ok 5) (fun x -> if x > 0 then Ok x else Error "negative")
(* Ok 5 *)

Result.bind (Error "fail") (fun x -> Ok (x + 1))
(* Error "fail" — short-circuits *)

Result.map_error (fun e -> "wrapped: " ^ e) (Error "oops")
(* Error "wrapped: oops" *)

Result.get_ok    (Ok 42)       (* 42 *)
Result.get_error (Error "oh")  (* "oh" *)
```

### Chaining with Result.bind (railway pattern)

```ocaml
(* Each step either passes Ok forward or stops with Error *)
let parse_and_eval input =
  parse input
  |> Result.bind typecheck
  |> Result.bind evaluate
```

---

## 8. Strings and IO

### String operations

```ocaml
let s = "hello, world"

String.length s              (* 12 *)
String.sub s 0 5             (* "hello" — sub start len *)
String.uppercase_ascii s     (* "HELLO, WORLD" *)
String.lowercase_ascii s     (* "hello, world" *)
String.contains s 'o'        (* true *)
String.concat ", " ["a";"b";"c"]   (* "a, b, c" *)
String.split_on_char ',' "a,b,c"   (* ["a";"b";"c"] *)

(* Concatenation uses ^ not + *)
let full = "Hello" ^ ", " ^ "World"

(* Convert to/from other types *)
string_of_int   42       (* "42" *)
string_of_float 3.14     (* "3.14" *)
int_of_string   "42"     (* 42 — raises if not a valid int *)
int_of_string_opt "42"   (* Some 42 *)
int_of_string_opt "abc"  (* None *)
```

### Printf — formatted output

```ocaml
(* Printf.printf: print to stdout *)
Printf.printf "Hello, %s! You are %d years old.\n" "Snir" 27

(* Printf.sprintf: format to a string *)
let msg = Printf.sprintf "Value: %d, Float: %.2f" 42 3.14159
(* "Value: 42, Float: 3.14" *)

(* Format specifiers *)
(* %d  int          *)
(* %f  float        *)
(* %s  string       *)
(* %b  bool         *)
(* %c  char         *)
(* %S  string with quotes (useful for debugging) *)
(* %.2f  float with 2 decimal places *)
```

### Basic print functions

```ocaml
print_string  "hello"          (* no newline *)
print_endline "hello"          (* with newline — use this mostly *)
print_int     42
print_float   3.14
print_char    'A'
print_newline ()               (* just a newline *)
```

### Reading input

```ocaml
let line  = read_line ()       (* reads one line from stdin *)
let int_n = int_of_string (read_line ())
```

### Buffer — efficient string building

Use `Buffer` when you're building a string incrementally (e.g. in a parser).
String concatenation with `^` in a loop is O(n²). Buffer is O(n).

```ocaml
let buf = Buffer.create 64    (* initial capacity hint *)

Buffer.add_char   buf 'H'
Buffer.add_string buf "ello"
Buffer.add_char   buf '!'

let result = Buffer.contents buf   (* "Hello!" *)
let len    = Buffer.length buf     (* 6 *)
Buffer.clear buf                   (* reset without freeing *)
```

---

## 9. Modules

OCaml organizes code into modules. Every `.ml` file is automatically a module.

### Inline module

```ocaml
module Point = struct
  type t = { x: float; y: float }

  let origin = { x = 0.0; y = 0.0 }

  let distance a b =
    let dx = a.x -. b.x in
    let dy = a.y -. b.y in
    sqrt (dx *. dx +. dy *. dy)
end

(* Use with module prefix *)
let p = Point.{ x = 3.0; y = 4.0 }
let d = Point.distance Point.origin p
```

### Opening a module

```ocaml
open Point

(* Now you can use 'distance' instead of 'Point.distance' *)
let d = distance origin p

(* wal_env.ml does this *)
open Wal_ast   (* brings expr, value, env into scope *)
```

### Local open (scoped)

```ocaml
(* Opens only inside the expression *)
let d = Point.(distance origin { x = 1.0; y = 0.0 })
```

### Module interfaces (.mli files)

A `.mli` file is the public API of a module. It hides implementation details.

```ocaml
(* wal_env.mli — what callers can see *)
val empty      : Wal_ast.env
val lookup     : string -> Wal_ast.env -> Wal_ast.value option
val lookup_exn : string -> Wal_ast.env -> Wal_ast.value
val extend     : (string * Wal_ast.value) list -> Wal_ast.env -> Wal_ast.env
val define     : string -> Wal_ast.value -> Wal_ast.env -> Wal_ast.env
```

If a function is in `wal_env.ml` but not in `wal_env.mli`, it's private.

### Functors (modules that take modules as parameters)

```ocaml
(* Example: Map.Make takes a module with a comparable type *)
module StringMap = Map.Make(String)

let m = StringMap.empty
let m = StringMap.add "key" 42 m
let v = StringMap.find_opt "key" m   (* Some 42 *)
```

---

## 10. Useful Stdlib Modules

### List

Already covered in section 5. Key functions: `map`, `filter`, `fold_left`,
`assoc_opt`, `iter`, `concat_map`.

### Array (mutable!)

```ocaml
let arr = [| 1; 2; 3; 4 |]    (* note |  brackets *)
arr.(0)                         (* 1 — read with .() *)
arr.(0) <- 99                   (* mutate! now arr = [|99;2;3;4|] *)
Array.length arr                (* 4 *)
Array.make 5 0                  (* [|0;0;0;0;0|] *)
Array.init 5 (fun i -> i * 2)  (* [|0;2;4;6;8|] *)
Array.map (fun x -> x+1) arr
Array.iter print_int arr
```

### String (see section 8)

### Char

```ocaml
Char.code 'A'          (* 65 — ASCII code *)
Char.chr  65           (* 'A' *)
Char.uppercase_ascii 'a'  (* 'A' *)
Char.lowercase_ascii 'A'  (* 'a' *)
```

### Int / Float

```ocaml
Int.max_int          (* biggest int on your platform *)
Int.min_int
Int.abs (-5)         (* 5 *)

Float.pi             (* 3.14159... *)
Float.infinity
Float.nan
Float.abs (-1.0)     (* 1.0 *)
floor 3.7            (* 3.0 *)
ceil  3.2            (* 4.0 *)
sqrt  2.0            (* 1.41421... *)
```

### Hashtbl (mutable hash table)

```ocaml
let tbl : (string, int) Hashtbl.t = Hashtbl.create 16

Hashtbl.add     tbl "a" 1
Hashtbl.add     tbl "b" 2
Hashtbl.find     tbl "a"        (* 1 — raises if not found *)
Hashtbl.find_opt tbl "c"        (* None *)
Hashtbl.mem      tbl "a"        (* true *)
Hashtbl.remove   tbl "a"
Hashtbl.length   tbl            (* number of bindings *)
Hashtbl.iter (fun k v -> Printf.printf "%s=%d\n" k v) tbl
Hashtbl.fold (fun k v acc -> acc + v) tbl 0   (* sum values *)
```

### Map (functional, immutable)

Better than Hashtbl when you want immutability and structural sharing.

```ocaml
module StringMap = Map.Make(String)

let m0 = StringMap.empty
let m1 = StringMap.add "x" 1 m0
let m2 = StringMap.add "y" 2 m1   (* m1 is unchanged *)

StringMap.find     "x" m2    (* 1 *)
StringMap.find_opt "z" m2    (* None *)
StringMap.mem      "x" m2    (* true *)
StringMap.remove   "x" m2    (* new map without "x" *)
StringMap.cardinal m2         (* 2 — number of bindings *)
StringMap.iter (fun k v -> Printf.printf "%s->%d\n" k v) m2
```

### Seq (lazy sequences)

```ocaml
(* Generate an infinite sequence — only computed on demand *)
let nats = Seq.ints 0   (* 0, 1, 2, 3, ... *)

Seq.take 5 nats |> List.of_seq   (* [0;1;2;3;4] *)

(* Convert between Seq and List *)
List.to_seq   [1;2;3]     (* Seq.t *)
List.of_seq   some_seq    (* list  *)
Array.to_seq  [|1;2;3|]
```

### Bytes (mutable byte buffer — for binary data)

```ocaml
let buf = Bytes.create 10          (* 10 uninitialized bytes *)
Bytes.set buf 0 'H'
Bytes.set buf 1 'i'
let s = Bytes.to_string (Bytes.sub buf 0 2)   (* "Hi" *)

(* For VCD parsing, you may read raw bytes *)
Bytes.get buf 0    (* 'H' *)
Bytes.length buf   (* 10 *)
```

---

## 11. Exceptions

### Defining and raising

```ocaml
(* Define a custom exception *)
exception Parse_error of string
exception Eval_error  of string * int   (* message, line number *)

(* Raise it *)
raise (Parse_error "unexpected token")
raise (Eval_error ("unbound symbol", 42))

(* Shorthand for simple string errors *)
failwith "something went wrong"         (* raises Failure "something went wrong" *)
invalid_arg "argument must be positive" (* raises Invalid_argument *)
```

### Catching exceptions

```ocaml
let safe_parse s =
  try
    let n = int_of_string s in
    Some n
  with
  | Failure _           -> None   (* int_of_string raises Failure on bad input *)
  | Invalid_argument msg -> None

(* Catch specific exceptions *)
try
  raise (Parse_error "oops")
with
| Parse_error msg -> Printf.printf "parse error: %s\n" msg
| Eval_error (msg, line) ->
    Printf.printf "eval error at line %d: %s\n" line msg
| _ ->
    Printf.printf "unknown error\n"
```

### The Result pattern is often better

```ocaml
(* Instead of try/raise, return Ok/Error *)
let parse s =
  match int_of_string_opt s with
  | Some n -> Ok n
  | None   -> Error (Printf.sprintf "not a number: %s" s)
```

Use exceptions for truly exceptional situations. Use `result` or `option`
for expected failure modes (like "symbol not found in env").

---

## 12. Dune Build Commands

```bash
# Build everything
dune build

# Build and run all tests
dune test

# Run a specific test file
dune test test/test_wal.ml

# Run the binary (with arguments after --)
dune exec bin/main.exe
dune exec bin/main.exe -- repl test.vcd
dune exec bin/main.exe -- --help

# Interactive REPL with your library loaded
# (great for experimenting with wal_ast, wal_env, etc.)
dune utop lib

# Delete all build artifacts
dune clean

# Generate HTML documentation from (** ... *) comments
dune build @doc
# Then open: _build/default/_doc/_html/index.html

# Check for type errors without building
dune build @check

# Build in watch mode — rebuilds on every file save
dune build --watch
```

---

## 13. OCaml REPL (utop)

`utop` is an enhanced OCaml REPL. Launch it inside the project with:

```bash
dune utop lib   # loads wal_fast library automatically
```

### Basic usage

```ocaml
(* Everything needs ;; to evaluate in utop *)
let x = 42;;
(* val x : int = 42 *)

let add x y = x + y;;
(* val add : int -> int -> int = <fun> *)

add 3 4;;
(* - : int = 7 *)

(* Load a file *)
#use "lib/wal_ast.ml";;

(* See the type of an expression *)
#typeof "hello";;

(* List available functions in a module *)
#show List;;

(* Quit *)
#quit;;
```

### Useful utop tips

```ocaml
(* Use open to bring module contents in scope *)
open Wal_ast;;

(* Now you can write Int 42 instead of Wal_ast.Int 42 *)
let e = List [Symbol "+"; Int 1; Int 2];;

(* Pretty-print using show_expr *)
show_expr e;;
(* - : string = "(+ 1 2)" *)
```

---

## 14. Common Gotchas for Beginners

### Semicolons — two very different things

```ocaml
(* ; sequences two expressions, discards the first result *)
print_endline "a"; print_endline "b"   (* prints a then b *)

(* ;; ends a top-level phrase in utop (not needed in .ml files) *)
let x = 42;;   (* in utop *)
```

In `.ml` files you rarely need `;;`. Use `let () = ...` for top-level
side effects:

```ocaml
let () = print_endline "hello"   (* runs at startup *)
```

### Equality: = vs ==

```ocaml
(* = is structural equality — compares contents *)
[1;2;3] = [1;2;3]   (* true *)
"hello" = "hello"   (* true *)

(* == is physical equality — same memory address (rarely what you want) *)
[1;2;3] == [1;2;3]  (* false — different allocations *)
let xs = [1;2;3] in xs == xs  (* true — same object *)
```

**Always use `=` unless you specifically need identity comparison.**

### Int vs Float arithmetic

```ocaml
(* Int operators: + - * / mod *)
3 + 4        (* 7 *)
7 / 2        (* 3 — integer division! *)

(* Float operators have a dot: +. -. *. /. *)
3.0 +. 4.0   (* 7.0 *)
7.0 /. 2.0   (* 3.5 *)

(* NO mixing — this is a type error: *)
3 + 4.0      (* Error: This expression has type float but int expected *)

(* Convert explicitly *)
float_of_int 3 +. 4.0   (* 7.0 *)
int_of_float 3.7         (* 3 — truncates, not rounds *)
Float.round 3.7          (* 4.0 *)
```

### String concatenation is ^ not +

```ocaml
"hello" ^ " world"   (* "hello world" — correct *)
"hello" + " world"   (* type error *)
```

### Mutable references

```ocaml
(* Create a mutable ref *)
let counter = ref 0

(* Read it with ! *)
let n = !counter   (* 0 *)

(* Write it with := *)
counter := !counter + 1
Printf.printf "%d\n" !counter  (* 1 *)

(* Refs are useful for accumulating in loops *)
let sum = ref 0
List.iter (fun x -> sum := !sum + x) [1;2;3;4;5]
Printf.printf "sum = %d\n" !sum   (* 15 *)
```

### Unit type and side effects

```ocaml
(* Functions with side effects return unit *)
let () = print_endline "hello"   (* () is unit *)

(* If you call a side-effecting function in a let binding,
   the result must be unit or you'll get a warning *)
let result = print_endline "hi"   (* result : unit *)

(* Sequence side effects with ; *)
let () =
  print_string "a";
  print_string "b";
  print_newline ()
```

### No implicit return

```ocaml
(* The LAST expression in a function IS the return value *)
let double x =
  let result = x * 2 in
  result              (* returned — no 'return' keyword *)

(* This is confusing at first but very consistent *)
let max_of a b =
  if a > b then a     (* returned when true *)
  else b              (* returned when false *)
```

### Lists are immutable, Arrays are mutable

```ocaml
(* List "update" creates a new list *)
let xs = [1; 2; 3]
let ys = 0 :: xs     (* [0;1;2;3] — xs unchanged *)

(* Array update mutates in place *)
let arr = [| 1; 2; 3 |]
arr.(0) <- 99        (* arr is now [|99;2;3|] *)
```

### if/else is an expression

```ocaml
(* Both branches must have the same type *)
let label = if x > 0 then "positive" else "non-positive"

(* NOT valid — branches have different types *)
let bad = if x > 0 then 42 else "oops"   (* type error *)

(* if without else is only valid when the then branch is unit *)
if x > 0 then print_endline "positive"
(* else () is implied *)
```

---

## 15. WAL-FAST Specific Patterns

### Project module map

```
Wal_ast   — types: expr, value, closure, env
Wal_env   — functions: lookup, extend, define
Wal_reader — (Phase 1) string -> expr
Wal_eval  — (Phase 2) expr * env -> value
Wal_vcd   — (Phase 3) parse .vcd files
Wal_trace — (Phase 3) signal access API
```

---

### How to add a built-in function to the evaluator

When `wal_eval.ml` exists, built-ins will live in an initial environment.
The pattern is: map a WAL symbol to a `VClosure`-like function value.

Here is the pattern you'll follow (pseudo-code that matches the project style):

```ocaml
(* In wal_eval.ml *)
open Wal_ast

(* A built-in is a function from value list -> value *)
type builtin = value list -> value

(* Step 1: write the implementation *)
let builtin_add args =
  match args with
  | [VInt a; VInt b] -> VInt (a + b)
  | [VFloat a; VFloat b] -> VFloat (a +. b)
  | _ -> failwith "'+' expects two numbers"

(* Step 2: register it in the initial environment *)
(* The initial env will look something like this: *)
let builtins : (string * builtin) list = [
  ("+",   builtin_add);
  ("-",   builtin_sub);
  ("*",   builtin_mul);
  ("/",   builtin_div);
  (* add your new built-in here: *)
  ("my-fn", fun args -> (* ... *) VNil);
]

(* Step 3: the evaluator calls the builtin when it sees (my-fn ...) *)
(* In eval, a List expression starting with a Symbol looks it up and calls it *)
```

**To add YOUR built-in:**

1. Write a function `builtin_yourname : value list -> value`
2. Pattern match the `args` for the expected types
3. Add `("your-wal-name", builtin_yourname)` to the `builtins` list
4. Test it: `echo '(your-wal-name 1 2)' | dune exec bin/main.exe -- repl test.vcd`

---

### How to extend the AST with a new expr type

The `expr` type lives in `wal_ast.ml`. Adding a new kind of expression
means adding a new constructor.

**Example: add a `Vec` (vector/array literal) expression:**

```ocaml
(* In wal_ast.ml — add to the expr type *)
type expr =
  | Int    of int
  | Float  of float
  | Bool   of bool
  | Str    of string
  | Symbol of string
  | Nil
  | List   of expr list
  | Vec    of expr list    (* NEW: #(1 2 3) vector literal *)
```

After adding `Vec`, the compiler will warn you about every `match expr`
that doesn't handle `Vec`. Follow the warnings:

1. **`wal_reader.ml`** — parse `#(...)` into `Vec [...]`
2. **`wal_eval.ml`** — evaluate `Vec exprs` into `VList (List.map eval exprs)`
3. **`show_expr`** in `wal_ast.ml` — add `| Vec exprs -> "#(" ^ ... ^ ")"`

The compiler's exhaustiveness checking means you cannot forget a case.

```ocaml
(* Also add to value type if Vec evaluates to something new *)
type value =
  (* ... existing ... *)
  | VVec of value array    (* or just reuse VList *)
```

---

### How to add a new VCD signal accessor

VCD (Value Change Dump) files record hardware signal values over time.
`wal_vcd.ml` and `wal_trace.ml` will provide access to them.

The pattern for a new accessor built-in (e.g. `(signal-at clk 100)` —
get value of signal `clk` at timestamp 100):

```ocaml
(* In wal_eval.ml or wal_trace.ml *)

(* Step 1: implement the lookup in wal_trace.ml *)
(* Wal_trace will expose something like: *)
val get_value_at : trace -> string -> int -> value option
(* trace is the parsed VCD, string is signal name, int is timestamp *)

(* Step 2: wire it up as a built-in *)
let builtin_signal_at trace args =
  match args with
  | [VSignal name; VInt timestamp] ->
    (match Wal_trace.get_value_at trace name timestamp with
     | Some v -> v
     | None   -> VNil)
  | _ -> failwith "'signal-at' expects a signal and a timestamp"

(* Step 3: register it (with trace partially applied) *)
("signal-at", builtin_signal_at loaded_trace)

(* Step 4: use from WAL *)
(* (signal-at clk 100) -> VBool true  (if clk is high at t=100) *)
```

**Adding a new signal property** (e.g. `(signal-width clk)` — bit width):

```ocaml
(* In wal_trace.ml, expose: *)
val signal_width : trace -> string -> int option

(* Built-in: *)
let builtin_signal_width trace args =
  match args with
  | [VSignal name] ->
    (match Wal_trace.signal_width trace name with
     | Some w -> VInt w
     | None   -> failwith (Printf.sprintf "unknown signal: %s" name))
  | _ -> failwith "'signal-width' expects one signal argument"
```

---

### Pattern: evaluating a WAL (+ 1 2) call

This is the core of `wal_eval.ml`. Here is the complete pattern:

```ocaml
(* evaluate : expr -> env -> value *)
let rec evaluate expr env =
  match expr with
  (* Literals evaluate to themselves *)
  | Int n    -> VInt n
  | Float f  -> VFloat f
  | Bool b   -> VBool b
  | Str s    -> VStr s
  | Nil      -> VNil

  (* Symbol lookup in environment *)
  | Symbol name -> Wal_env.lookup_exn name env

  (* Function call: (f arg1 arg2 ...) *)
  | List (func_expr :: arg_exprs) ->
    let func = evaluate func_expr env in
    let args = List.map (fun a -> evaluate a env) arg_exprs in
    apply func args env

  | List [] -> VNil

and apply func args env =
  match func with
  | VClosure { params; body; env = closure_env } ->
    (* Bind params to args in a new scope *)
    let bindings = List.combine params args in
    let new_env  = Wal_env.extend bindings closure_env in
    evaluate body new_env
  | _ -> failwith "not a function"
```

---

### Quick reference: WAL syntax -> OCaml types

```
WAL source     | OCaml expr type  | OCaml value type
---------------|------------------|-----------------
42             | Int 42           | VInt 42
3.14           | Float 3.14       | VFloat 3.14
#t             | Bool true        | VBool true
"hello"        | Str "hello"      | VStr "hello"
clk            | Symbol "clk"     | VSignal "clk" (after lookup)
()             | Nil              | VNil
(+ 1 2)        | List [Symbol "+"; Int 1; Int 2] | VInt 3 (after eval)
(lambda (x) x) | List [Symbol "lambda"; ...] | VClosure {...}
```

---

*Cheat sheet for wal-fast — Snir Bachar, April 2026.*
*Open an issue on GitHub if something is wrong or missing.*

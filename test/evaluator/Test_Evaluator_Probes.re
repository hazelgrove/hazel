open Alcotest;
open Util;
open Haz3lcore;
open Language;

/* Line-based probe testing infrastructure.
 *
 * Tests probes by line number. Assumes at most one probed term ending per line.
 *
 * Usage:
 *   probe_line_test(
 *     "test name",
 *     {|let x = ^^probe(1 + 2)
 *       in x|},
 *     [(0, ["3"])],  // Line 0 has probe with value "3"
 *   )
 *
 * Known limitations / cases not pursued:
 *
 * - Parens inside probe: ^^probe((expr)) loses the ID due to parens stripping
 *   during elaboration. Parens *outside* probe are fine.
 *
 * - TypFun: Hits `[failure] patterns should be handled separately in substitution`
 *   - a polymorphism evaluation bug, not a probe issue.
 *
 * - Fun with ascription: Function values get Fold projectors automatically
 *   applied in output. The ID preservation is in place but untested.
 *
 * - Floats: Printed representation varies (4. vs 4.0) - didn't bother
 *
 * - Test expressions: Always return unit - didn't bother
 *
 * - Multiple probed terms ending on same line: Not supported. Use multi-line
 *   constructs (lists, lets) to ensure probed terms end on different lines.
 */

/* Convert a sample value to a string for comparison */
let format_sample_value = (value: Exp.t): string => {
  let seg =
    ExpToSegment.exp_to_segment(
      ~settings={
        ...ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
        show_unknown_as_hole: false,
        hole_tiles: false,
      },
      value |> DHExp.strip_ascriptions,
    );
  let str =
    Printer.of_segment(~holes="?", ~indent="", ~is_single_line=true, seg);
  StringUtil.replace(StringUtil.regexp("\n"), str, " ");
};

/* Get probe samples organized by line number.
 * Returns a map from line number to list of formatted sample values.
 * Uses TermData to look up probe positions. */
let get_samples_by_line = (code: string): IntMap.t(list(string)) => {
  /* Parse to zipper */
  switch (Parser.to_zipper(~root=Exp, code)) {
  | None => IntMap.empty
  | Some(z) =>
    let MakeTerm.{term, term_data, _} =
      MakeTerm.from_zip_for_sem(z, ~root=Exp);
    let (info_map, elaborated) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
    let targets = Test_Evaluator_Prelude.targets_of_zipper(z, info_map);

    let (_, state) =
      Evaluator.evaluate(
        ~eval_info=EvalInfo.of_targets(targets),
        ~env=Builtins.env_init,
        elaborated,
      );
    let probes = EvaluatorState.get_probes(state);

    /* Get segment and measured for position lookup */
    let segment = Zipper.unselect_and_zip(z);
    let measured =
      Measured.of_segment(
        segment,
        ProjectorCore.Shape.Map.empty,
        Id.Map.empty,
      );

    /* Build map from line number to samples using TermData */
    Sample.Map.fold(
      (probe_id, samples, acc) => {
        switch (TermData.extreme_measures(probe_id, term_data, measured)) {
        | Some((start, _)) =>
          let line = start.row;
          let formatted_values =
            List.map(
              (s: Sample.t) => format_sample_value(s.value),
              samples,
            );
          let existing =
            IntMap.find_opt(line, acc) |> Option.value(~default=[]);
          IntMap.add(line, existing @ formatted_values, acc);
        | None => acc
        }
      },
      probes,
      IntMap.empty,
    );
  };
};

/* Main test function: check that probes on specified lines have expected values */
let probe_line_test =
    (name: string, code: string, expected: list((int, list(string)))) => {
  test_case(
    name,
    `Quick,
    () => {
      let actual_by_line = get_samples_by_line(code);

      List.iter(
        ((line, expected_values)) => {
          let actual_values =
            IntMap.find_opt(line, actual_by_line)
            |> Option.value(~default=[]);
          check(
            list(string),
            Printf.sprintf("Line %d", line),
            expected_values,
            actual_values,
          );
        },
        expected,
      );
    },
  );
};

/* Test function: check that probes on specified lines have expected sample COUNTS */
let probe_count_test =
    (name: string, code: string, expected: list((int, int))) => {
  test_case(
    name,
    `Quick,
    () => {
      let actual_by_line = get_samples_by_line(code);

      List.iter(
        ((line, expected_count)) => {
          let actual_values =
            IntMap.find_opt(line, actual_by_line)
            |> Option.value(~default=[]);
          check(
            int,
            Printf.sprintf("Line %d sample count", line),
            expected_count,
            List.length(actual_values),
          );
        },
        expected,
      );
    },
  );
};

/* ===== Test Groups ===== */

let basic_tests = [
  probe_line_test(
    "Simple arithmetic probe",
    {|let x = ^^probe(1 + 2) in x|},
    [(0, ["3"])],
  ),
  probe_line_test(
    "Probe on variable",
    {|let x = 5 in ^^probe(x)|},
    [(0, ["5"])],
  ),
  probe_line_test(
    "Probe on string",
    {|^^probe("hello")|},
    [(0, ["\"hello\""])],
  ),
  probe_line_test("Probe on boolean", {|^^probe(true)|}, [(0, ["true"])]),
  probe_line_test(
    "Probe on list",
    {|^^probe([1, 2, 3])|},
    [(0, ["[1, 2, 3]"])],
  ),
  probe_line_test("Probe on empty list", {|^^probe([])|}, [(0, ["[]"])]),
  probe_line_test(
    "Probe on empty list with ascription",
    {|^^probe([]) : [Int]|},
    [(0, ["[]"])],
  ),
  probe_line_test("Probe on empty hole", {|^^probe(?)|}, [(0, ["?"])]),
  probe_line_test(
    "Probe on constructor",
    {|type T = A + B in ^^probe(A)|},
    [(0, ["A"])],
  ),
  probe_line_test(
    "Probe on constructor with arg",
    {|type T = A(Int) + B in ^^probe(A(42))|},
    [(0, ["A(42)"])],
  ),
];

let operator_tests = [
  probe_line_test(
    "Probe on less than",
    {|^^probe(3 < 5)|},
    [(0, ["true"])],
  ),
  probe_line_test(
    "Probe on greater than",
    {|^^probe(5 > 3)|},
    [(0, ["true"])],
  ),
  probe_line_test(
    "Probe on greater than or equal",
    {|^^probe(5 >= 5)|},
    [(0, ["true"])],
  ),
  probe_line_test(
    "Probe on not equal",
    {|^^probe(3 != 5)|},
    [(0, ["true"])],
  ),
  probe_line_test(
    "Probe on integer division",
    {|^^probe(10 / 3)|},
    [(0, ["3"])],
  ),
  probe_line_test(
    "Probe on integer power",
    {|^^probe(2 ** 3)|},
    [(0, ["8"])],
  ),
  probe_line_test(
    "Probe on string concat",
    {|^^probe("hello" ++ " world")|},
    [(0, ["\"hello world\""])],
  ),
  probe_line_test(
    "Probe on boolean and",
    {|^^probe(true && false)|},
    [(0, ["false"])],
  ),
  probe_line_test(
    "Probe on boolean or",
    {|^^probe(true || false)|},
    [(0, ["true"])],
  ),
  probe_line_test(
    "Probe on boolean not",
    {|^^probe(!true)|},
    [(0, ["false"])],
  ),
  probe_line_test("Probe on unary minus", {|^^probe(-5)|}, [(0, ["-5"])]),
  probe_line_test(
    "Probe on list concat",
    {|^^probe([1, 2] @ [3, 4])|},
    [(0, ["[1, 2, 3, 4]"])],
  ),
  probe_line_test(
    "Probe on list cons",
    {|^^probe(1 :: [2, 3])|},
    [(0, ["[1, 2, 3]"])],
  ),
  probe_line_test(
    "Probe on builtin function call",
    {|^^probe(string_length("hello"))|},
    [(0, ["5"])],
  ),
  probe_line_test(
    "Probe on dot projection",
    {|let t = (a=1, b=2) in ^^probe(t.a)|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Probe on dot projection computation",
    {|let t = (x=10, y=20) in ^^probe(t.x + t.y)|},
    [(0, ["30"])],
  ),
];

let compound_expression_tests = [
  probe_line_test("Probe on parens", {|^^probe((1 + 2))|}, [(0, ["3"])]),
  probe_line_test(
    "Probe on parenthesized tuple",
    {|^^probe((1, "a"))|},
    [(0, ["(1, \"a\")"])],
  ),
  probe_line_test(
    "Probe in if-then branch (taken)",
    {|if true then ^^probe(1) else 2|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Probe in if-else branch (taken)",
    {|if false then 1 else ^^probe(2)|},
    [(0, ["2"])],
  ),
  probe_line_test(
    "Probe in if-then branch (not taken)",
    {|if false then ^^probe(1) else 2|},
    [(0, [])],
  ),
  probe_line_test(
    "Probe in case branch",
    {|case 1 | 1 => ^^probe(10) | _ => 20 end|},
    [(0, ["10"])],
  ),
  probe_line_test(
    "Probe on if-then-else",
    {|^^probe(if true then 1 else 2)|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Probe on if-then-else (else branch)",
    {|^^probe(if false then 1 else 2)|},
    [(0, ["2"])],
  ),
  probe_line_test(
    "Probe on let expression",
    {|^^probe(let x = 1 in x)|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Probe on let with computation",
    {|^^probe(let x = 1 + 2 in x * 3)|},
    [(0, ["9"])],
  ),
  probe_line_test(
    "Probe on case expression",
    {|^^probe(case 1 | 1 => 10 | _ => 20 end)|},
    [(0, ["10"])],
  ),
  probe_line_test(
    "Probe on case (second branch)",
    {|^^probe(case 2 | 1 => 10 | _ => 20 end)|},
    [(0, ["20"])],
  ),
  probe_line_test("Probe on sequence", {|^^probe(1; 2)|}, [(0, ["2"])]),
  probe_line_test(
    "Probe on nested ifs",
    {|^^probe(if true then (if false then 1 else 2) else 3)|},
    [(0, ["2"])],
  ),
  probe_line_test(
    "Probe on nested lets",
    {|^^probe(let x = 1 in let y = 2 in x + y)|},
    [(0, ["3"])],
  ),
  probe_line_test(
    "Probe on if containing let",
    {|^^probe(if true then let x = 5 in x else 0)|},
    [(0, ["5"])],
  ),
  probe_line_test(
    "Probe on let containing if",
    {|^^probe(let x = if true then 1 else 2 in x + 10)|},
    [(0, ["11"])],
  ),
  probe_line_test(
    "Probe on deeply nested compound",
    {|^^probe(let a = 1 in
  let b = if a == 1 then 10 else 20 in
  case b | 10 => 100 | _ => 200 end)|},
    [(0, ["100"])],
  ),
  probe_line_test(
    "Case inside let inside if",
    {|let x = 1
in if x == 1
then let y = case x | 1 => ^^probe(10) | _ => 0 end
  in y * 2
else 0|},
    [(2, ["10"])],
  ),
  probe_line_test(
    "Multiple nested lets with probes",
    {|let a = ^^probe(1 + 1)
in let b = ^^probe(a + 1)
in let c = ^^probe(b + 1)
in ^^probe(c + 1)|},
    [(0, ["2"]), (1, ["3"]), (2, ["4"]), (3, ["5"])],
  ),
  probe_line_test(
    "Multiple probes on different lines",
    {|let x = ^^probe(1)
in let y = ^^probe(2)
in x + y|},
    [(0, ["1"]), (1, ["2"])],
  ),
  probe_line_test(
    "Probes in both if branches",
    {|let x = true
in if x then ^^probe(1) else ^^probe(2)|},
    [(1, ["1"])],
  ),
  probe_line_test(
    "Outer probe on if with inner probe",
    {|^^probe(if true then ^^probe(1 + 2) else 0)|},
    [(0, ["3", "3"])],
  ),
  probe_line_test(
    "Outer probe on let with inner probe on body",
    {|^^probe(let x = 5 in ^^probe(x * 2))|},
    [(0, ["10", "10"])],
  ),
  probe_line_test(
    "Probe on function application",
    {|let f = fun x -> x + 1 in ^^probe(f(5))|},
    [(0, ["6"])],
  ),
  probe_line_test(
    "Probe in map function",
    {|let double = fun x -> ^^probe(x * 2)
in [double(1), double(2), double(3)]|},
    [(0, ["2", "4", "6"])],
  ),
  probe_line_test(
    "Probe on closure result",
    {|let make_adder = fun n -> fun x -> x + n
in let add5 = make_adder(5)
in ^^probe(add5(10))|},
    [(2, ["15"])],
  ),
  /* Tests for case/if inside function body - issue with no samples */
  probe_line_test(
    "Probe on case inside called function",
    {|let f = fun x -> ^^probe(case x | 1 => 10 | _ => 20 end)
in f(1)|},
    [(0, ["10"])],
  ),
  probe_line_test(
    "Probe on if inside called function",
    {|let f = fun x -> ^^probe(if x == 1 then 10 else 20)
in f(1)|},
    [(0, ["10"])],
  ),
  probe_line_test(
    "Probe on case with constructor pattern inside function",
    {|type T = + A(Int) in
let f = fun t -> ^^probe(case t | A(x) => x end)
in f(A(42))|},
    [(1, ["42"])],
  ),
  /* Integration tests - complex real-world scenarios (originally from user bug report) */
  probe_line_test(
    "Probe on case containing if inside multiarg function with type annotation",
    {|type Exp = + Var(String) in
let subst: (Exp, String, Exp) -> Exp =
fun (v, name, e) ->
^^probe(case e
| Var(x) =>
if x == name then v else e
end)
in subst(Var("foo"), "foo", Var("bar"))|},
    /* x="bar", name="foo", so x==name is false, result is e=Var("bar") */
    [(3, ["Var(\"bar\")"])],
  ),
  probe_line_test(
    "Probe on if inside case inside function with type annotation",
    {|type Exp = + Var(String) in
let subst: (Exp, String, Exp) -> Exp =
fun (v, name, e) ->
case e
| Var(x) =>
^^probe(if x == name then v else e)
end
in subst(Var("foo"), "foo", Var("bar"))|},
    /* x="bar", name="foo", so x==name is false, result is e=Var("bar") */
    [(5, ["Var(\"bar\")"])],
  ),
  /* Case inside function (without type annotation for contrast) */
  probe_line_test(
    "Probe inside case inside simple function (no annotation)",
    {|let f = fun x -> case x | 1 => ^^probe(10) | _ => 0 end
in f(1)|},
    [(0, ["10"])],
  ),
  probe_line_test(
    "Probe inside case with constructor pattern (no annotation)",
    {|type T = + A(Int) in
let f = fun t -> case t | A(x) => ^^probe(x + 1) end
in f(A(5))|},
    [(1, ["6"])],
  ),
];

let pattern_probe_tests = [
  probe_line_test(
    "Pattern probe on let binding",
    {|let ^^probe(x) = 42 in x|},
    [(0, ["42"])],
  ),
  probe_line_test(
    "Pattern probe on tuple destructuring",
    {|let (^^probe(a), b) = (1, 2) in a + b|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Pattern probe in function parameter",
    {|let f = fun ^^probe(x) -> x * 2
in f(5)|},
    [(0, ["5"])],
  ),
  probe_line_test(
    "Pattern probe in case",
    {|case (1, 2) | (^^probe(x), y) => x + y end|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Pattern probe on cons pattern",
    {|case [1, 2, 3] | ^^probe(x) :: xs => x | [] => 0 end|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Pattern probe on cons pattern tail",
    {|case [1, 2, 3] | x :: ^^probe(xs) => xs | [] => [] end|},
    [(0, ["[2, 3]"])],
  ),
  probe_line_test(
    "Pattern probe on list literal pattern",
    {|case [1, 2, 3] | [^^probe(a), b, c] => a | _ => 0 end|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Pattern probe on constructor pattern",
    {|type T = Some(Int) + None
in case Some(42) | Some(^^probe(x)) => x | None => 0 end|},
    [(1, ["42"])],
  ),
  probe_line_test(
    "Pattern probe on nested tuple pattern",
    {|case ((1, 2), 3) | ((^^probe(a), b), c) => a + b + c end|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Multiple pattern probes in same case",
    {|case (1, 2) | (^^probe(a),
    ^^probe(b)) => a + b end|},
    [(0, ["1"]), (1, ["2"])],
  ),
  probe_line_test(
    "Pattern probe on wildcard with value",
    {|case 42 | ^^probe(_) => 1 end|},
    [(0, ["42"])],
  ),
  probe_line_test(
    "Pattern probe with labeled tuple type (value coercion)",
    {|let ^^probe(x) : (l=String) = "a" in x|},
    [(0, ["(l=\"a\")"])],
  ),
];

let nested_probe_tests = [
  probe_line_test(
    "Nested probes in list",
    {|^^probe([
  ^^probe(1 + 2),
  ^^probe(3 + 4)
])|},
    [(0, ["[3, 7]"]), (1, ["3"]), (2, ["7"])],
  ),
  probe_line_test(
    "Nested probe with let on separate lines",
    {|let a = ^^probe(1 + 2)
in let b = ^^probe(a * 2)
in ^^probe(b + 1)|},
    [(0, ["3"]), (1, ["6"]), (2, ["7"])],
  ),
  probe_line_test(
    "Deeply nested probes via list",
    {|^^probe([
  ^^probe([
    ^^probe(1),
    2
  ]),
  3
])|},
    [(2, ["1"]), (1, ["[1, 2]"]), (0, ["[[1, 2], 3]"])],
  ),
];

let recursion_tests = [
  probe_line_test(
    "Factorial recursive probe",
    {|let fact = fun n ->
  if n <= 1 then 1
  else ^^probe(n) * fact(n - 1)
in fact(5)|},
    [(2, ["5", "4", "3", "2"])],
  ),
  probe_line_test(
    "Recursive sum with multiple probes",
    {|let sum = fun n ->
  if n <= 0 then ^^probe(0)
  else ^^probe(n) + sum(n - 1)
in sum(3)|},
    [(1, ["0"]), (2, ["3", "2", "1"])],
  ),
  probe_line_test(
    "User-defined list ADT with length",
    {|type List = Nil + Cons(Int, List)
in let len = fun lst ->
  case lst
  | Nil => 0
  | Cons(_, tail) => 1 + ^^probe(len(tail))
  end
in len(Cons(1, Cons(2, Cons(3, Nil))))|},
    [(4, ["0", "1", "2"])],
  ),
  probe_line_test(
    "Tree ADT with sum",
    {|type Tree = Leaf(Int) + Node(Tree, Tree)
in let sum = fun t ->
  case t
  | Leaf(n) => ^^probe(n)
  | Node(l, r) => sum(l) + sum(r)
  end
in sum(Node(Leaf(1), Node(Leaf(2), Leaf(3))))|},
    [(3, ["1", "2", "3"])],
  ),
  probe_line_test(
    "Tree ADT with multiple probes",
    {|type Tree = Leaf(Int) + Node(Tree, Tree)
in let depth = fun t ->
  case t
  | Leaf(^^probe(n)) => 1
  | Node(l, r) =>
    let dl = ^^probe(depth(l))
    in let dr = depth(r)
    in 1 + (if dl > dr then dl else dr)
  end
in depth(Node(Node(Leaf(1), Leaf(2)), Leaf(3)))|},
    [(3, ["1", "2", "3"]), (5, ["1", "2"])],
  ),
  probe_line_test(
    "List reverse returning list",
    {|let rev = fun lst ->
  case lst
  | [] => []
  | x :: xs => ^^probe(rev(xs)) @ [x]
  end
in rev([1, 2, 3])|},
    [(3, ["[]", "[3]", "[3, 2]"])],
  ),
  probe_line_test(
    "List map with probe on each element",
    {|let map = fun f -> fun lst ->
  case lst
  | [] => []
  | x :: xs => ^^probe(f(x)) :: map(f)(xs)
  end
in map(fun x -> x * 2)([1, 2, 3])|},
    [(3, ["2", "4", "6"])],
  ),
  probe_line_test(
    "Nested recursion with probes (fib)",
    {|let fib = fun n ->
  if n <= 1 then n
  else ^^probe(fib(n - 1)) + fib(n - 2)
in fib(5)|},
    [(2, ["1", "1", "2", "1", "3", "1", "1"])],
  ),
  /* Tests with non-atomic outputs (using ADTs since tuple probes have parsing issues) */
  probe_line_test(
    "Probe on ADT constructor",
    {|type T = A(Int) + B in ^^probe(A(42))|},
    [(0, ["A(42)"])],
  ),
  probe_line_test(
    "Recursive ADT construction",
    {|type List = Nil + Cons(Int, List)
in let build = fun n ->
  if n <= 0 then ^^probe(Nil)
  else ^^probe(Cons(n, build(n - 1)))
in build(3)|},
    [
      (2, ["Nil"]),
      (
        3,
        [
          "Cons(1, Nil)",
          "Cons(2, Cons(1, Nil))",
          "Cons(3, Cons(2, Cons(1, Nil)))",
        ],
      ),
    ],
  ),
];

let ascription_tests = [
  /* Basic ascriptions with unknown type (?) */
  probe_line_test(
    "Probe with unknown type ascription",
    {|^^probe(1 + 2) : ?|},
    [(0, ["3"])],
  ),
  probe_line_test(
    "Probe on literal with unknown type",
    {|^^probe(42) : ?|},
    [(0, ["42"])],
  ),
  probe_line_test(
    "Probe on list with unknown element type",
    {|^^probe([1, 2, 3]) : [?]|},
    [(0, ["[1, 2, 3]"])],
  ),
  probe_line_test(
    "Probe on function result with unknown type",
    {|let f: Int -> ? = fun x -> x + 1 in ^^probe(f(5))|},
    [(0, ["6"])],
  ),
  /* Function type annotations - critical for ID preservation */
  probe_line_test(
    "Probe in function body with type annotation",
    {|let f: Int -> Int = fun x -> ^^probe(x + 1)
in f(5)|},
    [(0, ["6"])],
  ),
  probe_line_test(
    "Probe inside case with function type annotation",
    {|let f: Int -> Int = fun x -> case x | 1 => ^^probe(10) | _ => 0 end
in f(1)|},
    [(0, ["10"])],
  ),
  probe_line_test(
    "Probe inside if with function type annotation",
    {|let f: Int -> Int = fun x -> if x == 1 then ^^probe(100) else 0
in f(1)|},
    [(0, ["100"])],
  ),
  probe_line_test(
    "Probe with tuple param and type annotation",
    {|let f: (Int, Int) -> Int = fun (a, b) -> ^^probe(a + b)
in f(3, 4)|},
    [(0, ["7"])],
  ),
  probe_line_test(
    "Probe inside case with custom type annotation",
    {|type T = + A(Int) in
let f: T -> Int = fun t -> case t | A(x) => ^^probe(x * 2) end
in f(A(5))|},
    [(1, ["10"])],
  ),
  /* Function with unknown return type */
  probe_line_test(
    "Probe in function with unknown return type",
    {|let f: Int -> ? = fun x -> ^^probe(x + 1)
in f(5)|},
    [(0, ["6"])],
  ),
  probe_line_test(
    "Probe inside case with unknown return type",
    {|let f: Int -> ? = fun x -> case x | 1 => ^^probe(10) | _ => 0 end
in f(1)|},
    [(0, ["10"])],
  ),
  /* Value coercions via ascription */
  probe_line_test(
    "Probe in let with labeled tuple type (value coercion)",
    {|let x : (l=String) = ^^probe("a") in x|},
    [(0, ["(l=\"a\")"])],
  ),
  probe_line_test(
    "Probe with outer ascription (singleton tuple coercion)",
    {|^^probe("a") : (l=String)|},
    [(0, ["(l=\"a\")"])],
  ),
  /* Compound expressions with ascriptions */
  probe_line_test(
    "Probe on list literal with ascription",
    {|^^probe([1, 2]) : [Int]|},
    [(0, ["[1, 2]"])],
  ),
  probe_line_test(
    "Probe on list concat with ascription",
    {|^^probe([1] @ [2]) : [Int]|},
    [(0, ["[1, 2]"])],
  ),
  probe_line_test(
    "Probe on if expression with ascription",
    {|^^probe(if true then 1 else 2) : Int|},
    [(0, ["1"])],
  ),
  probe_line_test(
    "Probe on case expression with ascription",
    {|^^probe(case 1 | 1 => 10 | _ => 20 end) : Int|},
    [(0, ["10"])],
  ),
  probe_line_test(
    "Probe on let expression with ascription",
    {|^^probe(let x = 1 in x + 1) : Int|},
    [(0, ["2"])],
  ),
  probe_line_test(
    "Probe on sequence with ascription",
    {|^^probe(1; 2) : Int|},
    [(0, ["2"])],
  ),
  /* Ascription distribution through typed function must preserve probe
   * Projector ID. Without fast_copy, the Projector case in
   * Ascriptions.transition gives the probe wrapper a fresh ID,
   * losing the probe target and producing zero samples. */
  probe_line_test(
    "Probe on let in typed function body",
    {|let f: ? -> Bool = fun _ -> ^^probe(let _ = 1 in true)
in f(4)|},
    [(0, ["true"])],
  ),
  probe_line_test(
    "Probe on seq in typed function body",
    {|let f: ? -> Int = fun _ -> ^^probe(1; 2)
in f(4)|},
    [(0, ["2"])],
  ),
  /* Ascription distribution through ConstructorAp must preserve probe ID.
   * Without fast_copy, the ConstructorAp case in Ascriptions.transition
   * creates a fresh Ap ID, losing the probe target. */
  probe_line_test(
    "Probe on constructor Ap in typed function body",
    {|type R = Ok(Int) + Err(String) in
let f: Int -> R = fun x -> ^^probe(Ok(x))
in f(42)|},
    [(1, ["Ok(42)"])],
  ),
  probe_line_test(
    "Probe on constructor Ap with string payload in typed function",
    {|type R = Ok(Int) + Err(String) in
let f: Int -> R = fun _ -> ^^probe(Err("bad"))
in f(0)|},
    [(1, ["Err(\"bad\")"])],
  ),
  /* ListConcat ascription distribution must preserve probe ID.
   * Without fast_copy, the ListConcat case creates a fresh ID. */
  probe_line_test(
    "Probe on list concat in typed function body",
    {|let f: [Int] -> [Int] = fun xs -> ^^probe(xs @ [0])
in f([1, 2])|},
    [(0, ["[1, 2, 0]"])],
  ),
];

/* Tests that probe samples are not duplicated when values flow through
 * typed functions or are projected with dot access. Two independent
 * mechanisms caused duplicates:
 * 1. Dot projection with is_value:false re-evaluated already-final values
 * 2. Ascription distribution through typed functions re-evaluated inner
 *    compound elements at a deeper call_stack */
let duplicate_prevention_tests = [
  /* Dot projection: extracting from a probed tuple must not duplicate.
   * Without is_value:true on the Dot transition, the projected value
   * gets re-evaluated, recording a second sample at the same call_stack. */
  probe_line_test(
    "Dot on probed labeled tuple field",
    {|let x = (a = ^^probe(1),
b = ^^probe(2)) in x.a|},
    [(0, ["1"]), (1, ["2"])],
  ),
  /* Typed identity function with labeled tuple.
   * Without Asc distribution dedup, each field gets a duplicate sample
   * at the function's call_stack when the ascription distributes. */
  probe_line_test(
    "Probed labeled tuple through typed identity",
    {|let x = (a = ^^probe(1),
b = ^^probe(2)) in
let f: (a = Int, b = Int) -> (a = Int, b = Int) =
  fun t -> t
in
f(x)|},
    [(0, ["1"]), (1, ["2"])],
  ),
  /* User-suggested: unlabeled tuple through typed identity with ? types */
  probe_line_test(
    "Probed unlabeled tuple through typed identity with holes",
    {|let f: ? -> (?, ?) = fun t -> t in
f((?, ^^probe(1)))|},
    [(1, ["1"])],
  ),
  /* User-suggested: labeled tuple through typed identity with ? types */
  probe_line_test(
    "Probed labeled tuple through typed identity with holes",
    {|let f: ? -> (a = ?, b = ?) = fun t -> t in
f((a = ?, b = ^^probe(1)))|},
    [(1, ["1"])],
  ),
  /* User-suggested: list through typed identity with ? types */
  probe_line_test(
    "Probed list through typed identity with holes",
    {|let f: ? -> [?] = fun t -> t in
f([?, ^^probe(1)])|},
    [(1, ["1"])],
  ),
  /* Chained typed functions: value passes through two Asc distributions */
  probe_line_test(
    "Probed tuple through two chained typed functions",
    {|let x = (^^probe(1),
^^probe(2)) in
let f: (Int, Int) -> (Int, Int) = fun t -> t in
let g: (Int, Int) -> (Int, Int) = fun t -> t in
g(f(x))|},
    [(0, ["1"]), (1, ["2"])],
  ),
  /* Multi-call: legitimate multiple samples must still work */
  probe_line_test(
    "Multi-call function probe still produces multiple samples",
    {|let f: Int -> Int = fun x -> ^^probe(x) in
[f(1), f(2), f(3)]|},
    [(0, ["1", "2", "3"])],
  ),
  /* Unbound variable: should produce exactly one sample, not two */
  probe_line_test(
    "Probe on unbound variable produces single sample",
    {|^^probe(x)|},
    [(0, ["x"])],
  ),
  /* Function value at top level: wrap_closure_when_done wraps Fun in
   * Closure, causing re-evaluation of inner expression with same target ID */
  probe_line_test(
    "Probe on function value produces single sample",
    {|^^probe(fun x -> x)|},
    [(0, ["^^fold(fun x -> x)"])],
  ),
  /* Indet case with no rules: wrap_closure_when_done wraps the indet
   * Match in Closure, causing re-evaluation with same target ID */
  probe_line_test(
    "Probe on case with no rules produces single sample",
    {|^^probe(case 1 end)|},
    [(0, ["case 1 end"])],
  ),
  /* wrap_closure re-evaluation bug: when a recursive function returns an
   * indeterminate value (e.g. if with hole condition), and the caller's
   * let-destructuring fails (IndetMatch), wrap_closure_when_done wraps in
   * Closure and re-evaluates. The re-evaluation traverses into the return
   * value from the deeper call, re-evaluating probed sub-expressions at
   * the wrong call stack. This produces spurious samples.
   *
   * Minimal case: partition_at([5, 10], 7)
   * - partition_at([10], 7): let succeeds, evaluates `10 < ?` → 1 sample
   * - partition_at([5, 10], 7): let fails (IndetMatch), `5 < ?` never
   *   evaluated → should be 0 samples at this level
   * Total expected: 1 sample for `hd < ?`. Bug produces 2. */
  probe_count_test(
    "Recursive indet: no spurious samples from wrap_closure (depth 2)",
    {|let f : ([Int], Int) -> ([Int], [Int]) =
  fun (xs, x) ->
    case xs
    | [] => ([], [])
    | hd::tl =>
      let (s, b) = f(tl, x) in
      if ^^probe(hd < ?)
      then ?
      else ?
    end
in f([5, 10], 7)|},
    [(6, 1)],
  ),
  /* Same bug at depth 4: partition_at called with [3, 9, 5, 10].
   * Only the deepest non-empty call ([10]) reaches the if condition.
   * Expected: 1 sample. Bug produces 4 (one per recursion level). */
  probe_count_test(
    "Recursive indet: no spurious samples from wrap_closure (depth 4)",
    {|let f : ([Int], Int) -> ([Int], [Int]) =
  fun (xs, x) ->
    case xs
    | [] => ([], [])
    | hd::tl =>
      let (s, b) = f(tl, x) in
      if ^^probe(hd < ?)
      then ?
      else ?
    end
in f([3, 9, 5, 10], 7)|},
    [(6, 1)],
  ),
  /* Full quicksort example: two test chains, each with one deepest call
   * that reaches the if condition. Expected: 2 samples total. */
  probe_count_test(
    "Quicksort recursive indet: correct sample count",
    {|let partition_at : ([Int], Int) -> ([Int], [Int]) =
  fun (xs, x) ->
    case xs
    | [] => ([], [])
    | hd::tl =>
      let (s, b) = partition_at(tl, x) in
      if ^^probe(hd < ?)
      then ?
      else ?
    end
in
let quicksort : [Int] -> [Int] =
  fun xs ->
    case xs
    | [] => []
    | hd::tl =>
      let (s, b) = partition_at(tl, hd) in
      ?
    end
in
let _ = quicksort([7, 3, 9, 5, 10])
in quicksort([5, 0, 9, 3, 1])|},
    [(6, 2)],
  ),
];

let module_tests = [
  probe_line_test(
    "Probe inside module body",
    {|let m = { let x = ^^probe(1 + 2) } in m.x|},
    [(0, ["3"])],
  ),
  /* ^^probe(m) captures the module's value, a module literal. */
  probe_line_test(
    "Probe on module value via variable",
    {|let m = { let x = 5 } in ^^probe(m)|},
    [(0, ["{ let x = 5 }"])],
  ),
  probe_line_test(
    "Probe on module dot access",
    {|let m = { let x = 5 } in ^^probe(m.x)|},
    [(0, ["5"])],
  ),
  probe_line_test(
    "Module keyword with probe in body",
    {|module m = { let x = ^^probe(2 + 3) } in m.x|},
    [(0, ["5"])],
  ),
  probe_line_test(
    "Probe on function defined in module",
    {|let m = { let f = fun x -> ^^probe(x + 1) }
in m.f(5)|},
    [(0, ["6"])],
  ),
  probe_line_test(
    "Probe on module with multiple bindings",
    {|let m = {
  let x = ^^probe(1 + 2);
  let y = ^^probe(3 + 4)
} in m.x + m.y|},
    [(1, ["3"]), (2, ["7"])],
  ),
  probe_line_test(
    "Probe on module expression collects sample",
    {|let m = ^^probe({ let x = 5 }) in m.x|},
    [(0, ["{ let x = 5 }"])],
  ),
  probe_line_test(
    "Probe inside a sealed module body fires",
    {|module M : { let x : Int } = { let x = ^^probe(1 + 2); let y = 3 } in M.x|},
    [(0, ["3"])],
  ),
  probe_line_test(
    "Probe on a sealed module shows the sealed value",
    {|module M : { let x : Int } = { let x = 1; let y = 2 } in ^^probe(M)|},
    [(0, ["{ let x = 1 }"])],
  ),
  probe_line_test(
    "Probe inside a sealed-away member still fires",
    {|module M : { let x : Int } = { let x = 1; let y = ^^probe(2) } in M.x|},
    [(0, ["2"])],
  ),
  probe_line_test(
    "Probed literal member samples once",
    {|let m = { let y = ^^probe(2); type T = Int } in m.y|},
    [(0, ["2"])],
  ),
  probe_line_test(
    "Probed literal member next to a function member samples once",
    {|let m = { let y = ^^probe(2); let f = fun x -> x } in m.f(m.y)|},
    [(0, ["2"])],
  ),
];

let spread_tests = [
  /* Spread operator inside function - single call */
  probe_line_test(
    "Probe inside spread operator",
    {|let f = fun m -> m...(a= ^^probe(0))
in f((a= 1, b= 2))|},
    [(0, ["0"])],
  ),
  /* Spread operator - multiple calls, check sample count */
  probe_count_test(
    "Spread probe count matches call count (3 calls)",
    {|let f = fun m -> m...(a= ^^probe(0))
in let r1 = f((a= 1, b= 2))
in let r2 = f(r1)
in f(r2)|},
    [(0, 3)],
  ),
  /* Spread operator with multiple probed fields */
  probe_count_test(
    "Multiple spread probes each get correct count",
    {|let f = fun m ->
m...(a= ^^probe(m.a + 1),
b= ^^probe(0))
in let r1 = f((a= 1, b= 2))
in let r2 = f(r1)
in f(r2)|},
    [(1, 3), (2, 3)],
  ),
  /* Spread operator inside case arm with typed function */
  probe_count_test(
    "Spread probe inside case arm (5 calls)",
    {|type M = (a= Int, b= Int) in
type Action = + Go + Stop in
let update : (M, Action) -> M =
fun (m, action) ->
case action
| Go =>
m...(a= ^^probe(m.a + 1),
b= ^^probe(0))
| Stop => m
end
in let init : M = (a= 1, b= 2)
in let r1 = update(init, Go)
in let r2 = update(r1, Go)
in let r3 = update(r2, Go)
in let r4 = update(r3, Go)
in update(r4, Go)|},
    [(6, 5), (7, 5)],
  ),
  /* Spread with non-tuple e2 (type error) - should still count correctly */
  probe_count_test(
    "Spread probe with non-tuple extension (type error case)",
    {|let f = fun m -> m...^^probe(1)
in let r1 = f((a= 1, b= 2))
in let r2 = f(r1)
in f(r2)|},
    [(0, 3)],
  ),
  /* Spread inside let inside case arm */
  probe_count_test(
    "Spread with let inside case arm",
    {|type M = (a= Int, b= Int) in
type Action = + Go in
let update : (M, Action) -> M =
fun (m, action) ->
case action
| Go =>
let x = m.a + 1 in
m...(a= ^^probe(x),
b= ^^probe(0))
end
in let init : M = (a= 1, b= 2)
in let r1 = update(init, Go)
in let r2 = update(r1, Go)
in update(r2, Go)|},
    [(7, 3), (8, 3)],
  ),
  /* Nested return type annotations: when g's return type Asc distributes
   * through f's return value, the inner probed values should NOT be
   * re-evaluated. Without the fix, the Asc distribution re-evaluates probed
   * values at a different (shorter) call stack, bypassing dedup. */
  probe_count_test(
    "Nested return type annotations: labeled tuple",
    {|let f : ? -> (a=Int) =
  fun _ -> (a= ^^probe(1))
in
let g : ? -> (a=Int) = fun _ -> f(0)
in
g(0)|},
    [(1, 1)],
  ),
  probe_count_test(
    "Nested return type annotations: list",
    {|let f : ? -> [Int] =
  fun _ -> [^^probe(1)]
in
let g : ? -> [Int] = fun _ -> f(0)
in
g(0)|},
    [(1, 1)],
  ),
  probe_count_test(
    "Nested return type annotations: constructor payload",
    {|type T = + A(Int) in
let f : ? -> T =
  fun _ -> A(^^probe(1))
in
let g : ? -> T = fun _ -> f(0)
in
g(0)|},
    [(2, 1)],
  ),
  probe_count_test(
    "Nested return type annotations: triple nesting",
    {|let f : ? -> (a=Int) =
  fun _ -> (a= ^^probe(1))
in
let g : ? -> (a=Int) = fun _ -> f(0)
in
let h : ? -> (a=Int) = fun _ -> g(0)
in
h(0)|},
    [(1, 1)],
  ),
  probe_count_test(
    "Nested return type annotations: multiple calls",
    {|let f : ? -> (a=Int) =
  fun _ -> (a= ^^probe(1))
in
let g : ? -> (a=Int) = fun _ -> f(0)
in
let _ = g(0) in
let _ = g(0) in
g(0)|},
    [(1, 3)],
  ),
  /* Recursive function with return type: each recursive call should
   * produce exactly one sample, not be doubled by Asc re-distribution. */
  probe_count_test(
    "Recursive function with return type annotation",
    {|let count : Int -> [Int] =
  fun n ->
    if n <= 0 then []
    else ^^probe(n) :: count(n - 1)
in
count(3)|},
    [(3, 3)],
  ),
];

/* Regression tests for hazelgrove/hazel#2264: probes on applications of
   builtin tuple operations (those with custom_statics) used to produce zero
   samples because mk_builtin_ap_elab built the elab Ap with a fresh id,
   so the runtime target lookup never matched the user_term id keyed in
   the info_map. */
let custom_statics_tuple_op_tests = [
  /* Full Ap: to_lvs */
  probe_count_test(
    "Probe on to_lvs full application",
    {|let t = (a=1, b=2) in ^^probe(to_lvs(t))|},
    [(0, 1)],
  ),
  /* Full Ap: omit_all_labels */
  probe_count_test(
    "Probe on omit_all_labels full application",
    {|let t = (a=1, b=2) in ^^probe(omit_all_labels(t))|},
    [(0, 1)],
  ),
  /* Full Ap: project_labels (uses handle_tuple_operation) */
  probe_count_test(
    "Probe on project_labels full application",
    {|let t = (a=1, b=2) in ^^probe(project_labels(t, a))|},
    [(0, 1)],
  ),
  /* Full Ap: select_labels (uses handle_tuple_operation) */
  probe_count_test(
    "Probe on select_labels full application",
    {|let t = (a=1, b=2) in ^^probe(select_labels(t, a))|},
    [(0, 1)],
  ),
  /* Full Ap: omit_labels (uses handle_tuple_operation) */
  probe_count_test(
    "Probe on omit_labels full application",
    {|let t = (a=1, b=2) in ^^probe(omit_labels(t, a))|},
    [(0, 1)],
  ),
  /* Full Ap: group_by_label */
  probe_count_test(
    "Probe on group_by_label full application",
    {|let rows = [(k="x", v=1), (k="x", v=2), (k="y", v=3)]
in ^^probe(group_by_label(rows, k))|},
    [(1, 1)],
  ),
  /* Multi-call: each invocation should record its own sample. */
  probe_count_test(
    "Probe on to_lvs in function called multiple times",
    {|let f = fun t -> ^^probe(to_lvs(t))
in let _ = f((a=1, b=2))
in let _ = f((a=3, b=4))
in f((a=5, b=6))|},
    [(0, 3)],
  ),
];

let tests = [
  ("Evaluator.Probes.Basic", basic_tests),
  ("Evaluator.Probes.Operators", operator_tests),
  ("Evaluator.Probes.Compound", compound_expression_tests),
  ("Evaluator.Probes.Patterns", pattern_probe_tests),
  ("Evaluator.Probes.Nested", nested_probe_tests),
  ("Evaluator.Probes.Recursion", recursion_tests),
  ("Evaluator.Probes.Ascription", ascription_tests),
  ("Evaluator.Probes.DuplicatePrevention", duplicate_prevention_tests),
  ("Evaluator.Probes.Modules", module_tests),
  ("Evaluator.Probes.Spread", spread_tests),
  ("Evaluator.Probes.CustomStaticsTupleOps", custom_statics_tuple_op_tests),
];

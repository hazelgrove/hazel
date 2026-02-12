open Alcotest;
open Test_Evaluator_Prelude;

/* Module dynamics tests - Phase 1 implementation */

/* Test empty module evaluates to empty tuple */
let test_empty_module =
  test_case("Empty module evaluates to unit", `Quick, () => {
    parse_and_evaluate_test("()", {|{}|})
  });

/* Test single binding module */
let test_single_binding =
  test_case("Single binding module", `Quick, () => {
    parse_and_evaluate_test("(x=1)", {|{ let x = 1 }|})
  });

/* Test multiple bindings module */
let test_multiple_bindings =
  test_case("Multiple bindings module", `Quick, () => {
    parse_and_evaluate_test(
      {|(x=1, y="hello")|},
      {|{ let x = 1; let y = "hello" }|},
    )
  });

/* Test shadowing - only last value exported */
let test_shadowing =
  test_case("Shadowed binding exports last value", `Quick, () => {
    parse_and_evaluate_test(
      {|(x="hello")|},
      {|{ let x = 1; let x = "hello" }|},
    )
  });

/* Test accessing module binding via dot */
let test_module_access =
  test_case("Access module binding", `Quick, () => {
    parse_and_evaluate_test("1", {|{ let x = 1 }.x|})
  });

/* Test module used as labeled tuple */
let test_module_as_tuple =
  test_case("Module as labeled tuple", `Quick, () => {
    parse_and_evaluate_test(
      "3",
      {|let point = { let x = 1; let y = 2 } in point.x + point.y|},
    )
  });

/* Test bare expression side effect */
let test_bare_expression =
  test_case("Bare expression evaluated", `Quick, () => {
    parse_and_evaluate_test("(x=2)", {|{ 1 + 1; let x = 2 }|})
  });

/* Test module with function binding */
let test_module_with_function =
  test_case("Module with function binding", `Quick, () => {
    parse_and_evaluate_test(
      "11",
      {|{ let f = fun x -> x + 1; let result = f(10) }.result|},
    )
  });

/* Test module bindings can refer to earlier bindings */
let test_sequential_bindings =
  test_case("Sequential bindings can refer to earlier ones", `Quick, () => {
    parse_and_evaluate_test("(x=1, y=2)", {|{ let x = 1; let y = x + 1 }|})
  });

/* Test type alias in module */
let test_type_alias =
  test_case("Type alias in module", `Quick, () => {
    parse_and_evaluate_test("(x=42)", {|{ type T = Int; let x = 42 : T }|})
  });

let tests = (
  "Evaluator.Modules",
  [
    test_empty_module,
    test_single_binding,
    test_multiple_bindings,
    test_shadowing,
    test_module_access,
    test_module_as_tuple,
    test_bare_expression,
    test_module_with_function,
    test_sequential_bindings,
    test_type_alias,
    test_case("Nested modules", `Quick, () => {
      parse_and_evaluate_test("(m=(y=1))", {|{ let m = { let y = 1 } }|})
    }),
  ],
);

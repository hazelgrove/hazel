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

/* ===== MODULE KEYWORD TESTS ===== */

/* Test module keyword with lowercase name in exp context */
let test_module_keyword_lowercase =
  test_case("Module keyword with lowercase name", `Quick, () => {
    parse_and_evaluate_test("1", {|module m = { let x = 1 } in m.x|})
  });

/* Test module keyword with capitalized name in exp context */
let test_module_keyword_capitalized =
  test_case("Module keyword with capitalized name", `Quick, () => {
    parse_and_evaluate_test(
      "3",
      {|module M = { let x = 1; let y = 2 } in M.x + M.y|},
    )
  });

/* Test module keyword inside module body */
let test_module_keyword_in_mod =
  test_case("Module keyword inside module body", `Quick, () => {
    parse_and_evaluate_test(
      "42",
      {|{ module Inner = { let z = 42 }; let r = Inner.z }.r|},
    )
  });

/* Test nested module keyword with capitalized inner name */
let test_nested_module_keyword =
  test_case("Nested module keyword", `Quick, () => {
    parse_and_evaluate_test(
      "10",
      {|module Outer = { module Inner = { let x = 10 } } in Outer.Inner.x|},
    )
  });

/* Test module keyword with type annotation */
let test_module_keyword_annotated =
  test_case("Module keyword with annotation", `Quick, () => {
    parse_and_evaluate_test(
      "1",
      {|module M : (x=Int) = { let x = 1 } in M.x|},
    )
  });

/* Test module keyword with sig annotation */
let test_module_keyword_sig_annotated =
  test_case("Module keyword with sig annotation", `Quick, () => {
    parse_and_evaluate_test(
      "42",
      {|module M : { let x : Int } = { let x = 42 } in M.x|},
    )
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
    /* Module keyword tests */
    test_module_keyword_lowercase,
    test_module_keyword_capitalized,
    test_module_keyword_in_mod,
    test_nested_module_keyword,
    test_module_keyword_annotated,
    test_module_keyword_sig_annotated,
  ],
);

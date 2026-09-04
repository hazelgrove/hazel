open Alcotest;
open Test_Evaluator_Prelude;

/* Module dynamics: a module evaluates item by item to a module value, whose
   items are its exported bindings. Expected values are written as module
   literals; an evaluated binding compares equal to `let x = v`. */

/* Test empty module evaluates to the empty module */
let test_empty_module =
  test_case("Empty module evaluates to the empty module", `Quick, () => {
    parse_and_evaluate_test("{}", {|{}|})
  });

/* Test single binding module */
let test_single_binding =
  test_case("Single binding module", `Quick, () => {
    parse_and_evaluate_test("{ let x = 1 }", {|{ let x = 1 }|})
  });

/* Test multiple bindings module */
let test_multiple_bindings =
  test_case("Multiple bindings module", `Quick, () => {
    parse_and_evaluate_test(
      {|{ let x = 1; let y = "hello" }|},
      {|{ let x = 1; let y = "hello" }|},
    )
  });

/* Test shadowing - only last value exported */
let test_shadowing =
  test_case("Shadowed binding exports last value", `Quick, () => {
    parse_and_evaluate_test(
      {|{ let x = "hello" }|},
      {|{ let x = 1; let x = "hello" }|},
    )
  });

/* Test accessing module binding via dot */
let test_module_access =
  test_case("Access module binding", `Quick, () => {
    parse_and_evaluate_test("1", {|{ let x = 1 }.x|})
  });

/* Test member access on a bound module */
let test_module_as_tuple =
  test_case("Member access on a bound module", `Quick, () => {
    parse_and_evaluate_test(
      "3",
      {|let point = { let x = 1; let y = 2 } in point.x + point.y|},
    )
  });

/* Test bare expression side effect */
let test_bare_expression =
  test_case("Bare expression evaluated and dropped", `Quick, () => {
    parse_and_evaluate_test("{ let x = 2 }", {|{ 1 + 1; let x = 2 }|})
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
    parse_and_evaluate_test(
      "{ let x = 1; let y = 2 }",
      {|{ let x = 1; let y = x + 1 }|},
    )
  });

/* Test type alias in module: type items have no runtime content */
let test_type_alias =
  test_case("Type alias in module is dropped at runtime", `Quick, () => {
    parse_and_evaluate_test(
      "{ let x = 42 }",
      {|{ type T = Int; let x = 42 : T }|},
    )
  });

/* Test pattern binding inside a module */
let test_pattern_binding =
  test_case("Pattern binding exports each variable", `Quick, () => {
    parse_and_evaluate_test(
      "{ let a = 1; let b = 2 }",
      {|{ let (a, b) = (1, 2) }|},
    )
  });

/* Test applying a function member */
let test_function_member_applied =
  test_case("Function member applied", `Quick, () => {
    parse_and_evaluate_test("2", {|{ let f = fun x -> x + 1 }.f(1)|})
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

/* Test module keyword with a multi-member sig annotation */
let test_module_keyword_annotated =
  test_case("Module keyword with multi-member sig annotation", `Quick, () => {
    parse_and_evaluate_test(
      "3",
      {|module M : { let x : Int; let y : Int } = { let x = 1; let y = 2 } in M.x + M.y|},
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

/* Test type members in a signature annotation are erased */
let test_sig_type_member_erased =
  test_case("Signature type members have no runtime content", `Quick, () => {
    parse_and_evaluate_test(
      "1",
      {|let m : { type T = Int; let x : T } = { type T = Int; let x = 1 } in m.x|},
    )
  });

/* Sealing at runtime keeps only the signature's members */
let test_sealing_drops_extras =
  test_case("Sealing drops members absent from the signature", `Quick, () => {
    parse_and_evaluate_test(
      "{ let x = 1 }",
      {|module M : { let x : Int } = { let x = 1; let y = 2 } in M|},
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
    test_pattern_binding,
    test_function_member_applied,
    test_case("Nested modules", `Quick, () => {
      parse_and_evaluate_test(
        "{ let m = { let y = 1 } }",
        {|{ let m = { let y = 1 } }|},
      )
    }),
    /* Module keyword tests */
    test_module_keyword_lowercase,
    test_module_keyword_capitalized,
    test_module_keyword_in_mod,
    test_nested_module_keyword,
    test_module_keyword_annotated,
    test_module_keyword_sig_annotated,
    test_sig_type_member_erased,
    test_sealing_drops_extras,
  ],
);

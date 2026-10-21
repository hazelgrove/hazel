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

/* A module ascribed a one-member signature, with a member more: an error
   in statics (the signature does not admit the extra member), but it still
   runs, and M.x is the member, 1, as a labeled tuple's projection gives */
let test_module_one_member_signature =
  test_case(
    "Projecting a declared member of a module wider than its signature",
    `Quick,
    () => {
    parse_and_evaluate_test(
      "1",
      {|module M : { let x : Int } = { let x = 1; let y = 2 } in M.x|},
    )
  });

/* A module bound to a function */
let test_module_function =
  test_case("A module bound to a function", `Quick, () => {
    parse_and_evaluate_test("3", {|module F = fun x -> x + 1 in F(2)|})
  });

/* A module bound to a fixpoint the user wrote: the fixpoint is how a
   module binds a recursive function, so m(3) = 3 * 2 * 1 */
let test_module_fix =
  test_case("A module bound to a fixpoint", `Quick, () => {
    parse_and_evaluate_test(
      "6",
      {|module m = fix f -> fun n -> if n == 0 then 1 else n * f(n - 1) in m(3)|},
    )
  });

/* A module's definition can use the module it shadows. The definition's
   elaboration runs without a fixpoint, so this result does not show which
   F the statics bound; Statics.Modules checks that. */
let test_module_shadows_module =
  test_case("A module definition uses the module it shadows", `Quick, () => {
    parse_and_evaluate_test(
      "4",
      {|module F = fun x -> x + 1 in module F = fun x -> F(x) * 2 in F(1)|},
    )
  });

/* G defined in terms of G, where an outer G exists. As a `module ... in`
   binding it is not recursive, so its G is the outer one: G(3) = 20 + 1.
   As an item in braces it lowers to a plain Let, which is recursive, so G
   calls itself: G(3) = 3. This pins that split as it is, until it is
   decided whether braces items are recursive. */
let test_module_binding_uses_outer =
  test_case(
    "A module binding's G is the outer G (pins the braces split)", `Quick, () => {
    parse_and_evaluate_test(
      "21",
      {|module G = fun (n : Int) -> n * 10 in module G = fun (n : Int) -> if n == 0 then 0 else G(n - 1) + 1 in G(3)|},
    )
  });

let test_module_item_calls_itself =
  test_case(
    "A module item's G calls itself (pins the braces split)", `Quick, () => {
    parse_and_evaluate_test(
      "3",
      {|module G = fun (n : Int) -> n * 10 in module Outer = { module G = fun (n : Int) -> if n == 0 then 0 else G(n - 1) + 1 } in Outer.G(3)|},
    )
  });

/* A let inside a module's definition is recursive as usual */
let test_module_def_recursive_let =
  test_case("A recursive let inside a module definition", `Quick, () => {
    parse_and_evaluate_test(
      "5",
      {|module F = fun x -> (let g = fun n -> if n == 0 then x else g(n - 1) in g(3)) in F(5)|},
    )
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
    test_module_one_member_signature,
    test_module_function,
    test_module_fix,
    test_module_shadows_module,
    test_module_binding_uses_outer,
    test_module_item_calls_itself,
    test_module_def_recursive_let,
    test_module_keyword_capitalized,
    test_module_keyword_in_mod,
    test_nested_module_keyword,
    test_module_keyword_annotated,
    test_module_keyword_sig_annotated,
  ],
);

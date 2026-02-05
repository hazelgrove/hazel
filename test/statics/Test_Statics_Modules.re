open Language;
open Alcotest;
open Test_Statics_Prelude;
open FTemp;
open Typ;

/* Module statics tests - Phase 1 implementation */

/* Test empty module */
let test_empty_module =
  fully_consistent_typecheck(
    "Empty module",
    {|{}|},
    Some(prod([])),
  );

/* Test single binding module */
let test_single_binding =
  fully_consistent_typecheck(
    "Single let binding",
    {|{ let x = 1 }|},
    Some(prod([tup_label(label("x"), int())])),
  );

/* Test multiple bindings */
let test_multiple_bindings =
  fully_consistent_typecheck(
    "Multiple let bindings",
    {|{ let x = 1; let y = "hello" }|},
    Some(
      prod([
        tup_label(label("x"), int()),
        tup_label(label("y"), string()),
      ]),
    ),
  );

/* Test shadowing - only last binding should be exported */
let test_shadowing =
  fully_consistent_typecheck(
    "Shadowed binding exports last value",
    {|{ let x = 1; let x = "hello" }|},
    Some(prod([tup_label(label("x"), string())])),
  );

/* Test type alias in module */
let test_type_alias =
  fully_consistent_typecheck(
    "Type alias in module",
    {|{ type T = Int; let x = 1 : T }|},
    Some(prod([tup_label(label("x"), int())])),
  );

/* Test bare expression in module */
let test_bare_expression =
  fully_consistent_typecheck(
    "Bare expression in module",
    {|{ 1 + 1; let x = 2 }|},
    Some(prod([tup_label(label("x"), int())])),
  );

/* Test accessing module binding */
let test_module_access =
  fully_consistent_typecheck(
    "Access module binding",
    {|{ let x = 1 }.x|},
    Some(int()),
  );

/* TODO: Test nested module when nested modules work */

/* Test module with complex expression */
let test_module_complex_expression =
  fully_consistent_typecheck(
    "Module with complex expression",
    {|{ let f = fun x : Int -> x + 1; let result = f(10) }|},
    Some(
      prod([
        tup_label(label("f"), arrow(int(), int())),
        tup_label(label("result"), int()),
      ]),
    ),
  );

/* Test module used as labeled tuple */
let test_module_as_labeled_tuple =
  fully_consistent_typecheck(
    "Module as labeled tuple",
    {|let point = { let x = 1; let y = 2 } in point.x + point.y|},
    Some(int()),
  );

/* Skip test for syntax that may not work yet */
let skip_module_test = (message: string, _expression: string) =>
  test_case("Skip: " ++ message, `Quick, () => {
    Alcotest.skip()
  });

let tests = (
  "Statics.Modules",
  [
    test_empty_module,
    test_single_binding,
    test_multiple_bindings,
    test_shadowing,
    test_type_alias,
    test_bare_expression,
    test_module_access,
    test_module_complex_expression,
    test_module_as_labeled_tuple,
    skip_module_test("Nested modules", {|{ let m = { let y = 1 } }|}),
  ],
);

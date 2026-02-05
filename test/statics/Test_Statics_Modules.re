open Language;
open Alcotest;
open Test_Statics_Prelude;
open FTemp;
open Typ;

/* Module statics tests - Phase 1 implementation */

/* Test empty module */
let test_empty_module =
  fully_consistent_typecheck("Empty module", {|{}|}, Some(prod([])));

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

/* Test module as let binding definition - verifies module works in Exp position */
let test_module_in_let_def =
  fully_consistent_typecheck(
    "Module as let definition",
    {|let m = { let y = 1 } in m|},
    Some(prod([tup_label(label("y"), int())])),
  );

/* Simpler diagnostic - module with two bindings to avoid singleton tuple issues */
let test_module_two_bindings_in_let =
  fully_consistent_typecheck(
    "Module with two bindings as let def",
    {|let m = { let x = 1; let y = 2 } in m|},
    Some(prod([tup_label(label("x"), int()), tup_label(label("y"), int())])),
  );

/* Diagnostic: test the expanded form directly - this is what { let y = 1 } expands to */
let test_expansion_directly =
  fully_consistent_typecheck(
    "Expanded module form directly",
    {|let y = 1 in (y=y)|},
    Some(prod([tup_label(label("y"), int())])),
  );

/* Diagnostic: expanded form nested in let */
let test_expansion_in_let =
  fully_consistent_typecheck(
    "Expanded form in let",
    {|let m = (let y = 1 in (y=y)) in m|},
    Some(prod([tup_label(label("y"), int())])),
  );

/* Test nested module - inner module should have labeled tuple type */
let test_nested_module =
  fully_consistent_typecheck(
    "Nested modules",
    {|{ let m = { let y = 1 } }|},
    Some(
      prod([
        tup_label(label("m"), prod([tup_label(label("y"), int())])),
      ]),
    ),
  );

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
    test_module_in_let_def,
    test_module_two_bindings_in_let,
    test_expansion_directly,
    test_expansion_in_let,
    test_nested_module,
  ],
);

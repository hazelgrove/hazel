open Test_Statics_Prelude;
open FTemp;
open Typ;

/* ===== WELL-TYPED MODULE TESTS ===== */

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

/* Test module as let binding definition */
let test_module_in_let_def =
  fully_consistent_typecheck(
    "Module as let definition",
    {|let m = { let y = 1 } in m|},
    Some(prod([tup_label(label("y"), int())])),
  );

/* Test module with two bindings in let */
let test_module_two_bindings_in_let =
  fully_consistent_typecheck(
    "Module with two bindings as let def",
    {|let m = { let x = 1; let y = 2 } in m|},
    Some(
      prod([tup_label(label("x"), int()), tup_label(label("y"), int())]),
    ),
  );

/* Diagnostic: test the expanded form directly */
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

/* Test nested module */
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

/* Test nested field access */
let test_nested_field_access =
  fully_consistent_typecheck(
    "Nested module field access",
    {|let m = { let inner = { let x = 1 } } in m.inner.x|},
    Some(int()),
  );

/* Test module with boolean binding */
let test_module_bool =
  fully_consistent_typecheck(
    "Module with boolean binding",
    {|{ let flag = true }|},
    Some(prod([tup_label(label("flag"), bool())])),
  );

/* Test module with float binding */
let test_module_float =
  fully_consistent_typecheck(
    "Module with float binding",
    {|{ let pi = 3.14 }|},
    Some(prod([tup_label(label("pi"), float())])),
  );

/* Test module with list binding */
let test_module_list =
  fully_consistent_typecheck(
    "Module with list binding",
    {|{ let xs = [1, 2, 3] }|},
    Some(prod([tup_label(label("xs"), list(int()))])),
  );

/* Test module with tuple binding */
let test_module_tuple =
  fully_consistent_typecheck(
    "Module with tuple binding",
    {|{ let pair = (1, true) }|},
    Some(prod([tup_label(label("pair"), prod([int(), bool()]))])),
  );

/* Test sequential binding reference */
let test_sequential_binding_ref =
  fully_consistent_typecheck(
    "Later bindings can reference earlier ones",
    {|{ let x = 1; let y = x + 1 }|},
    Some(
      prod([tup_label(label("x"), int()), tup_label(label("y"), int())]),
    ),
  );

/* Test module with annotation (Prod type) */
let test_module_with_prod_annotation =
  fully_consistent_typecheck(
    "Module with labeled tuple annotation",
    {|let m : (x=Int) = { let x = 1 } in m|},
    Some(prod([tup_label(label("x"), int())])),
  );

/* Test module with multi-field Prod annotation */
let test_module_with_multi_prod_annotation =
  fully_consistent_typecheck(
    "Module with multi-field labeled tuple annotation",
    {|let m : (x=Int, y=Bool) = { let x = 1; let y = true } in m|},
    Some(
      prod([tup_label(label("x"), int()), tup_label(label("y"), bool())]),
    ),
  );

/* ===== TYPE ERROR TESTS ===== */

/* Type mismatch: annotation says Int, module provides String */
let test_error_type_mismatch =
  inconsistent_typecheck(
    "Type mismatch in module annotation",
    {|let m : (x=Int) = { let x = "hello" } in m|} |> parse_exp,
  );

/* Type mismatch with multiple bindings */
let test_error_type_mismatch_multi =
  inconsistent_typecheck(
    "Type mismatch in one of multiple bindings",
    {|let m : (x=Int, y=Bool) = { let x = 1; let y = "oops" } in m|}
    |> parse_exp,
  );

/* Wrong type used in module body */
let test_error_wrong_type_in_body =
  inconsistent_typecheck(
    "Wrong type in annotated module body",
    {|{ let x = 1 + true }|} |> parse_exp,
  );

/* Free variable in module */
let test_error_free_variable =
  inconsistent_typecheck(
    "Free variable in module",
    {|{ let x = unbound_var }|} |> parse_exp,
  );

/* Type error in nested module */
let test_error_nested_type_mismatch =
  inconsistent_typecheck(
    "Type error in nested module",
    {|{ let inner = { let x = 1 + true } }|} |> parse_exp,
  );

/* Dot access on non-existent field */
let test_error_bad_field_access =
  inconsistent_typecheck(
    "Access non-existent module field",
    {|{ let x = 1 }.y|} |> parse_exp,
  );

/* Dot access on non-existent field via variable */
let test_error_bad_field_via_var =
  inconsistent_typecheck(
    "Access non-existent field via module variable",
    {|let m = { let x = 1 } in m.y|} |> parse_exp,
  );

/* Type error from using module field with wrong type */
let test_error_field_type_mismatch =
  inconsistent_typecheck(
    "Using module field where wrong type expected",
    {|let m = { let x = "hello" } in m.x + 1|} |> parse_exp,
  );

/* Type annotation with wrong type on binding inside module */
let test_error_binding_annotation_mismatch =
  inconsistent_typecheck(
    "Annotated binding type mismatch inside module",
    {|{ let x : Int = "hello" }|} |> parse_exp,
  );

/* Type error in sequential binding reference */
let test_error_sequential_type =
  inconsistent_typecheck(
    "Type error from sequential binding",
    {|{ let x = "hello"; let y = x + 1 }|} |> parse_exp,
  );

let tests = (
  "Statics.Modules",
  [
    /* Well-typed tests */
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
    test_nested_field_access,
    test_module_bool,
    test_module_float,
    test_module_list,
    test_module_tuple,
    test_sequential_binding_ref,
    test_module_with_prod_annotation,
    test_module_with_multi_prod_annotation,
    /* Type error tests */
    test_error_type_mismatch,
    test_error_type_mismatch_multi,
    test_error_wrong_type_in_body,
    test_error_free_variable,
    test_error_nested_type_mismatch,
    test_error_bad_field_access,
    test_error_bad_field_via_var,
    test_error_field_type_mismatch,
    test_error_binding_annotation_mismatch,
    test_error_sequential_type,
  ],
);

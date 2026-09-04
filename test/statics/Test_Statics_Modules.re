open Test_Statics_Prelude;
open FTemp;
open Typ;

/* Signature-type builders. Items must be listed in synthesized (source)
   order: Typ.fast_equal compares signature items positionally. */
let val_ = (x, ty) => Sig.sig_let(Pat.asc(Pat.var(x), ty));
let type_ = (t, ty) => Sig.sig_type(TPat.var(t), ty);

/* ===== WELL-TYPED MODULE TESTS ===== */

/* Test empty module */
let test_empty_module =
  fully_consistent_typecheck("Empty module", {|{}|}, Some(sig_([])));

/* Test single binding module */
let test_single_binding =
  fully_consistent_typecheck(
    "Single let binding",
    {|{ let x = 1 }|},
    Some(sig_([val_("x", int())])),
  );

/* Test multiple bindings */
let test_multiple_bindings =
  fully_consistent_typecheck(
    "Multiple let bindings",
    {|{ let x = 1; let y = "hello" }|},
    Some(sig_([val_("x", int()), val_("y", string())])),
  );

/* Test shadowing - only last binding should be exported */
let test_shadowing =
  fully_consistent_typecheck(
    "Shadowed binding exports last value",
    {|{ let x = 1; let x = "hello" }|},
    Some(sig_([val_("x", string())])),
  );

/* Test type alias in module: the member keeps referring to the exported
   type member, which the signature binds. */
let test_type_alias =
  fully_consistent_typecheck(
    "Type alias in module",
    {|{ type T = Int; let x = 1 : T }|},
    Some(sig_([type_("T", int()), val_("x", var("T"))])),
  );

/* Test bare expression in module */
let test_bare_expression =
  fully_consistent_typecheck(
    "Bare expression in module",
    {|{ 1 + 1; let x = 2 }|},
    Some(sig_([val_("x", int())])),
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
    Some(sig_([val_("y", int())])),
  );

/* Test module with two bindings in let */
let test_module_two_bindings_in_let =
  fully_consistent_typecheck(
    "Module with two bindings as let def",
    {|let m = { let x = 1; let y = 2 } in m|},
    Some(sig_([val_("x", int()), val_("y", int())])),
  );

/* Labeled tuples are unaffected by modules */
let test_expansion_directly =
  fully_consistent_typecheck(
    "Labeled tuple built from a let",
    {|let y = 1 in (y=y)|},
    Some(prod([tup_label(label("y"), int())])),
  );

let test_expansion_in_let =
  fully_consistent_typecheck(
    "Labeled tuple built from a let, bound",
    {|let m = (let y = 1 in (y=y)) in m|},
    Some(prod([tup_label(label("y"), int())])),
  );

/* Test nested module */
let test_nested_module =
  fully_consistent_typecheck(
    "Nested modules",
    {|{ let m = { let y = 1 } }|},
    Some(sig_([val_("m", sig_([val_("y", int())]))])),
  );

/* Test module with complex expression */
let test_module_complex_expression =
  fully_consistent_typecheck(
    "Module with complex expression",
    {|{ let f = fun x : Int -> x + 1; let result = f(10) }|},
    Some(sig_([val_("f", arrow(int(), int())), val_("result", int())])),
  );

/* Test member access on a bound module */
let test_module_as_labeled_tuple =
  fully_consistent_typecheck(
    "Member access on a bound module",
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
    Some(sig_([val_("flag", bool())])),
  );

/* Test module with float binding */
let test_module_float =
  fully_consistent_typecheck(
    "Module with float binding",
    {|{ let pi = 3.14 }|},
    Some(sig_([val_("pi", float())])),
  );

/* Test module with list binding */
let test_module_list =
  fully_consistent_typecheck(
    "Module with list binding",
    {|{ let xs = [1, 2, 3] }|},
    Some(sig_([val_("xs", list(int()))])),
  );

/* Test module with tuple binding: the member's type stays a tuple */
let test_module_tuple =
  fully_consistent_typecheck(
    "Module with tuple binding",
    {|{ let pair = (1, true) }|},
    Some(sig_([val_("pair", prod([int(), bool()]))])),
  );

/* Test sequential binding reference */
let test_sequential_binding_ref =
  fully_consistent_typecheck(
    "Later bindings can reference earlier ones",
    {|{ let x = 1; let y = x + 1 }|},
    Some(sig_([val_("x", int()), val_("y", int())])),
  );

/* ===== SIGNATURE SYNTHESIS ===== */

let test_interleaved_members =
  fully_consistent_typecheck(
    "Interleaved members in source order",
    {|{ let x = 1; type T = Bool; let y = true }|},
    Some(sig_([val_("x", int()), type_("T", bool()), val_("y", bool())])),
  );

let test_type_only_module =
  fully_consistent_typecheck(
    "Type-only module is a signature",
    {|{ type T = Int }|},
    Some(sig_([type_("T", int())])),
  );

let test_member_references_type_member =
  fully_consistent_typecheck(
    "Value member references sibling type member",
    {|{ type T = Int; let x : T = 1 }|},
    Some(sig_([type_("T", int()), val_("x", var("T"))])),
  );

/* A shadowed type member is not exported; its uses are inlined. */
let test_shadowed_type_member =
  fully_consistent_typecheck(
    "Shadowed type member is inlined, last declaration exported",
    {|{ type T = Int; let x : T = 1; type T = Bool; let y : T = true }|},
    Some(sig_([val_("x", int()), type_("T", bool()), val_("y", bool())])),
  );

let test_module_keyword_type_member =
  fully_consistent_typecheck(
    "Module returned with type member",
    {|module M = { type T = Int; let x : T = 1 } in M|},
    Some(sig_([type_("T", int()), val_("x", var("T"))])),
  );

let test_nested_type_member_through_variable =
  fully_consistent_typecheck(
    "Nested module type members through variable",
    {|module M = { module P = { type S = Int } } in M|},
    Some(sig_([val_("P", sig_([type_("S", int())]))])),
  );

let test_sig_alias_stays_sig =
  fully_consistent_typecheck(
    "Signature alias stays a signature",
    {|type MT = { let x : Int } in let m : MT = { let x = 4 } in m.x|},
    Some(int()),
  );

/* A binder takes its annotation as written; the unannotated member is `?`. */
let test_unannotated_sig_member =
  fully_consistent_typecheck(
    "Unannotated signature member accepts any definition",
    {|let m : { let x } = { let x = 1 } in m|},
    Some(sig_([Sig.sig_let(Pat.var("x"))])),
  );

let test_dot_on_sig_with_type_member =
  fully_consistent_typecheck(
    "Member access substitutes the module's manifest type member",
    {|let m = { type T = Int; let x : T = 1 } in m.x + 1|},
    Some(int()),
  );

/* ===== SIGNATURE ANNOTATION TESTS (well-typed) ===== */

/* Empty module with matching empty sig annotation */
let test_empty_sig_annotation =
  fully_consistent_typecheck(
    "Empty module with empty sig annotation",
    {|let s1 : {} = {} in s1|},
    Some(sig_([])),
  );

/* Module with matching sig annotation */
let test_matching_sig_annotation =
  fully_consistent_typecheck(
    "Module with matching sig annotation",
    {|let s1 : { let x : Int } = { let x = 42 } in s1|},
    Some(sig_([val_("x", int())])),
  );

/* Multi-member matching sig annotation */
let test_matching_sig_multi =
  fully_consistent_typecheck(
    "Module with multi-member matching sig annotation",
    {|let m : { let x : Int; let y : Bool } = { let x = 1; let y = true } in m|},
    Some(sig_([val_("x", int()), val_("y", bool())])),
  );

/* A manifest type member in the signature is checked and substituted into
   the expected member types; the binder keeps the annotation as written. */
let test_sig_type_member_matches =
  fully_consistent_typecheck(
    "Signature manifest type member matches the module's",
    {|let m : { type T = Int; let x : T } = { type T = Int; let x = 1 } in m|},
    Some(sig_([type_("T", int()), val_("x", var("T"))])),
  );

/* ===== TYPE ERROR TESTS ===== */

/* Labeled tuple annotations do not accept modules */
let test_error_type_mismatch =
  inconsistent_typecheck(
    "Labeled tuple annotation with wrong member type rejects module",
    {|let m : (x=Int) = { let x = "hello" } in m|} |> parse_exp,
  );

let test_error_type_mismatch_multi =
  inconsistent_typecheck(
    "Multi-field labeled tuple annotation rejects module",
    {|let m : (x=Int, y=Bool) = { let x = 1; let y = "oops" } in m|}
    |> parse_exp,
  );

/* Module member type doesn't match sig annotation */
let test_error_sig_type_mismatch =
  inconsistent_typecheck(
    "Module member type mismatch with sig annotation",
    {|let annotated : { let x : Int } = { let x = true } in annotated|}
    |> parse_exp,
  );

/* Module with matching and non-matching members */
let test_error_sig_partial_mismatch =
  inconsistent_typecheck(
    "Sig annotation with one matching and one mismatched member",
    {|let annotated : { let x : Int; let y : Bool } = { let x = 1; let y = 2 } in annotated|}
    |> parse_exp,
  );

/* Module missing members (sig wider than module) */
let test_error_sig_too_wide =
  inconsistent_typecheck(
    "Module missing members required by sig",
    {|let annotated : { let x : Int; let y : Bool; let z : String } = { let x = 1; let y = true } in annotated|}
    |> parse_exp,
  );

/* Singleton module missing a member (sig wider) */
let test_error_sig_too_wide_singleton =
  inconsistent_typecheck(
    "Singleton module missing member required by sig",
    {|let annotated : { let x : Int; let y : Bool } = { let x = 1 } in annotated|}
    |> parse_exp,
  );

/* Empty module missing all members */
let test_error_sig_too_wide_empty =
  inconsistent_typecheck(
    "Empty module missing all members required by sig",
    {|let annotated : { let x : Int; let y : Bool } = {} in annotated|}
    |> parse_exp,
  );

/* Signature manifest type member differs from the module's */
let test_error_sig_type_member_mismatch =
  inconsistent_typecheck(
    "Signature manifest type member mismatch",
    {|let m : { type T = Int; let x : T } = { type T = Bool; let x = true } in m|}
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

/* Type members are not value members */
let test_error_type_member_as_value =
  inconsistent_typecheck(
    "Type member is not a value member",
    {|let m = { type T = Int } in m.T|} |> parse_exp,
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

/* ===== SIGNATURES ARE DISTINCT FROM LABELED TUPLE TYPES ===== */

let test_sig_rejects_tuple =
  inconsistent_typecheck(
    "Sig annotation rejects labeled tuple value",
    {|let mt : { let x : Int } = (x=1) in mt|} |> parse_exp,
  );

let test_prod_rejects_module =
  inconsistent_typecheck(
    "Prod annotation rejects module value",
    {|let tm : (x=Int) = { let x = 1 } in tm|} |> parse_exp,
  );

let test_sig_rejects_tuple_wrong_type =
  inconsistent_typecheck(
    "Sig annotation rejects labeled tuple with wrong member type",
    {|let mt : { let x : Int } = (x=true) in mt|} |> parse_exp,
  );

let test_prod_rejects_module_wrong_type =
  inconsistent_typecheck(
    "Prod annotation rejects module with wrong member type",
    {|let tm : (x=Int) = { let x = true } in tm|} |> parse_exp,
  );

let test_empty_module_is_not_unit =
  inconsistent_typecheck(
    "Empty module is not the unit tuple",
    {|let u : () = {} in u|} |> parse_exp,
  );

let test_unit_is_not_empty_module =
  inconsistent_typecheck(
    "Unit tuple is not the empty module",
    {|let e : {} = () in e|} |> parse_exp,
  );

let test_module_tuple_equality_rejected =
  inconsistent_typecheck(
    "Comparing a labeled tuple with a module is inconsistent",
    {|test (x=1) == { let x = 1 } end|} |> parse_exp,
  );

/* ===== NO WIDTH SUBTYPING YET (arrives with ana_meet) ===== */
let test_extra_member_rejected =
  inconsistent_typecheck(
    "Extra module member is rejected by an exact signature",
    {|let annotated : {} = { let x = 1 } in annotated|} |> parse_exp,
  );

let test_extra_member_multi_rejected =
  inconsistent_typecheck(
    "Extra member in multi-member module is rejected",
    {|let annotated : { let x : Int } = { let x = 1; let y = 2 } in annotated|}
    |> parse_exp,
  );

let test_sig_too_narrow_rejected =
  inconsistent_typecheck(
    "Signature narrower than module is rejected",
    {|let annotated : { let x : Int; let y : Bool } = { let x = 1; let y = true; let z = "hello" } in annotated|}
    |> parse_exp,
  );

/* A hole-typed member is still a required member */
let test_label_mismatch_hole =
  inconsistent_typecheck(
    "Label mismatch with hole type is a missing member",
    {|let m : { let x : ? } = { let y = 1 } in m|} |> parse_exp,
  );

/* ===== MODULE KEYWORD TESTS ===== */

/* Test module keyword with lowercase name */
let test_module_keyword_lowercase =
  fully_consistent_typecheck(
    "Module keyword with lowercase name",
    {|module m = { let x = 1 } in m.x|},
    Some(int()),
  );

/* Test module keyword with capitalized name */
let test_module_keyword_capitalized =
  fully_consistent_typecheck(
    "Module keyword with capitalized name",
    {|module M = { let x = 1; let y = 2 } in M.x + M.y|},
    Some(int()),
  );

/* Test module keyword inside module body */
let test_module_keyword_in_mod =
  fully_consistent_typecheck(
    "Module keyword inside module body",
    {|{ module Inner = { let z = 42 }; let r = Inner.z }|},
    Some(
      sig_([val_("Inner", sig_([val_("z", int())])), val_("r", int())]),
    ),
  );

/* Test module keyword returning the module itself */
let test_module_keyword_returns_module =
  fully_consistent_typecheck(
    "Module keyword returns module value",
    {|module M = { let a = 1; let b = true } in M|},
    Some(sig_([val_("a", int()), val_("b", bool())])),
  );

/* Test capitalized name in dot position (chained access) */
let test_capitalized_dot_access =
  fully_consistent_typecheck(
    "Capitalized name in dot position",
    {|module Outer = { module Inner = { let x = 10 } } in Outer.Inner.x|},
    Some(int()),
  );

/* Module keyword with prod annotation is rejected */
let test_module_keyword_prod_annotation =
  inconsistent_typecheck(
    "Module keyword with prod annotation is rejected",
    {|module M : (x=Int) = { let x = 1 } in M.x|} |> parse_exp,
  );

/* Test module keyword with sig annotation */
let test_module_keyword_sig_annotation =
  fully_consistent_typecheck(
    "Module keyword with sig annotation",
    {|module M : { let x : Int } = { let x = 42 } in M.x|},
    Some(int()),
  );

/* Module keyword with multi-field prod annotation is rejected */
let test_module_keyword_multi_annotation =
  inconsistent_typecheck(
    "Module keyword with multi-field prod annotation is rejected",
    {|module M : (x=Int, y=Bool) = { let x = 1; let y = true } in M|}
    |> parse_exp,
  );

/* Test module keyword annotation type mismatch */
let test_error_module_keyword_annotation_mismatch =
  inconsistent_typecheck(
    "Module keyword annotation type mismatch",
    {|module M : (x=Int) = { let x = "hello" } in M|} |> parse_exp,
  );

/* Test module keyword sig annotation mismatch */
let test_error_module_keyword_sig_mismatch =
  inconsistent_typecheck(
    "Module keyword sig annotation mismatch",
    {|module M : { let x : Int } = { let x = true } in M|} |> parse_exp,
  );

/* ===== QUALIFIED TYPE ACCESS TESTS ===== */
/* Use `x + 0` etc. to force the result to a concrete type, since
   the annotation M.T stores as ProdProjection in the info map. */

/* Basic M.T access */
let test_qualified_type_basic =
  fully_consistent_typecheck(
    "Qualified type access: M.T",
    {|module M = { type T = Int } in let x : M.T = 6 in x + 0|},
    Some(int()),
  );

/* Multiple type exports */
let test_qualified_type_multiple =
  fully_consistent_typecheck(
    "Qualified type access: multiple exports",
    {|module M = { type T = Int; type U = Bool } in let x : M.T = 1 in let y : M.U = true in x + 0|},
    Some(int()),
  );

/* Internal type reference resolution */
let test_qualified_type_internal_ref =
  fully_consistent_typecheck(
    "Qualified type access: internal reference",
    {|module M = { type A = Int -> Bool; type B = A } in let f : M.B = fun x -> x > 0 in f(1)|},
    Some(bool()),
  );

/* Nested module type access */
let test_qualified_type_nested =
  fully_consistent_typecheck(
    "Qualified type access: nested M.P.S",
    {|module M = { module P = { type S = Int } } in let x : M.P.S = 5 in x + 0|},
    Some(int()),
  );

/* Lowercase let binding with type access */
let test_qualified_type_lowercase =
  fully_consistent_typecheck(
    "Qualified type access: lowercase let",
    {|let m = { type T = Int } in let y : m.T = 6 in y + 0|},
    Some(int()),
  );

/* Type used after non-shadowed definition */
let test_qualified_type_shadowing =
  fully_consistent_typecheck(
    "Qualified type access: type used after non-shadowed definition",
    {|module M = { type T = Int; let x : T = 1 } in let y : M.T = 2 in y + 0|},
    Some(int()),
  );

/* Unknown type member (error) */
let test_error_qualified_type_unknown =
  inconsistent_typecheck(
    "Qualified type access: unknown member M.U",
    {|module M = { type T = Int } in let x : M.U = 5 in x|} |> parse_exp,
  );

/* Signature alias type member */
let test_qualified_type_sig_alias =
  fully_consistent_typecheck(
    "Qualified type access: signature alias",
    {|type MS = { type T = Int; let x : T } in let y : MS.T = 3 in y + 0|},
    Some(int()),
  );

/* ===== QUALIFIED TYPE ACCESS: ALIASING TESTS ===== */

/* Variable aliasing */
let test_qualified_type_var_alias =
  fully_consistent_typecheck(
    "Qualified type access: variable aliasing",
    {|module M = { type T = Int } in let n = M in let x : n.T = 5 in x + 0|},
    Some(int()),
  );

/* Module aliasing */
let test_qualified_type_module_alias =
  fully_consistent_typecheck(
    "Qualified type access: module aliasing",
    {|module M = { type T = Int } in module N = M in let x : N.T = 5 in x + 0|},
    Some(int()),
  );

/* Chained aliasing */
let test_qualified_type_chained_alias =
  fully_consistent_typecheck(
    "Qualified type access: chained aliasing",
    {|module M = { type T = Int } in let n = M in let p = n in let x : p.T = 5 in x + 0|},
    Some(int()),
  );

/* Nested module with values alongside type exports (Shapes.Geo.Point scenario) */
let test_qualified_type_nested_with_values =
  fully_consistent_typecheck(
    "Qualified type access: nested with sibling value bindings",
    {|module Shapes = { let radius = 5; module Geo = { type Point = (x=Int, y=Int) } } in let home : Shapes.Geo.Point = (x=0, y=0) in home.x + 0|},
    Some(int()),
  );

/* Module aliasing inside module: module Geo = Geometry propagates type exports */
let test_qualified_type_nested_alias =
  fully_consistent_typecheck(
    "Qualified type access: nested module alias",
    {|module Geometry = { type Point = (Int, Int) } in module Shapes = { module Geo = Geometry } in let (a, _) : Shapes.Geo.Point = (0, 1) in a + 0|},
    Some(int()),
  );

/* Module aliasing with sibling type exports */
let test_qualified_type_nested_alias_with_sibling =
  fully_consistent_typecheck(
    "Qualified type access: nested module alias with sibling type",
    {|module Geometry = { type Point = (Int, Int) } in module Shapes = { module Geo = Geometry; type Radius = Int } in let (a, _) : Shapes.Geo.Point = (0, 1) in a + 0|},
    Some(int()),
  );

/* Type alias shadowing: nested modules should be able to shadow outer type aliases */
let test_type_alias_shadowing_in_nested_module =
  fully_consistent_typecheck(
    "Type alias shadowing in nested module",
    {|type T = Int in module M = { type T = Bool; let x : T = true } in M.x|},
    Some(bool()),
  );

let test_type_alias_shadowing_sequential =
  fully_consistent_typecheck(
    "Type alias shadowing sequential",
    {|type T = Int in type T = Bool in true : T|},
    Some(bool()),
  );

let test_type_alias_shadowing_nested_module =
  fully_consistent_typecheck(
    "Type alias shadowing in nested module within same body",
    {|module M = { type T = Int; module Q = { type T = String; let x : T = "abc" } } in M.Q.x|},
    Some(string()),
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
    /* Signature synthesis */
    test_interleaved_members,
    test_type_only_module,
    test_member_references_type_member,
    test_shadowed_type_member,
    test_module_keyword_type_member,
    test_nested_type_member_through_variable,
    test_sig_alias_stays_sig,
    test_unannotated_sig_member,
    test_dot_on_sig_with_type_member,
    /* Sig annotation tests */
    test_empty_sig_annotation,
    test_matching_sig_annotation,
    test_matching_sig_multi,
    test_sig_type_member_matches,
    /* Type error tests */
    test_error_type_mismatch,
    test_error_type_mismatch_multi,
    test_error_sig_type_mismatch,
    test_error_sig_partial_mismatch,
    test_error_sig_too_wide,
    test_error_sig_too_wide_singleton,
    test_error_sig_too_wide_empty,
    test_error_sig_type_member_mismatch,
    test_error_wrong_type_in_body,
    test_error_free_variable,
    test_error_nested_type_mismatch,
    test_error_bad_field_access,
    test_error_bad_field_via_var,
    test_error_type_member_as_value,
    test_error_field_type_mismatch,
    test_error_binding_annotation_mismatch,
    test_error_sequential_type,
    /* Signatures are distinct from labeled tuple types */
    test_sig_rejects_tuple,
    test_prod_rejects_module,
    test_sig_rejects_tuple_wrong_type,
    test_prod_rejects_module_wrong_type,
    test_empty_module_is_not_unit,
    test_unit_is_not_empty_module,
    test_module_tuple_equality_rejected,
    /* No width subtyping yet */
    test_extra_member_rejected,
    test_extra_member_multi_rejected,
    test_sig_too_narrow_rejected,
    test_label_mismatch_hole,
    /* Module keyword tests */
    test_module_keyword_lowercase,
    test_module_keyword_capitalized,
    test_module_keyword_in_mod,
    test_module_keyword_returns_module,
    test_capitalized_dot_access,
    /* Module keyword annotation tests */
    test_module_keyword_prod_annotation,
    test_module_keyword_sig_annotation,
    test_module_keyword_multi_annotation,
    test_error_module_keyword_annotation_mismatch,
    test_error_module_keyword_sig_mismatch,
    /* Qualified type access tests */
    test_qualified_type_basic,
    test_qualified_type_multiple,
    test_qualified_type_internal_ref,
    test_qualified_type_nested,
    test_qualified_type_lowercase,
    test_qualified_type_shadowing,
    test_error_qualified_type_unknown,
    test_qualified_type_sig_alias,
    /* Qualified type access aliasing tests */
    test_qualified_type_var_alias,
    test_qualified_type_module_alias,
    test_qualified_type_chained_alias,
    test_qualified_type_nested_with_values,
    test_qualified_type_nested_alias,
    test_qualified_type_nested_alias_with_sibling,
    /* Type alias shadowing tests */
    test_type_alias_shadowing_in_nested_module,
    test_type_alias_shadowing_sequential,
    test_type_alias_shadowing_nested_module,
  ],
);

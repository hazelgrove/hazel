open Test_Statics_Prelude;
open FTemp;
open Typ;

/* Signature-type builders. Items must be listed in synthesized (source)
   order: Typ.fast_equal compares signature items positionally. */
let val_ = (x, ty) => Sig.sig_let(Pat.asc(Pat.var(x), ty));
let type_ = (t, ty) => Sig.sig_type(TPat.var(t), ty);
let mod_ = (x, ty) => Sig.sig_module(MPat.asc(MPat.var(x), ty));

/* Assert that some mark in the program satisfies [pred]. */
let has_mark_test = (name, source, pred: Language.Mark.t => bool) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let marks =
        statics(parse_exp(source)) |> errors |> List.concat_map(snd);
      Alcotest.(check(bool))(name, true, List.exists(pred, marks));
    },
  );

/* The marks on the sub-expression whose term satisfies [pred]. */
let subexp_marks =
    (source, pred: Language.Exp.term => bool): list(Language.Mark.t) =>
  Language.Id.Map.fold(
    (_, info: Language.Info.t, acc) =>
      switch (acc, info) {
      | (None, InfoExp({user_term, marks, _})) when pred(user_term.term) =>
        Some(marks)
      | _ => acc
      },
    statics(parse_exp(source)),
    None,
  )
  |> Option.value(~default=[]);

/* Exactly one mark in the whole program, and it satisfies [pred]. */
let single_mark_test = (name, source, pred: Language.Mark.t => bool) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let marks =
        statics(parse_exp(source)) |> errors |> List.concat_map(snd);
      Alcotest.(check(bool))(
        name,
        true,
        switch (marks) {
        | [m] => pred(m)
        | _ => false
        },
      );
    },
  );

/* ===== PROJECTION ERROR ATTRIBUTION ===== */

/* `m.y` with no member y: the label carries the error, the dot only a
   message (the design the type-level `M.Fake` already follows). */
let is_dot: Language.Exp.term => bool =
  fun
  | Dot(_) => true
  | _ => false;
let is_label = (l: string, t: Language.Exp.term): bool =>
  switch (t) {
  | Label(name) => name == l
  | _ => false
  };

let test_member_not_found_on_label =
  Alcotest.test_case(
    "A missing member is reported on the label, not on the dot",
    `Quick,
    () => {
      let src = {|let m = { let x = 1 } in m.y|};
      Alcotest.(check(bool))(
        "label marked",
        true,
        List.exists(
          fun
          | Language.Mark.ModuleMemberNotFound({
              name: "y",
              members: ["x"],
              type_member: false,
            }) =>
            true
          | _ => false,
          subexp_marks(src, is_label("y")),
        ),
      );
      Alcotest.(check(bool))(
        "dot unmarked",
        true,
        subexp_marks(src, is_dot) == [],
      );
    },
  );

let test_type_member_as_value_mark =
  has_mark_test(
    "Accessing a type member as a value says so",
    {|let m = { type T = Int } in m.T|},
    fun
    | Language.Mark.ModuleMemberNotFound({name: "T", type_member: true, _}) =>
      true
    | _ => false,
  );

let test_dot_on_non_module_mark =
  has_mark_test(
    "Projecting from a non-module value is a dot error",
    {|let n = 1 in n.x|},
    fun
    | Language.Mark.DotOperatorRequiresTuple => true
    | _ => false,
  );

let test_type_member_not_found_mark =
  has_mark_test(
    "A missing type member names the module's type members",
    {|module M = { type Real = Int } in let bad : M.Fake = 1 in bad|},
    fun
    | Language.Mark.ModuleTypeMemberNotFound({
        name: "Fake",
        members: ["Real"],
        submodule: false,
      }) =>
      true
    | _ => false,
  );

let test_no_type_members_mark =
  has_mark_test(
    "A module without type members says so",
    {|module E = {} in let bad : E.T = 1 in bad|},
    fun
    | Language.Mark.ModuleTypeMemberNotFound({name: "T", members: [], _}) =>
      true
    | _ => false,
  );

let test_submodule_not_found_mark =
  has_mark_test(
    "A missing sub-module in a type path says so",
    {|module M = { module P = { type S = Int } } in let bad : M.Q.S = 1 in bad|},
    fun
    | Language.Mark.ModuleTypeMemberNotFound({
        name: "Q",
        members: ["P"],
        submodule: true,
      }) =>
      true
    | _ => false,
  );

let test_value_used_as_module_path_mark =
  has_mark_test(
    "A value that is not a module cannot root a type path",
    {|let n = 1 in let y : n.T = 2 in y|},
    fun
    | Language.Mark.TypWantModule({name: "n", _}) => true
    | _ => false,
  );

/* A root whose type is unknown may be a module, so `n.T` is not an error. */
let test_unknown_typed_root_is_not_an_error =
  Alcotest.test_case(
    "A root of unknown type may be a module: its type path is not an error",
    `Quick,
    () => {
      let marks =
        statics(parse_exp({|let n : ? = 1 in let y : n.T = 2 in y|}))
        |> errors
        |> List.concat_map(snd);
      Alcotest.(check(bool))("no marks", true, List.is_empty(marks));
    },
  );

/* A differing manifest type member is reported once, on the member's
   definition type: the members are checked against the module's own
   definition of T, and the module is not reported a second time. */
let test_type_member_mismatch_single_error =
  single_mark_test(
    "A differing type member is the module's only error",
    {|module M : { type T = Int; let x : T } = { type T = Bool; let x = true } in M|},
    fun
    | Language.Mark.ModuleTypeMemberMismatch({name: "T", _}) => true
    | _ => false,
  );

/* The marks on the type whose term satisfies [pred]. */
let subtyp_marks =
    (source, pred: Language.Typ.term => bool): list(Language.Mark.t) =>
  Language.Id.Map.fold(
    (_, info: Language.Info.t, acc) =>
      switch (acc, info) {
      | (None, InfoTyp({user_term, marks, _})) when pred(user_term.term) =>
        Some(marks)
      | _ => acc
      },
    statics(parse_exp(source)),
    None,
  )
  |> Option.value(~default=[]);

let test_type_member_mismatch_on_the_definition =
  Alcotest.test_case(
    "A differing type member is marked on its definition type",
    `Quick,
    () => {
      let marks =
        subtyp_marks(
          {|module M : { type T = Int; let x : T } = { type T = Bool; let x = true } in M|},
          fun
          | Atom(Bool) => true
          | _ => false,
        );
      Alcotest.(check(bool))(
        "Bool marked",
        true,
        List.exists(
          fun
          | Language.Mark.ModuleTypeMemberMismatch({name: "T", _}) => true
          | _ => false,
          marks,
        ),
      );
    },
  );

let test_type_member_mismatch_with_wrong_definition =
  Alcotest.test_case(
    "A differing type member and a definition wrong for the module's own T",
    `Quick,
    () => {
      let marks =
        statics(
          parse_exp(
            {|module M : { type T = Int; let x : T } = { type T = Bool; let x = 1 } in M|},
          ),
        )
        |> errors
        |> List.concat_map(snd);
      Alcotest.(check(int))("two errors", 2, List.length(marks));
      Alcotest.(check(bool))(
        "type member and definition",
        true,
        List.exists(
          fun
          | Language.Mark.ModuleTypeMemberMismatch(_) => true
          | _ => false,
          marks,
        )
        && List.exists(
             fun
             | Language.Mark.ExpectationMismatch(_) => true
             | _ => false,
             marks,
           ),
      );
    },
  );

/* ===== HOLE BINDERS ===== */

/* A hole among the items could still become the members the signature
   declares and the module lacks, so they are assumed rather than reported;
   a wildcard binds nothing and assumes nothing. */
let test_hole_binder_assumes_missing_members =
  fully_consistent_typecheck(
    "A hole binder stands in for the missing members",
    {|module M : { let x : Int; let y : Int } = { let x = 1; let ? = 2 } in M.y|},
    Some(int()),
  );

let test_hole_item_assumes_missing_members =
  fully_consistent_typecheck(
    "A hole item stands in for the missing members",
    {|module M : { let x : Int; let y : Int } = { let x = 1; ? } in M.y|},
    Some(int()),
  );

let test_hole_in_destructuring_binder_assumes_missing_members =
  fully_consistent_typecheck(
    "A hole inside a destructuring binder stands in for the missing members",
    {|module M : { let x : Int; let y : Int } = { let (x, ?) = (1, 2) } in M.y|},
    Some(int()),
  );

let test_hole_item_assumes_missing_type_member =
  fully_consistent_typecheck(
    "A hole item stands in for a missing type member",
    {|module M : { type T = Int; let x : Int } = { let x = 1; ? } in let y : M.T = 3 in y + 1|},
    Some(int()),
  );

let test_error_wildcard_binder_does_not_assume =
  has_mark_test(
    "A wildcard binder does not stand in for missing members",
    {|module M : { let x : Int; let y : Int } = { let x = 1; let _ = 2 } in M|},
    fun
    | Language.Mark.ModuleMissingMembers(["y"]) => true
    | _ => false,
  );

/* Only a hole where a member could be bound counts: not one in a definition
   or in a type annotation. */
let test_error_non_binder_holes_do_not_assume =
  Alcotest.test_case(
    "Holes in a definition or an annotation do not stand in for members",
    `Quick,
    () => {
      let missing_y = src =>
        List.exists(
          fun
          | Language.Mark.ModuleMissingMembers(["y"]) => true
          | _ => false,
          statics(parse_exp(src)) |> errors |> List.concat_map(snd),
        );
      Alcotest.(check(bool))(
        "definition hole",
        true,
        missing_y(
          {|module M : { let x : Int; let y : Int } = { let x = ? } in M|},
        ),
      );
      Alcotest.(check(bool))(
        "annotation hole",
        true,
        missing_y(
          {|module M : { let x : Int; let y : Int } = { let x : ? = 1 } in M|},
        ),
      );
    },
  );

/* ===== NESTED EXPECTATIONS ===== */

/* The signature's expectations reach sub-modules and the variables inside
   destructuring patterns, so a problem is reported where it is. */
let is_int_vs = (syn_cls, m: Language.Mark.t) =>
  switch (m) {
  | ExpectationMismatch({ana, syn}) =>
    switch (Language.Typ.term_of(ana), Language.Typ.term_of(syn)) {
    | (Atom(Int), Atom(c)) => c == syn_cls
    | _ => false
    }
  | _ => false
  };

let test_nested_missing_member_localized =
  single_mark_test(
    "A sub-module lacking a member is reported once, inside it",
    {|module M : { module Inner : { let x : Int; let y : Int } } = { module Inner = { let x = 1 } } in M|},
    fun
    | Language.Mark.ModuleMissingMembers(["y"]) => true
    | _ => false,
  );

let test_nested_type_member_mismatch_localized =
  single_mark_test(
    "A sub-module's differing type member is reported once, on its item",
    {|module M : { module Inner : { type T = Int } } = { module Inner = { type T = Bool } } in M|},
    fun
    | Language.Mark.ModuleTypeMemberMismatch({name: "T", _}) => true
    | _ => false,
  );

let test_nested_definition_mismatch_localized =
  single_mark_test(
    "A sub-module's wrong definition is reported once, on the definition",
    {|module M : { module Inner : { let x : Int } } = { module Inner = { let x = true } } in M|},
    is_int_vs(Bool),
  );

let test_nested_user_annotation_kept =
  Alcotest.test_case(
    "A sub-module's own annotation is kept: its definition is checked against it",
    `Quick,
    () => {
      /* Inner's own annotation disagrees with the outer expectation. Were it
         replaced by the expectation, `true` would be marked against Int. */
      let marks =
        statics(
          parse_exp(
            {|module M : { module Inner : { let x : Int } } = { module Inner : { let x : Bool } = { let x = true } } in M|},
          ),
        )
        |> errors
        |> List.concat_map(snd);
      Alcotest.(check(bool))(
        "the disagreement is marked",
        false,
        List.is_empty(marks),
      );
      Alcotest.(check(bool))(
        "the definition is not marked against Int",
        false,
        List.exists(is_int_vs(Bool), marks),
      );
    },
  );

let test_error_nested_module_sealed =
  inconsistent_typecheck(
    "A sub-module is sealed by the outer signature",
    {|module M : { module Inner : { let x : Int } } = { module Inner = { let x = 1; let y = 2 } } in M.Inner.y|}
    |> parse_exp,
  );

/* The component carries the mismatch and the module does not. The tuple
   literal is also marked, as it is for a top-level
   `let (a : Int, b : Int) = (1, "s")`: that cascade is not module-specific. */
let component_mismatch_test = (name, source, syn_cls) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let marks =
        statics(parse_exp(source)) |> errors |> List.concat_map(snd);
      Alcotest.(check(bool))(
        "component marked",
        true,
        List.exists(is_int_vs(syn_cls), marks),
      );
      Alcotest.(check(bool))(
        "module unmarked",
        true,
        subexp_marks(
          source,
          fun
          | Module(_) => true
          | _ => false,
        )
        == [],
      );
    },
  );

let test_tuple_binder_mismatch_localized =
  component_mismatch_test(
    "A destructured member with the wrong type is reported on the component",
    {|module M : { let a : Int; let b : Int } = { let (a, b) = (1, "s") } in M|},
    String,
  );

let test_tuple_binder_ok =
  fully_consistent_typecheck(
    "A destructured definition matching its signature",
    {|module M : { let a : Int; let b : Int } = { let (a, b) = (1, 2) } in M.a + M.b|},
    Some(int()),
  );

let test_labeled_tuple_binder_ok =
  fully_consistent_typecheck(
    "A labeled destructured definition matching its signature",
    {|module M : { let a : Int; let b : Int } = { let (x=a, y=b) = (x=1, y=2) } in M.a + M.b|},
    Some(int()),
  );

let test_nested_tuple_binder_mismatch_localized =
  component_mismatch_test(
    "A nested destructured member with the wrong type is reported on the component",
    {|module M : { let a : Int; let b : Int; let c : Int } = { let ((a, b), c) = ((1, 2), "s") } in M|},
    String,
  );

/* A cons pattern is also inexhaustive, as at top level. */
let test_cons_binder_mismatch_localized =
  component_mismatch_test(
    "A cons-destructured member with the wrong type is reported on the element",
    {|module M : { let h : Int; let t : [Int] } = { let h :: t = ["s"] } in M|},
    String,
  );

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
    Some(sig_([mod_("P", sig_([type_("S", int())]))])),
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

/* Module items report the member they declare, not the rest of the body. */
let test_item_types =
  Alcotest.test_case(
    "Module items are typed as their member",
    `Quick,
    () => {
      let exp =
        parse_exp(
          {|{ module Db = { type Id = Int }; let x = 1; type T = Bool }|},
        );
      let s = statics(exp);
      let item_types =
        switch (exp.term) {
        | Module(items) =>
          List.map(
            (item: Language.Mod.t) =>
              Language.Statics.Map.ty_of(Language.Mod.rep_id(item), s),
            items,
          )
        | _ => []
        };
      Alcotest.check(
        Alcotest.list(Alcotest.option(testable_typ)),
        "item types",
        [Some(sig_([type_("Id", int())])), Some(int()), Some(bool())],
        item_types,
      );
    },
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

/* A capitalized sub-module is declared with `module M : S` in a signature */
let test_sig_module_member =
  fully_consistent_typecheck(
    "Sub-module declared in a signature",
    {|module M : { module Inner : { let x : Int }; let y : Int } = { module Inner = { let x = 1 }; let y = 2 } in M.Inner.x + M.y|},
    Some(int()),
  );

let test_sig_module_member_binder_type =
  fully_consistent_typecheck(
    "Sub-module signature member is kept as written on the binder",
    {|module M : { module Inner : { let x : Int } } = { module Inner = { let x = 1 } } in M|},
    Some(sig_([mod_("Inner", sig_([val_("x", int())]))])),
  );

let test_sig_lowercase_nested =
  fully_consistent_typecheck(
    "Lowercase sub-module declared with let in a signature",
    {|let m : { let inner : { let x : Int } } = { let inner = { let x = 1 } } in m.inner.x|},
    Some(int()),
  );

let test_error_sig_module_member_mismatch =
  inconsistent_typecheck(
    "Sub-module member type mismatch",
    {|module M : { module Inner : { let x : Int } } = { module Inner = { let x = true } } in M|}
    |> parse_exp,
  );

/* ===== SPECIFIC MARKS ===== */

let test_missing_member_mark =
  has_mark_test(
    "Missing member is reported as a missing member",
    {|let m : { let x : Int; let y : Bool } = { let x = 1 } in m|},
    fun
    | Language.Mark.ModuleMissingMembers(["y"]) => true
    | _ => false,
  );

let test_type_member_mismatch_mark =
  has_mark_test(
    "Differing type member is reported on the type item",
    {|let m : { type T = Int; let x : T } = { type T = Bool; let x = true } in m|},
    fun
    | Language.Mark.ModuleTypeMemberMismatch({name: "T", _}) => true
    | _ => false,
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

/* ===== SEALING AT COERCION SITES ===== */
/* At an ascription, an annotated binder or an application argument a module
   may export more than the signature declares; the extras are sealed away and
   the binder has exactly the signature's type. Everywhere else signatures
   match exactly. */
let test_width_empty_sig =
  fully_consistent_typecheck(
    "Extra member is sealed away by an empty signature",
    {|let annotated : {} = { let x = 1 } in annotated|},
    Some(sig_([])),
  );

let test_width_extra_member =
  fully_consistent_typecheck(
    "Extra member is sealed away by the signature",
    {|let annotated : { let x : Int } = { let x = 1; let y = 2 } in annotated|},
    Some(sig_([val_("x", int())])),
  );

let test_width_narrower_sig =
  fully_consistent_typecheck(
    "Signature narrower than module seals the rest",
    {|let annotated : { let x : Int; let y : Bool } = { let x = 1; let y = true; let z = "hello" } in annotated|},
    Some(sig_([val_("x", int()), val_("y", bool())])),
  );

let test_width_extra_type_member =
  fully_consistent_typecheck(
    "Extra type member is sealed away",
    {|let m : { let x : Int } = { type T = Int; let x = 1 } in m|},
    Some(sig_([val_("x", int())])),
  );

let test_sealed_member_inaccessible =
  inconsistent_typecheck(
    "Sealed-away member is not accessible",
    {|let m : { let x : Int } = { let x = 1; let y = 2 } in m.y|} |> parse_exp,
  );

let test_width_in_asc =
  fully_consistent_typecheck(
    "Sealing at an ascription",
    {|({ let x = 1; let y = 2 } : { let x : Int })|},
    Some(sig_([val_("x", int())])),
  );

let test_width_in_module_keyword =
  fully_consistent_typecheck(
    "Sealing with the module keyword",
    {|module M : { let x : Int } = { let x = 1; let y = 2 } in M|},
    Some(sig_([val_("x", int())])),
  );

let test_width_function_argument_literal =
  fully_consistent_typecheck(
    "Sealing a module literal argument",
    {|let f = fun (m : { let x : Int }) -> m.x in f({ let x = 1; let y = 2 })|},
    Some(int()),
  );

let test_width_function_argument_variable =
  fully_consistent_typecheck(
    "Sealing a module variable argument",
    {|let big = { let x = 1; let y = 2 } in let f = fun (m : { let x : Int }) -> m.x in f(big)|},
    Some(int()),
  );

let test_width_bound_variable =
  fully_consistent_typecheck(
    "Sealing when binding a module variable",
    {|let big = { let x = 1; let y = 2 } in let m : { let x : Int } = big in m|},
    Some(sig_([val_("x", int())])),
  );

let test_width_hole_member =
  fully_consistent_typecheck(
    "Hole-typed member accepts any type, extras sealed",
    {|let m : { let x : ? } = { let x = 1; let y = 2 } in m|},
    Some(sig_([val_("x", unknown(Hole(EmptyHole)))])),
  );

let test_width_depth =
  fully_consistent_typecheck(
    "Sealing through a nested module member",
    {|let n : { let m : { let x : Int } } = { let m = { let x = 1; let y = 2 } } in n|},
    Some(sig_([val_("m", sig_([val_("x", int())]))])),
  );

/* Function types match exactly: a function on a narrower module is not
   coerced to one on a wider module (eta-expand instead). */
let test_error_width_no_contravariance =
  inconsistent_typecheck(
    "A function on a narrower module is not coerced to a wider domain",
    {|let g : { let x : Int; let y : Int } -> Int = fun (m : { let x : Int }) -> m.x in g|}
    |> parse_exp,
  );

let test_error_width_covariant_domain =
  inconsistent_typecheck(
    "A function on a wider module does not accept a narrower one",
    {|let g : { let x : Int } -> Int = fun (m : { let x : Int; let y : Int }) -> m.x in g|}
    |> parse_exp,
  );

/* Consistency stays exact: branches must agree on their members. */
let test_error_width_not_in_if =
  inconsistent_typecheck(
    "Width does not apply across if branches",
    {|if true then { let x = 1 } else { let x = 1; let y = 2 }|} |> parse_exp,
  );

/* If branches are not coercion sites: under an annotation the wider branch
   itself carries the mismatch. */
let test_error_width_not_in_if_annotated =
  Alcotest.test_case(
    "Under an annotation a wider if branch is marked on the branch",
    `Quick,
    () => {
      let marks =
        subexp_marks(
          {|let m : { let x : Int } = if true then { let x = 1 } else { let x = 1; let y = 2 } in m|},
          fun
          | Module(items) => List.length(items) == 2
          | _ => false,
        );
      Alcotest.(check(bool))(
        "wider branch marked",
        true,
        List.exists(
          fun
          | Language.Mark.ExpectationMismatch(_) => true
          | _ => false,
          marks,
        ),
      );
    },
  );

let test_if_identical_sigs =
  fully_consistent_typecheck(
    "If branches with identical signatures",
    {|if true then { let x = 1 } else { let x = 2 }|},
    Some(sig_([val_("x", int())])),
  );

let test_width_not_for_tuples =
  inconsistent_typecheck(
    "Labeled tuples are not sealed by width",
    {|let t : (x=Int) = (x=1, y=2) in t|} |> parse_exp,
  );

/* Slide examples: sealing in use */
let test_width_interface_function =
  fully_consistent_typecheck(
    "A function over any module with the members it needs",
    {|let greet = fun (m : { let name : String }) -> "hello " ++ m.name in
module Alice = { let name = "Alice"; let age = 30 } in
module Bob = { let name = "Bob"; let email = "bob@example.com" } in
greet(Alice) ++ greet(Bob)|},
    Some(string()),
  );

let test_width_hides_helper =
  fully_consistent_typecheck(
    "Only the signature's members are exported",
    {|module Stack : {
  let empty : [Int];
  let push : Int -> [Int] -> [Int];
  let top : [Int] -> Int
} = {
  let default = 0;
  let empty = [];
  let push = fun x -> fun s -> x :: s;
  let top = fun s -> case s | h :: _ => h | [] => default end
} in
Stack.top(Stack.push(1)(Stack.empty))|},
    Some(int()),
  );

let test_error_width_hidden_helper =
  inconsistent_typecheck(
    "A helper the signature omits is not accessible",
    {|module Stack : { let top : [Int] -> Int } = {
  let default = 0;
  let top = fun s -> case s | h :: _ => h | [] => default end
} in
Stack.default|}
    |> parse_exp,
  );

let test_width_nested_member_wider =
  fully_consistent_typecheck(
    "Nested members may be wider than declared",
    {|module Config : { module Db : { let host : String } } = {
  module Db = { let host = "localhost"; let port = 5432 }
} in
Config.Db.host|},
    Some(string()),
  );

let test_width_if_branch_ascribed =
  fully_consistent_typecheck(
    "Ascribing the wider branch seals it so the branches agree",
    {|let pick = fun b -> if b then { let x = 1 } else ({ let x = 1; let y = 2 } : { let x : Int }) in pick(true).x|},
    Some(int()),
  );

/* Non-sites: the same wider module is a mismatch anywhere but under an
   ascription, an annotated binder or in argument position, up to parentheses
   and tuple structure. */
let test_error_sealing_not_in_list =
  inconsistent_typecheck(
    "A wider module is not sealed as a list element",
    {|let wide = { let x = 1; let y = 2 } in let l : [{ let x : Int }] = [wide] in l|}
    |> parse_exp,
  );

let test_sealing_in_list_ascribed =
  fully_consistent_typecheck(
    "Ascribing a list element seals it",
    {|let wide = { let x = 1; let y = 2 } in let l : [{ let x : Int }] = [(wide : { let x : Int })] in l|},
    Some(list(sig_([val_("x", int())]))),
  );

let test_error_sealing_module_literal_in_list =
  has_mark_test(
    "A module literal with extra members as a list element is a mismatch",
    {|let l : [{ let x : Int }] = [{ let x = 1; let y = 2 }] in l|},
    fun
    | Language.Mark.ExpectationMismatch(_) => true
    | _ => false,
  );

let test_sealing_tuple_argument =
  fully_consistent_typecheck(
    "Each component of a tuple argument is sealed",
    {|let f = fun (m : { let x : Int }, n : Int) -> m.x + n in
let wide = { let x = 1; let y = 2 } in
f(wide, 1)|},
    Some(int()),
  );

let test_sealing_tuple_literal_annotated =
  fully_consistent_typecheck(
    "An annotation seals the components of a tuple literal",
    {|let wide = { let x = 1; let y = 2 } in let p : (Int, { let x : Int }) = (1, wide) in p|},
    Some(prod([int(), sig_([val_("x", int())])])),
  );

let test_sealing_labeled_tuple_component =
  fully_consistent_typecheck(
    "An annotation seals a labeled tuple component",
    {|let wide = { let x = 1; let y = 2 } in let p : (m={ let x : Int }, n=Int) = (m=wide, n=1) in p.n|},
    Some(int()),
  );

/* The coercion is structural through tuples, so a variable of tuple type is
   coerced componentwise at a site too. */
let test_sealing_tuple_variable =
  fully_consistent_typecheck(
    "A variable of tuple type is coerced componentwise at a site",
    {|let wide = { let x = 1; let y = 2 } in let q = (1, wide) in let p : (Int, { let x : Int }) = q in p|},
    Some(prod([int(), sig_([val_("x", int())])])),
  );

let test_error_sealing_not_in_case_pattern =
  inconsistent_typecheck(
    "A pattern annotation narrower than the scrutinee is a mismatch",
    {|let wide = { let x = 1; let y = 2 } in case wide | (m : { let x : Int }) => m.x end|}
    |> parse_exp,
  );

let test_sealing_scrutinee_ascribed =
  fully_consistent_typecheck(
    "Ascribing the scrutinee seals it",
    {|let wide = { let x = 1; let y = 2 } in case (wide : { let x : Int }) | m => m.x end|},
    Some(int()),
  );

/* A function literal's body is its result: under an expected arrow type it
   is sealed to the codomain, like a functor body to its result signature.
   The parameter is still matched exactly (no contravariance). */
let test_sealing_function_body =
  fully_consistent_typecheck(
    "A function body is sealed to the expected codomain",
    {|let f : { let x : Int } -> { let x : Int } = fun m -> { let x = m.x; let y = 1 } in f({ let x = 1 })|},
    Some(sig_([val_("x", int())])),
  );

let test_sealing_through_parens =
  fully_consistent_typecheck(
    "Parentheses do not block sealing",
    {|let wide = { let x = 1; let y = 2 } in let n : { let x : Int } = ((wide)) in n|},
    Some(sig_([val_("x", int())])),
  );

let test_error_sealing_not_through_let_body =
  inconsistent_typecheck(
    "A let body is not a coercion site",
    {|let wide = { let x = 1; let y = 2 } in let n : { let x : Int } = (let h = 1 in wide) in n|}
    |> parse_exp,
  );

let test_error_sealing_not_in_if_branches =
  inconsistent_typecheck(
    "If branches are not coercion sites",
    {|let wide = { let x = 1; let y = 2 } in let n : { let x : Int } = if true then wide else wide in n|}
    |> parse_exp,
  );

let test_sealing_constructor_argument =
  fully_consistent_typecheck(
    "A constructor argument is sealed like any application argument",
    {|type Box = Wrap({ let x : Int }) + Empty in
let b : Box = Wrap({ let x = 1; let y = 2 }) in
case b | Wrap(m) => m.x | Empty => 0 end|},
    Some(int()),
  );

let test_error_sealing_not_through_sum_variable =
  inconsistent_typecheck(
    "A variable of a wider sum type is not coerced",
    {|type Box = Wrap({ let x : Int }) + Empty in
type Wide = Wrap({ let x : Int; let y : Int }) + Empty in
let w : Wide = Wrap({ let x = 1; let y = 2 }) in
let b : Box = w in b|}
    |> parse_exp,
  );

/* A hole-named signature member is not a required member, and the module's
   binding is an extra member the signature seals away. */
let test_hole_named_member_matches_any =
  fully_consistent_typecheck(
    "Signature member with a hole for its name accepts a module member",
    {|let m : { let ? : ? } = { let y = 1 } in 0|},
    Some(int()),
  );

/* A hole-typed member is still a required member */
let test_label_mismatch_hole =
  inconsistent_typecheck(
    "Label mismatch with hole type is a missing member",
    {|let m : { let x : ? } = { let y = 1 } in m|} |> parse_exp,
  );

/* ===== ABSTRACT TYPE MEMBERS ===== */

/* `type T` with no definition. A module sealed by such a signature must
   define T, but outside the module T is known only as the path `M.T`. */
let abs_ = t => Sig.sig_type_abstract(TPat.var(t));
let path = (m, t) => prod_projection(var(m), label(t));
let sealed_m = {|module M : { type T; let x : T } = { type T = Int; let x = 1 } in |};

let test_abstract_member_wellformed =
  fully_consistent_typecheck(
    "A signature may declare an abstract type member",
    {|type S = { type T; let x : T } in 1|},
    Some(int()),
  );

/* A module variable names its own abstract types: M's signature is seen as
   `{ type T = M.T; let x : T }`. */
let test_sealed_module_type =
  fully_consistent_typecheck(
    "A sealed module names its abstract member by the path M.T",
    sealed_m ++ {|M|},
    Some(sig_([type_("T", path("M", "T")), val_("x", var("T"))])),
  );

let test_sealed_member_has_path_type =
  fully_consistent_typecheck(
    "A member of abstract type has the path type",
    sealed_m ++ {|M.x|},
    Some(path("M", "T")),
  );

let test_error_sealed_representation_hidden =
  inconsistent_typecheck(
    "Sealing hides the representation",
    sealed_m ++ {|M.x + 1|} |> parse_exp,
  );

let test_abstract_member_used_through_interface =
  fully_consistent_typecheck(
    "Values of abstract type flow through the module's own functions",
    {|module C : { type T; let zero : T; let get : T -> Int } = { type T = Int; let zero = 0; let get = fun t -> t } in C.get(C.zero)|},
    Some(int()),
  );

let test_abstract_path_annotation =
  fully_consistent_typecheck(
    "An abstract path annotates a binding",
    sealed_m ++ {|let q : M.T = M.x in q|},
    Some(path("M", "T")),
  );

let test_error_distinct_sealings =
  inconsistent_typecheck(
    "Separately sealed modules have distinct abstract types",
    sealed_m
    ++ {|module N : { type T; let x : T } = { type T = Int; let x = 1 } in let y : N.T = M.x in y|}
    |> parse_exp,
  );

let test_error_same_sig_alias_distinct_instances =
  inconsistent_typecheck(
    "Two modules sealed by the same signature alias are distinct",
    {|type S = { type T; let x : T } in module M : S = { type T = Int; let x = 1 } in module N : S = { type T = Int; let x = 1 } in let y : N.T = M.x in y|}
    |> parse_exp,
  );

let test_module_alias_shares_abstract_type =
  fully_consistent_typecheck(
    "module N = M shares M's abstract type",
    sealed_m ++ {|module N = M in let y : N.T = M.x in y|},
    Some(path("N", "T")),
  );

let test_variable_alias_shares_abstract_type =
  fully_consistent_typecheck(
    "let m = M shares M's abstract type",
    sealed_m ++ {|let m = M in let z : m.T = m.x in z|},
    Some(path("m", "T")),
  );

let test_manifest_member_stays_transparent =
  fully_consistent_typecheck(
    "A manifest type member is transparent",
    {|module M : { type T = Int; let x : T } = { type T = Int; let x = 1 } in M.x + 1|},
    Some(int()),
  );

let test_unsealed_module_stays_transparent =
  fully_consistent_typecheck(
    "An unsealed module's type members are transparent",
    {|module M = { type T = Int; let x = 1 : T } in M.x + 1|},
    Some(int()),
  );

let test_missing_type_member_mark =
  has_mark_test(
    "A module lacking an abstract member's definition is missing it",
    {|module M : { type T; let x : T } = { let x = 1 } in M|},
    fun
    | Language.Mark.ModuleMissingMembers(["T"]) => true
    | _ => false,
  );

let test_error_type_member_kind_mismatch =
  inconsistent_typecheck(
    "A value member does not satisfy a type member of the same name",
    {|module M : { type x } = { let x = 1 } in M|} |> parse_exp,
  );

let test_error_forward_reference_in_sig =
  inconsistent_typecheck(
    "Signature members cannot mention a later type member",
    {|type S = { let x : T; type T } in 1|} |> parse_exp,
  );

let test_sealing_through_abstract_path =
  fully_consistent_typecheck(
    "A module may realize its abstract type by another module's path",
    sealed_m
    ++ {|module N : { type U; let y : U } = { type U = M.T; let y = M.x } in N.y|},
    Some(path("N", "U")),
  );

/* Only a path can name an abstract member; projecting from any other
   expression of the signature's type yields `?`. */
let test_non_path_projection_is_unknown =
  fully_consistent_typecheck(
    "An abstract member projected from a non-path is unknown",
    sealed_m ++ {|(M : { type T; let x : T }).x|},
    Some(unknown(Internal)),
  );

let test_error_unknown_member_on_sealed =
  inconsistent_typecheck(
    "A member absent from the sealing signature is not accessible",
    sealed_m ++ {|M.y|} |> parse_exp,
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
      sig_([mod_("Inner", sig_([val_("z", int())])), val_("r", int())]),
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

/* A capitalized module name parses as a constructor, so a module whose name
   is also a constructor's must still win in dot position: the JSON type
   contributes `List`, `Int`, `String`, `Float` and `Bool` to every program's
   constructor namespace, and projecting a constructor means nothing. */
let test_module_shadowing_a_constructor =
  fully_consistent_typecheck(
    "A module named like a constructor is still a module when projected",
    {|module List = { let second = 2 } in List.second|},
    Some(int()),
  );

let test_constructor_still_applies =
  fully_consistent_typecheck(
    "The constructor of that name still applies",
    {|module List = { let second = 1 } in let j : JSON = List([Int(2)]) in List.second|},
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

/* ===== FUNCTION SHORTHAND MEMBERS =====
   `let f(x) = ...` in a module body binds `f` through the same desugaring
   as a top-level shorthand (see Test_Statics_FunctionSugar); its parameters
   are not members. */

let test_shorthand_member_exported =
  fully_consistent_typecheck(
    "Shorthand member is exported with its function type",
    {|{ let f(x: Int): Int = x + 1 }|},
    Some(sig_([val_("f", arrow(int(), int()))])),
  );

let test_shorthand_member_forms =
  fully_consistent_typecheck(
    "Shorthand members: unannotated, nullary, and recursive",
    {|module M = { let f(x) = x + 1; let g() = 2; let fact(n: Int): Int = if n == 0 then 1 else n * fact(n - 1) } in M.f(1) + M.g() + M.fact(4)|},
    Some(int()),
  );

let test_shorthand_member_signature =
  fully_consistent_typecheck(
    "Shorthand member checked against its signature",
    {|module M : { let f : Int -> Int } = { let f(x) = x + 1 } in M.f(1)|},
    Some(int()),
  );

let test_shorthand_member_signature_return_type =
  fully_consistent_typecheck(
    "Shorthand member with a return type, checked against its signature",
    {|module M : { let f : Int -> Int } = { let f(x): Int = x + 1 } in M.f(1)|},
    Some(int()),
  );

let test_shorthand_member_shadowed =
  fully_consistent_typecheck(
    "A later binding shadows a shorthand member",
    {|{ let f(x: Int) = x; let f = 2 }|},
    Some(sig_([val_("f", int())])),
  );

let test_shorthand_parameter_not_member =
  single_mark_test(
    "A shorthand's parameter is not a member",
    {|{ let f(x: Int) = x }.x|},
    fun
    | Language.Mark.ModuleMemberNotFound({name: "x", members: ["f"], _}) =>
      true
    | _ => false,
  );

/* The signature's type is pushed onto the definition, as for a plain
   member: the body is marked, the module is not. */
let test_shorthand_member_mismatch_on_definition =
  Alcotest.test_case(
    "A shorthand member's mismatch is reported on its definition",
    `Quick,
    () => {
      let src = {|module M : { let f : Int -> Bool } = { let f(x) = x + 1 } in M|};
      let is_mismatch: Language.Mark.t => bool =
        fun
        | ExpectationMismatch(_) => true
        | _ => false;
      Alcotest.(check(bool))(
        "definition marked",
        true,
        List.exists(
          is_mismatch,
          subexp_marks(
            src,
            fun
            | BinOp(_) => true
            | _ => false,
          ),
        ),
      );
      Alcotest.(check(bool))(
        "module unmarked",
        true,
        subexp_marks(
          src,
          fun
          | Module(_) => true
          | _ => false,
        )
        == [],
      );
    },
  );

/* ===== CYCLIC MEMBER PATHS ===== */

/* A signature that names a same-named outer binding has no weak head normal
   form: `m.T` inside the inner `m`'s own signature resolves to the inner `m`.
   Normalizing it used to abort the whole analysis with
   Failure("weak_head_normalize exceeded 1000 recursive calls"); the stuck path
   is now an ordinary type error on the member that fails to match it. */
let is_type_member_mismatch_on_t: Language.Mark.t => bool =
  fun
  | ModuleTypeMemberMismatch({name: "T", _}) => true
  | _ => false;

let test_cyclic_member_path_let =
  single_mark_test(
    "A signature naming a same-named outer binding is a type error, not a crash",
    {|let m = { type T = Int } in let m : { type T = m.T } = { type T = Int } in let y : m.T = 1 in y|},
    is_type_member_mismatch_on_t,
  );

let test_cyclic_member_path_module =
  single_mark_test(
    "The module form of a self-referential signature is a type error, not a crash",
    {|module M = { type T = Int } in module M : { type T = M.T } = { type T = Int } in let y : M.T = 1 in y|},
    is_type_member_mismatch_on_t,
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
    test_item_types,
    /* Sig annotation tests */
    test_empty_sig_annotation,
    test_matching_sig_annotation,
    test_matching_sig_multi,
    test_sig_type_member_matches,
    test_sig_module_member,
    test_sig_module_member_binder_type,
    test_sig_lowercase_nested,
    test_error_sig_module_member_mismatch,
    /* Specific marks */
    test_missing_member_mark,
    test_type_member_mismatch_mark,
    /* Projection error attribution */
    test_member_not_found_on_label,
    test_type_member_as_value_mark,
    test_dot_on_non_module_mark,
    test_type_member_not_found_mark,
    test_no_type_members_mark,
    test_submodule_not_found_mark,
    test_value_used_as_module_path_mark,
    test_unknown_typed_root_is_not_an_error,
    test_type_member_mismatch_single_error,
    test_type_member_mismatch_on_the_definition,
    test_hole_binder_assumes_missing_members,
    test_hole_item_assumes_missing_members,
    test_hole_in_destructuring_binder_assumes_missing_members,
    test_hole_item_assumes_missing_type_member,
    test_error_wildcard_binder_does_not_assume,
    test_error_non_binder_holes_do_not_assume,
    test_type_member_mismatch_with_wrong_definition,
    /* Nested expectations */
    test_nested_missing_member_localized,
    test_nested_type_member_mismatch_localized,
    test_nested_definition_mismatch_localized,
    test_nested_user_annotation_kept,
    test_error_nested_module_sealed,
    test_tuple_binder_mismatch_localized,
    test_tuple_binder_ok,
    test_labeled_tuple_binder_ok,
    test_nested_tuple_binder_mismatch_localized,
    test_cons_binder_mismatch_localized,
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
    /* Sealing at coercion sites */
    test_width_empty_sig,
    test_width_extra_member,
    test_width_narrower_sig,
    test_width_extra_type_member,
    test_sealed_member_inaccessible,
    test_width_in_asc,
    test_width_in_module_keyword,
    test_width_function_argument_literal,
    test_width_function_argument_variable,
    test_width_bound_variable,
    test_width_hole_member,
    test_width_depth,
    test_error_width_no_contravariance,
    test_error_width_covariant_domain,
    test_error_width_not_in_if,
    test_error_width_not_in_if_annotated,
    test_if_identical_sigs,
    test_width_not_for_tuples,
    test_width_interface_function,
    test_width_hides_helper,
    test_error_width_hidden_helper,
    test_width_nested_member_wider,
    test_width_if_branch_ascribed,
    /* Non-sites */
    test_error_sealing_not_in_list,
    test_sealing_in_list_ascribed,
    test_error_sealing_module_literal_in_list,
    test_sealing_tuple_argument,
    test_sealing_tuple_literal_annotated,
    test_sealing_labeled_tuple_component,
    test_sealing_tuple_variable,
    test_error_sealing_not_in_case_pattern,
    test_sealing_scrutinee_ascribed,
    test_sealing_function_body,
    test_sealing_through_parens,
    test_error_sealing_not_through_let_body,
    test_error_sealing_not_in_if_branches,
    test_sealing_constructor_argument,
    test_error_sealing_not_through_sum_variable,
    test_hole_named_member_matches_any,
    test_label_mismatch_hole,
    /* Abstract type members */
    test_abstract_member_wellformed,
    test_sealed_module_type,
    test_sealed_member_has_path_type,
    test_error_sealed_representation_hidden,
    test_abstract_member_used_through_interface,
    test_abstract_path_annotation,
    test_error_distinct_sealings,
    test_error_same_sig_alias_distinct_instances,
    test_module_alias_shares_abstract_type,
    test_variable_alias_shares_abstract_type,
    test_manifest_member_stays_transparent,
    test_unsealed_module_stays_transparent,
    test_missing_type_member_mark,
    test_error_type_member_kind_mismatch,
    test_error_forward_reference_in_sig,
    test_sealing_through_abstract_path,
    test_non_path_projection_is_unknown,
    test_error_unknown_member_on_sealed,
    /* Module keyword tests */
    test_module_keyword_lowercase,
    test_module_keyword_capitalized,
    test_module_keyword_in_mod,
    test_module_keyword_returns_module,
    test_capitalized_dot_access,
    test_module_shadowing_a_constructor,
    test_constructor_still_applies,
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
    /* Function shorthand members */
    test_shorthand_member_exported,
    test_shorthand_member_forms,
    test_shorthand_member_signature,
    test_shorthand_member_signature_return_type,
    test_shorthand_member_shadowed,
    test_shorthand_parameter_not_member,
    test_shorthand_member_mismatch_on_definition,
    /* Cyclic member paths */
    test_cyclic_member_path_let,
    test_cyclic_member_path_module,
  ],
);

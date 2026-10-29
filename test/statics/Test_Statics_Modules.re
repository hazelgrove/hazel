open Test_Statics_Prelude;
open Language;
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

/* ===== SIGNATURE ANNOTATION TESTS (well-typed) ===== */

/* Sec 12: Empty module with matching empty sig annotation */
let test_empty_sig_annotation =
  fully_consistent_typecheck(
    "Empty module with empty sig annotation",
    {|let s1 : {} = {} in s1|},
    Some(prod([])),
  );

/* Sec 13: Module with matching sig annotation */
let test_matching_sig_annotation =
  fully_consistent_typecheck(
    "Module with matching sig annotation",
    {|let s1 : { let x : Int } = { let x = 42 } in s1|},
    Some(prod([tup_label(label("x"), int())])),
  );

/* Sec 13b: Multi-member matching sig annotation */
let test_matching_sig_multi =
  fully_consistent_typecheck(
    "Module with multi-member matching sig annotation",
    {|let m : { let x : Int; let y : Bool } = { let x = 1; let y = true } in m|},
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

/* Sec 14: Module member type doesn't match sig annotation */
let test_error_sig_type_mismatch =
  inconsistent_typecheck(
    "Module member type mismatch with sig annotation",
    {|let annotated : { let x : Int } = { let x = true } in annotated|}
    |> parse_exp,
  );

/* Sec 15: Module with matching and non-matching members */
let test_error_sig_partial_mismatch =
  inconsistent_typecheck(
    "Sig annotation with one matching and one mismatched member",
    {|let annotated : { let x : Int; let y : Bool } = { let x = 1; let y = 2 } in annotated|}
    |> parse_exp,
  );

/* Sec 16: Module missing members (sig wider than module) */
let test_error_sig_too_wide =
  inconsistent_typecheck(
    "Module missing members required by sig",
    {|let annotated : { let x : Int; let y : Bool; let z : String } = { let x = 1; let y = true } in annotated|}
    |> parse_exp,
  );

/* Sec 17: Singleton module missing a member (sig wider) */
let test_error_sig_too_wide_singleton =
  inconsistent_typecheck(
    "Singleton module missing member required by sig",
    {|let annotated : { let x : Int; let y : Bool } = { let x = 1 } in annotated|}
    |> parse_exp,
  );

/* Sec 18: Empty module missing all members */
let test_error_sig_too_wide_empty =
  inconsistent_typecheck(
    "Empty module missing all members required by sig",
    {|let annotated : { let x : Int; let y : Bool } = {} in annotated|}
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

/* ===== LIMITATIONS: Module/Tuple equivalence =====
   Currently modules are a sugar for labeled tuples. These tests document
   behaviors that are expected to change when full module types (Sig) are
   implemented. See plans/modules.md Phase 2.1. */

/* Sec 20: Module and tuple types are interchangeable (limitation) */
let test_limitation_sig_tuple_compat_mt =
  fully_consistent_typecheck(
    "Limitation: sig annotation accepts labeled tuple value",
    {|let mt : { let x : Int } = (x=1) in mt|},
    Some(prod([tup_label(label("x"), int())])),
  );

let test_limitation_sig_tuple_compat_tm =
  fully_consistent_typecheck(
    "Limitation: prod annotation accepts module value",
    {|let tm : (x=Int) = { let x = 1 } in tm|},
    Some(prod([tup_label(label("x"), int())])),
  );

/* Sec 21: Cross-type errors work regardless of module/tuple mix */
let test_limitation_cross_error_mt =
  inconsistent_typecheck(
    "Limitation: sig annotation catches tuple type error",
    {|let mt : { let x : Int } = (x=true) in mt|} |> parse_exp,
  );

let test_limitation_cross_error_tm =
  inconsistent_typecheck(
    "Limitation: prod annotation catches module type error",
    {|let tm : (x=Int) = { let x = true } in tm|} |> parse_exp,
  );

/* Sec 22-24: Precise width matching required (limitation).
   With full modules, extra members should be allowed (open meet).
   Currently, width mismatches are errors because modules desugar to Prods. */
let test_limitation_extra_member =
  inconsistent_typecheck(
    "Limitation: extra module member errors (should pass with full modules)",
    {|let annotated : {} = { let x = 1 } in annotated|} |> parse_exp,
  );

let test_limitation_extra_member_multi =
  inconsistent_typecheck(
    "Limitation: extra member in multi-member module errors",
    {|let annotated : { let x : Int } = { let x = 1; let y = 2 } in annotated|}
    |> parse_exp,
  );

let test_limitation_sig_too_narrow =
  inconsistent_typecheck(
    "Limitation: sig narrower than module errors",
    {|let annotated : { let x : Int; let y : Bool } = { let x = 1; let y = true; let z = "hello" } in annotated|}
    |> parse_exp,
  );

/* Sec 26: Label mismatch with hole type produces no error (limitation) */
let test_limitation_label_mismatch_hole =
  fully_consistent_typecheck(
    "Limitation: label mismatch with hole type has no error",
    {|let m : { let x : ? } = { let y = 1 } in m|},
    Some(prod([tup_label(label("x"), unknown(Hole(EmptyHole)))])),
  );

/* ===== MODULE KEYWORD TESTS ===== */

/* The id of the inner F in `module F = fun x -> F(x) in 1`, with or
   without an ascription on the module's name */
let self_reference_id = (exp: Exp.t): Id.t =>
  switch (IdTagged.term_of(exp)) {
  | ModuleExp(_, {term: Fun(_, {term: Ap(_, f, _), _}, _, _), _}, _) =>
    IdTagged.rep_id(f)
  | _ => Alcotest.fail("expected `module F = fun x -> F(x) in ...`")
  };

/* A module definition that names its own module: modules are not
   recursive, so the name is unbound inside it. The one error is at the
   inner F, which reads as a constructor that is not defined. */
let module_names_itself = (name, source) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(source);
      Alcotest.(check(list(pair(string, testable_issue))))(
        "Static errors",
        [
          (Id.show(self_reference_id(exp)), Marks([FreeConstructor("F")])),
        ],
        errors(statics(exp))
        |> List.map(((id, ms)) => (Id.show(id), Marks(ms))),
      );
    },
  );

let test_module_names_itself =
  module_names_itself(
    "A module definition that names its own module is an error",
    {|module F = fun x -> F(x) in 1|},
  );

let test_ascribed_module_names_itself =
  module_names_itself(
    "An ascribed module definition that names its own module is an error",
    {|module F : Int -> Int = fun x -> F(x) in 1|},
  );

/* Inside its own definition, a module's name is not in scope */
let test_module_name_not_in_scope_in_def =
  Alcotest.test_case(
    "A module's name is not in scope in its definition",
    `Quick,
    () => {
      let exp = parse_exp({|module F = fun x -> ? in 1|});
      let hole_id =
        switch (IdTagged.term_of(exp)) {
        | ModuleExp(_, {term: Fun(_, hole, _, _), _}, _) =>
          IdTagged.rep_id(hole)
        | _ => Alcotest.fail("expected `module F = fun x -> ? in ...`")
        };
      let hole_ctx =
        switch (Statics.Map.lookup(hole_id, statics(exp))) {
        | Some(info) => Info.ctx_of(info)
        | None => Alcotest.fail("no info at the hole")
        };
      Alcotest.(check(bool))(
        "F is unbound at the hole",
        true,
        Option.is_none(Ctx.lookup_var(hole_ctx, "F")),
      );
      Alcotest.(check(bool))(
        "x is bound at the hole",
        true,
        Option.is_some(Ctx.lookup_var(hole_ctx, "x")),
      );
    },
  );

/* A module bound to a fixpoint the user wrote (Alexander's first program
   on #2651). Its definition elaborates to a FixF, the shape a recursive
   Let gives a definition that names its own variable, but here f is the
   fixpoint's own variable: no errors. The fixpoint is unannotated, so the
   type is unknown. */
let test_module_fix =
  fully_consistent_typecheck(
    "A module bound to a fixpoint has no errors",
    {|module m = fix f -> fun n -> if n == 0 then 1 else n * f(n - 1) in m(3)|},
    Some(unknown(Internal)),
  );

/* A fixpoint whose body names the module, not the fixpoint's variable
   (Alexander's second program on #2651): the module's name is unbound in
   its definition, so the one error is at the inner m, a free variable */
let test_module_fix_names_itself =
  Alcotest.test_case(
    "A module bound to a fixpoint that names the module is an error",
    `Quick,
    () => {
      let exp =
        parse_exp(
          {|module m = fix g -> fun n -> if n == 0 then 0 else m(n - 1) in m(3)|},
        );
      let inner_m =
        switch (IdTagged.term_of(exp)) {
        | ModuleExp(
            _,
            {
              term:
                FixF(
                  _,
                  {
                    term:
                      Fun(
                        _,
                        {term: If(_, _, {term: Ap(_, m, _), _}), _},
                        _,
                        _,
                      ),
                    _,
                  },
                  _,
                ),
              _,
            },
            _,
          ) =>
          IdTagged.rep_id(m)
        | _ =>
          Alcotest.fail(
            "expected `module m = fix g -> fun n -> if ... else m(...) in ...`",
          )
        };
      Alcotest.(check(list(pair(string, testable_issue))))(
        "Static errors",
        [(Id.show(inner_m), Marks([Free("m")]))],
        errors(statics(exp))
        |> List.map(((id, ms)) => (Id.show(id), Marks(ms))),
      );
    },
  );

/* A module whose definition names the module it shadows */
let shadows_module_source = {|module F = fun (x : Int) -> x > 0 in module F = fun x -> F(x) in F(1)|};

/* F(1) is a Bool only when the inner F is the outer one */
let test_module_shadows_module =
  fully_consistent_typecheck(
    "A module definition names the module it shadows",
    shadows_module_source,
    Some(bool()),
  );

/* The second definition's info: its ctx binds F to the outer module, and
   its co_ctx, which an item's dependencies are read from, has F as a free
   use */
let test_module_shadows_module_info =
  Alcotest.test_case(
    "A module definition's info binds the module it shadows",
    `Quick,
    () => {
      let exp = parse_exp(shadows_module_source);
      let (outer_id, def, inner_f) =
        switch (IdTagged.term_of(exp)) {
        | ModuleExp(
            outer,
            _,
            {
              term:
                ModuleExp(
                  _,
                  {term: Fun(_, {term: Ap(_, f, _), _}, _, _), _} as def,
                  _,
                ),
              _,
            },
          ) => (
            IdTagged.rep_id(outer),
            def,
            f,
          )
        | _ =>
          Alcotest.fail(
            "expected `module F = ... in module F = fun x -> F(x) in ...`",
          )
        };
      let m = statics(exp);
      let inner_f_ctx =
        switch (Statics.Map.lookup(IdTagged.rep_id(inner_f), m)) {
        | Some(info) => Info.ctx_of(info)
        | None => Alcotest.fail("no info at the inner F")
        };
      Alcotest.(check(option(string)))(
        "F at the inner reference is the outer module",
        Some(Id.show(outer_id)),
        Ctx.lookup_var(inner_f_ctx, "F")
        |> Option.map((entry: Ctx.var_entry) => Id.show(entry.id)),
      );
      let def_co_ctx =
        switch (Statics.Map.lookup_exp(IdTagged.rep_id(def), m)) {
        | Some(info) => info.co_ctx
        | None => Alcotest.fail("no info at the second definition")
        };
      Alcotest.(check(bool))(
        "F is free in the second definition",
        true,
        CoCtx.has_any(def_co_ctx, ["F"]),
      );
    },
  );

/* A `module` item in braces lowers to a plain Let, which is recursive, so
   an item that names itself is not an error, unlike a `module ... in`
   binding. This pins that split as it is, until it is decided whether
   braces items are recursive. */
let test_module_item_names_itself =
  fully_consistent_typecheck(
    "A module item in braces that names itself is still recursive",
    {|module Outer = { module G = fun n -> if n == 0 then 7 else G(n - 1) } in Outer.G(3)|},
    Some(int()),
  );

/* Statics of `code`, which must have no errors and synthesize `expected`,
   in under `seconds` */
let checks_within = (name, ~seconds, code, expected) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(code);
      let t0 = Sys.time();
      let s = statics(exp);
      let dt = Sys.time() -. t0;
      Alcotest.(check(list(testable_issue)))(
        "Static errors",
        [],
        List.map(((_, ms)) => Marks(ms), errors(s)),
      );
      Alcotest.check(
        Alcotest.option(testable_typ),
        code,
        expected,
        synthesized_type(s, exp),
      );
      Alcotest.(check(bool))(
        Printf.sprintf("took %.1f s", dt),
        true,
        dt < seconds,
      );
    },
  );

/* module F0 = fun x : Int -> module F1 = fun x : Int -> ... in F1(x) in
   F0(1): n module-bound functions, each holding the next. A module
   analyzes its definition once, so this is n analyses; as recursive lets,
   with the probe and three passes each, it would be 4^n. */
let nested_function_modules = n => {
  let rec go = i =>
    i > n
      ? "x"
      : Printf.sprintf(
          "module F%d = fun x : Int -> %s in F%d(x)",
          i,
          go(i + 1),
          i,
        );
  "module F0 = fun x : Int -> " ++ go(1) ++ " in F0(1)";
};

/* The same chain with each module ascribed Int -> (a = Int), so each
   module binding is analyzed against a one-label product and goes through
   the singleton-label pre-pass, twice: the module's, then its Let's. That
   is three analyses per module, 3^n; with a recursive Let pre-pass it
   would be six, 6^n. */
let nested_labeled_function_modules = n => {
  let rec go = i =>
    i > n
      ? "(a = x)"
      : Printf.sprintf(
          "module F%d : Int -> (a = Int) = fun x -> %s in F%d(x)",
          i,
          go(i + 1),
          i,
        );
  "module F0 : Int -> (a = Int) = fun x -> " ++ go(1) ++ " in F0(1).a";
};

let test_nested_function_modules =
  checks_within(
    "Nine nested function modules check in under 5 s",
    ~seconds=5.,
    nested_function_modules(9),
    Some(int()),
  );

let test_nested_labeled_function_modules =
  checks_within(
    "Seven nested function modules at one-label types check in under 10 s",
    ~seconds=10.,
    nested_labeled_function_modules(7),
    Some(int()),
  );

/* module F0 = fix f0 -> fun x -> if x == 0 then module F1 = fix f1 ->
   ... in F1(x) else f0(x - 1) in F0(1): n modules, each bound to a
   fixpoint the user wrote, each holding the next in its then branch. A
   module analyzes its definition once, so this is n analyses. Told apart
   by its elaboration's shape, a FixF, each definition would be taken for
   one that names its module and analyzed again: with the let's recursion
   probe, three analyses per module, 3^n. The probe alone would be 2^n. */
let nested_fix_modules = n => {
  let rec go = i =>
    i > n
      ? "x"
      : Printf.sprintf(
          "module F%d = fix f%d -> fun x -> if x == 0 then %s else f%d(x - 1) in F%d(x)",
          i,
          i,
          go(i + 1),
          i,
          i,
        );
  "module F0 = fix f0 -> fun x -> if x == 0 then "
  ++ go(1)
  ++ " else f0(x - 1) in F0(1)";
};

let test_nested_fix_modules =
  checks_within(
    "Twelve nested modules bound to fixpoints check in under 0.5 s",
    ~seconds=0.5,
    nested_fix_modules(12),
    Some(unknown(Internal)),
  );

/* A let inside a module's definition is recursive as usual */
let test_module_member_recursive =
  fully_consistent_typecheck(
    "A module member function can call itself",
    {|module M = { let f = fun n -> if n == 0 then 7 else f(n - 1) } in M.f(3)|},
    Some(int()),
  );

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
      prod([
        tup_label(label("Inner"), prod([tup_label(label("z"), int())])),
        tup_label(label("r"), int()),
      ]),
    ),
  );

/* Test module keyword returning the module itself */
let test_module_keyword_returns_module =
  fully_consistent_typecheck(
    "Module keyword returns module value",
    {|module M = { let a = 1; let b = true } in M|},
    Some(
      prod([tup_label(label("a"), int()), tup_label(label("b"), bool())]),
    ),
  );

/* Test capitalized name in dot position (chained access) */
let test_capitalized_dot_access =
  fully_consistent_typecheck(
    "Capitalized name in dot position",
    {|module Outer = { module Inner = { let x = 10 } } in Outer.Inner.x|},
    Some(int()),
  );

/* Test module keyword with prod annotation */
let test_module_keyword_prod_annotation =
  fully_consistent_typecheck(
    "Module keyword with prod annotation",
    {|module M : (x=Int) = { let x = 1 } in M.x|},
    Some(int()),
  );

/* Test module keyword with sig annotation */
let test_module_keyword_sig_annotation =
  fully_consistent_typecheck(
    "Module keyword with sig annotation",
    {|module M : { let x : Int } = { let x = 42 } in M.x|},
    Some(int()),
  );

/* Test module keyword with multi-field annotation */
let test_module_keyword_multi_annotation =
  fully_consistent_typecheck(
    "Module keyword with multi-field annotation",
    {|module M : (x=Int, y=Bool) = { let x = 1; let y = true } in M|},
    Some(
      prod([tup_label(label("x"), int()), tup_label(label("y"), bool())]),
    ),
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

/* ===== QUALIFIED TYPE ACCESS TESTS (Phase 1) ===== */
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

/* Type shadowing in module (last wins) — has ShadowsType diagnostic */
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

/* ===== QUALIFIED TYPE ACCESS: ALIASING TESTS (Phase 1b) ===== */

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
    /* Well-typed tests (Sections A, B) */
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
    /* Sig annotation tests (Section B) */
    test_empty_sig_annotation,
    test_matching_sig_annotation,
    test_matching_sig_multi,
    /* Type error tests (Sections B errors) */
    test_error_type_mismatch,
    test_error_type_mismatch_multi,
    test_error_sig_type_mismatch,
    test_error_sig_partial_mismatch,
    test_error_sig_too_wide,
    test_error_sig_too_wide_singleton,
    test_error_sig_too_wide_empty,
    test_error_wrong_type_in_body,
    test_error_free_variable,
    test_error_nested_type_mismatch,
    test_error_bad_field_access,
    test_error_bad_field_via_var,
    test_error_field_type_mismatch,
    test_error_binding_annotation_mismatch,
    test_error_sequential_type,
    /* Limitation tests (Section C) — expected to change with full modules */
    test_limitation_sig_tuple_compat_mt,
    test_limitation_sig_tuple_compat_tm,
    test_limitation_cross_error_mt,
    test_limitation_cross_error_tm,
    test_limitation_extra_member,
    test_limitation_extra_member_multi,
    test_limitation_sig_too_narrow,
    test_limitation_label_mismatch_hole,
    /* Module keyword tests */
    test_module_names_itself,
    test_ascribed_module_names_itself,
    test_module_name_not_in_scope_in_def,
    test_module_fix,
    test_module_fix_names_itself,
    test_module_shadows_module,
    test_module_shadows_module_info,
    test_module_member_recursive,
    test_module_item_names_itself,
    test_nested_function_modules,
    test_nested_labeled_function_modules,
    test_nested_fix_modules,
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
    /* Qualified type access tests (Phase 1) */
    test_qualified_type_basic,
    test_qualified_type_multiple,
    test_qualified_type_internal_ref,
    test_qualified_type_nested,
    test_qualified_type_lowercase,
    test_qualified_type_shadowing,
    test_error_qualified_type_unknown,
    /* Qualified type access aliasing tests (Phase 1b) */
    test_qualified_type_var_alias,
    test_qualified_type_module_alias,
    test_qualified_type_chained_alias,
    test_qualified_type_nested_with_values,
    test_qualified_type_nested_alias,
    test_qualified_type_nested_alias_with_sibling,
    /* Type alias shadowing tests (Issue 5) */
    test_type_alias_shadowing_in_nested_module,
    test_type_alias_shadowing_sequential,
    test_type_alias_shadowing_nested_module,
  ],
);

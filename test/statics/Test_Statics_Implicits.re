open Alcotest;
open Language;
open Test_Statics_Prelude;
module T = FTemp.Typ;

/* Modular implicits: `implicit S : SIG` components of a function parameter
   are resolved at applications from the implicit instances in scope (see
   Implicits.re). Expected types are written with FTemp builders. */

let show_prelude = {|type SHOW = { type T; let show : T -> String } in
let implicit ShowInt = { type T = Int; let show = string_of_int } in
let implicit ShowBool = { type T = Bool; let show = fun b -> if b then "true" else "false" } in
let show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in
|};
let with_show = body => show_prelude ++ body;

let is_ambiguous = (binder, candidates) =>
  fun
  | Mark.ImplicitAmbiguous({binder: b, candidates: c, _}) =>
    b == binder && c == candidates
  | _ => false;
let is_not_found = (binder, candidates) =>
  fun
  | Mark.ImplicitNotFound({binder: b, candidates: c, _}) =>
    b == binder && c == candidates
  | _ => false;

/* No unused-variable (or other) warning anywhere in the program. */
let no_warnings_test = (name, source) =>
  test_case(
    name,
    `Quick,
    () => {
      let warnings =
        statics(parse_exp(source))
        |> Id.Map.fold(
             (_, info: Info.t, acc) =>
               switch (info) {
               | InfoExp({warnings, _})
               | InfoPat({warnings, _}) => warnings @ acc
               | _ => acc
               },
             _,
             [],
           );
      check(int, name, 0, List.length(warnings));
    },
  );

/* ===== THE CALLER'S TYPE EQUATION ===== */

/* One program, twice, differing only in the keyword on the module
   parameter. The implicit binder is named in the function's type, so the
   equation the caller establishes (S is ShowInt, so S.T is Int) survives
   into the parameter and the result. */
let equation_kept = expected =>
  {|type SHOW = {type T; let show : T -> String} in
let implicit ShowInt = {type T = Int; let show = string_of_int} in
let dup = fun (implicit S : SHOW, x : S.T) -> (x, x) in
let out : |}
  ++ expected
  ++ {| = dup(ShowInt, 3) in
out|};

/* A plain module parameter cannot be named in a function's type, so part 4's
   avoidance closes every path rooted at it: the parameter that mentions it
   and the result both escape. The equation is still lost, but no longer
   silently: the argument is checked against an escaped abstract type, so the
   call is an error naming it. What would recover the equation instead of
   reporting its loss is a dependent domain for ordinary module parameters,
   or an ML sharing constraint. */
let equation_lost = {|type SHOW = {type T; let show : T -> String} in
module ShowInt = {type T = Int; let show = string_of_int} in
let dup = fun (m : SHOW, x : m.T) -> (x, x) in
let out : (String, String) = dup(ShowInt, 3) in
out|};

let test_equation_kept =
  fully_consistent_typecheck(
    "An implicit binder keeps the caller's type equation",
    equation_kept("(Int, Int)"),
    Some(T.prod([T.int(), T.int()])),
  );

let test_equation_kept_rejects =
  inconsistent_typecheck(
    "The kept equation rejects a wrong annotation",
    parse_exp(equation_kept("(String, String)")),
  );

let test_equation_lost_by_plain_parameter =
  has_mark_test(
    "A plain module parameter loses the equation, and says so",
    equation_lost,
    fun
    | Mark.EscapedType({path, side}) => path == "m.T" && side == Required
    | _ => false,
  );

/* ===== RESOLUTION ===== */

let test_resolves_by_argument =
  fully_consistent_typecheck(
    "An instance is resolved by the argument's type",
    with_show({|show(3) ++ show(true)|}),
    Some(T.string()),
  );

let test_explicit_passing =
  fully_consistent_typecheck(
    "A call whose arity includes the implicit passes it explicitly",
    with_show({|show(ShowBool, true) ++ show(ShowInt, 3)|}),
    Some(T.string()),
  );

let test_explicit_mismatch =
  inconsistent_typecheck(
    "An explicitly passed instance still types the value argument",
    parse_exp(with_show({|show(ShowBool, 3)|})),
  );

let test_ambiguous =
  single_mark_test(
    "Two fitting instances are ambiguous",
    with_show(
      {|let name = fun (implicit S : SHOW, n : Int) -> n in name(3)|},
    ),
    is_ambiguous("S", ["ShowBool", "ShowInt"]),
  );

let test_not_found =
  single_mark_test(
    "No fitting instance is an error on the application",
    with_show({|show("s")|}),
    is_not_found("S", ["ShowBool", "ShowInt"]),
  );

/* The type members the failure says the instance would have to have. */
let requires = (binder, constraints) =>
  fun
  | Mark.ImplicitNotFound({binder: b, constraints: cs, _}) =>
    b == binder
    && List.length(cs) == List.length(constraints)
    && List.for_all2(
         ((n1, t1), (n2, t2)) => n1 == n2 && Typ.fast_equal(t1, t2),
         cs,
         constraints,
       )
  | _ => false;

let test_not_found_names_required_member =
  single_mark_test(
    "A missing instance names the member the argument requires",
    with_show({|show("s")|}),
    requires("S", [("T", T.string())]),
  );

let default_prelude = {|type DEF = { type T; let default : T } in
let implicit DefInt = { type T = Int; let default = 0 } in
let implicit DefBool = { type T = Bool; let default = true } in
let default = fun implicit D : DEF -> D.default in
|};

let test_not_found_names_member_from_expectation =
  single_mark_test(
    "A missing instance names the member the expected type requires",
    default_prelude ++ {|let d : String = default() in d|},
    requires("D", [("T", T.string())]),
  );

/* The rendered message, which both the CLI and the inspector build from the
   mark: it names the signature and the required members, and names no
   signature at all for an unannotated binder (whose signature is unknown
   and used to render as a blank). */
let message_test = (name, source, expected) =>
  test_case(name, `Quick, () =>
    check(
      bool,
      expected,
      true,
      Haz3lcore.ErrorPrint.all(statics(parse_exp(source)))
      |> List.exists(Util.StringUtil.plain_match(expected)),
    )
  );

let test_not_found_message =
  message_test(
    "The message names the signature and the required member",
    with_show({|show("s")|}),
    "No implicit instance of SHOW with T = String for S",
  );

let test_not_found_message_unannotated =
  message_test(
    "The message of an unannotated binder names no signature",
    {|let f = fun (implicit S, x) -> x in f(3)|},
    "No implicit instance for S",
  );

/* The marks sit on the application node (the whole call), not on the
   function or the argument. */
let marks_on_application_test = (name, source, f, pred) =>
  test_case(name, `Quick, () =>
    check(
      bool,
      name,
      true,
      subexp_marks(
        source,
        fun
        | Ap(_, {term: Var(x), _}, _) => x == f
        | _ => false,
      )
      |> List.exists(pred),
    )
  );

let test_ambiguous_on_application =
  marks_on_application_test(
    "An ambiguity is reported on the application",
    with_show(
      {|let name = fun (implicit S : SHOW, n : Int) -> n in name(3)|},
    ),
    "name",
    is_ambiguous("S", ["ShowBool", "ShowInt"]),
  );

let test_not_found_on_application =
  marks_on_application_test(
    "A missing instance is reported on the application",
    with_show({|show("s")|}),
    "show",
    is_not_found("S", ["ShowBool", "ShowInt"]),
  );

let test_hole_argument_ambiguous =
  single_mark_test(
    "A hole argument fits every instance",
    with_show({|show(?)|}),
    is_ambiguous("S", ["ShowBool", "ShowInt"]),
  );

let test_nested_resolves_to_parameter =
  fully_consistent_typecheck(
    "Inside an implicit function the parameter is an instance",
    with_show(
      {|let twice = fun (implicit S : SHOW, x : S.T) -> show(x) ++ show(x) in twice(3)|},
    ),
    Some(T.string()),
  );

let test_only_implicit_parameter =
  fully_consistent_typecheck(
    "A function with only an implicit parameter is called with ()",
    {|type NAMED = { let name : String } in let implicit N = { let name = "n" } in let f = fun implicit X : NAMED -> X.name in f()|},
    Some(T.string()),
  );

let test_resolves_by_expected_type =
  fully_consistent_typecheck(
    "The expected type narrows the candidates",
    {|type DEF = { type T; let default : T } in let implicit DI = { type T = Int; let default = 0 } in let implicit DB = { type T = Bool; let default = true } in let default = fun implicit D : DEF -> D.default in let x : Int = default() in x|},
    Some(T.int()),
  );

let test_module_body_instance =
  fully_consistent_typecheck(
    "An instance declared in a module body scopes over the later items",
    {|type SHOW = { type T; let show : T -> String } in let show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in { let implicit si = { type T = Int; let show = string_of_int }; let r = show(3) }.r|},
    Some(T.string()),
  );

let test_function_shorthand =
  fully_consistent_typecheck(
    "The function shorthand takes implicit components",
    {|type SHOW = { type T; let show : T -> String } in let implicit ShowInt = { type T = Int; let show = string_of_int } in let show(implicit S : SHOW, x : S.T) = S.show(x) in show(3)|},
    Some(T.string()),
  );

let test_mixed_arity_by_signature =
  fully_consistent_typecheck(
    "With two implicits, a passed module fills the one it fits",
    with_show(
      {|type NAMED = { let name : String } in let implicit N1 = { let name = "a" } in let both = fun (implicit S : SHOW, implicit N : NAMED, x : S.T) -> S.show(x) ++ N.name in both(N1, 3)|},
    ),
    Some(T.string()),
  );

let test_arity_mismatch =
  inconsistent_typecheck(
    "Too many arguments is an ordinary shape mismatch",
    parse_exp(with_show({|show(1, 2, 3)|})),
  );

let test_shadowed_instance_excluded =
  single_mark_test(
    "A shadowed instance is no longer a candidate",
    {|type SHOW = { type T; let show : T -> String } in let implicit si = { type T = Int; let show = string_of_int } in let show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in let si = 3 in show(3)|},
    is_not_found("S", []),
  );

let test_sealed_instance_not_selected =
  inconsistent_typecheck(
    "A sealed instance's abstract member does not fit an Int argument",
    parse_exp(
      {|type SHOW = { type T; let show : T -> String } in let implicit ss : SHOW = { type T = Int; let show = string_of_int } in let show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in show(3)|},
    ),
  );

let test_nonliteral_tuple_argument =
  fully_consistent_typecheck(
    "A tuple-typed variable supplies the explicit components",
    with_show(
      {|let f = fun (implicit S : SHOW, a : S.T, n : Int) -> S.show(a) ++ string_of_int(n) in let pair = (true, 2) in f(pair)|},
    ),
    Some(T.string()),
  );

let test_member_implicit_function =
  fully_consistent_typecheck(
    "An implicit function reached through a module member",
    with_show(
      {|module Lib = { let render = fun (implicit S : SHOW, x : S.T) -> S.show(x) } in Lib.render(true)|},
    ),
    Some(T.string()),
  );

let test_pipeline =
  fully_consistent_typecheck(
    "A pipeline resolves like an application",
    with_show({|3 |> show|}),
    Some(T.string()),
  );

let test_recursive_implicit_function =
  fully_consistent_typecheck(
    "A recursive call inside the body resolves to the parameter",
    with_show(
      {|let count = fun (implicit S : SHOW, x : S.T, n : Int) -> if n == 0 then S.show(x) else count(x, n - 1) in count(3, 2)|},
    ),
    Some(T.string()),
  );

let test_tuple_typed_single_component =
  fully_consistent_typecheck(
    "A literal tuple is the one explicit component when it is tuple-typed",
    with_show(
      {|let g = fun (implicit S : SHOW, p : (S.T, Int)) -> case p | (x, n) => S.show(x) end in g((true, 1))|},
    ),
    Some(T.string()),
  );

let test_non_module_is_not_passed =
  single_mark_test(
    "A non-module in an implicit's position is not taken as the instance",
    with_show({|show(3, 4)|}),
    is_not_found("S", ["ShowBool", "ShowInt"]),
  );

/* An unannotated implicit binder has unknown type, so it may be a module:
   a path through it is not an error. */
let test_unannotated_binder_path =
  Alcotest.test_case(
    "A path through an unannotated implicit binder is not an error", `Quick, () =>
    Alcotest.(check(bool))(
      "no TypWantModule",
      false,
      statics(parse_exp({|let f = fun (implicit S, x : S.T) -> x in f|}))
      |> errors
      |> List.concat_map(snd)
      |> List.exists(
           fun
           | Mark.TypWantModule(_) => true
           | _ => false,
         ),
    )
  );

/* ===== TYPES ===== */

let test_annotation_form =
  fully_consistent_typecheck(
    "The annotation form types an implicit function",
    with_show(
      {|let show2 : (implicit S : SHOW, S.T) -> String = show in show2(3)|},
    ),
    Some(T.string()),
  );

let test_alpha_binders =
  fully_consistent_typecheck(
    "Binder names are renamed positionally",
    with_show({|let show2 : (implicit R : SHOW, R.T) -> String = show in 1|}),
    Some(T.int()),
  );

let test_implicit_vs_explicit_shape =
  inconsistent_typecheck(
    "An implicit function is not an explicit one",
    parse_exp(with_show({|let g : (SHOW, Int) -> String = show in 1|})),
  );

let test_non_module_instance =
  single_mark_test(
    "An instance must be a module",
    {|let implicit x : Int = 3 in x|},
    fun
    | Mark.ImplicitBinderNotModule(_) => true
    | _ => false,
  );

let test_misplaced_binder_type =
  single_mark_test(
    "An implicit binder type belongs in an arrow domain",
    {|let f : (implicit S : { let n : Int }) = ? in 1|},
    fun
    | Mark.ImplicitBinderPosition => true
    | _ => false,
  );

/* ===== INFO MAP ===== */

let test_ids_preserved_resolved =
  Test_Statics_FunctionSugar.info_map_preserves_ids(
    "Every surface id survives a resolved application",
    with_show({|show(3)|}),
  );

let test_ids_preserved_explicit =
  Test_Statics_FunctionSugar.info_map_preserves_ids(
    "Every surface id survives an explicit application",
    with_show({|show(ShowBool, true)|}),
  );

let test_instances_used =
  no_warnings_test(
    "An instance used only through resolution is not unused",
    with_show({|show(3) ++ show(true)|}),
  );

let tests = (
  "Statics.Implicits",
  [
    test_resolves_by_argument,
    test_explicit_passing,
    test_explicit_mismatch,
    test_ambiguous,
    test_equation_kept,
    test_equation_kept_rejects,
    test_equation_lost_by_plain_parameter,
    test_not_found,
    test_not_found_names_required_member,
    test_not_found_names_member_from_expectation,
    test_not_found_message,
    test_not_found_message_unannotated,
    test_ambiguous_on_application,
    test_not_found_on_application,
    test_hole_argument_ambiguous,
    test_nested_resolves_to_parameter,
    test_only_implicit_parameter,
    test_resolves_by_expected_type,
    test_module_body_instance,
    test_function_shorthand,
    test_mixed_arity_by_signature,
    test_arity_mismatch,
    test_shadowed_instance_excluded,
    test_sealed_instance_not_selected,
    test_nonliteral_tuple_argument,
    test_member_implicit_function,
    test_pipeline,
    test_recursive_implicit_function,
    test_tuple_typed_single_component,
    test_non_module_is_not_passed,
    test_unannotated_binder_path,
    test_annotation_form,
    test_alpha_binders,
    test_implicit_vs_explicit_shape,
    test_non_module_instance,
    test_misplaced_binder_type,
    test_ids_preserved_resolved,
    test_ids_preserved_explicit,
    test_instances_used,
  ],
);

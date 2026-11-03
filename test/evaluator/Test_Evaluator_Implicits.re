open Alcotest;
open Test_Evaluator_Prelude;

/* Modular implicits at runtime: the resolved instance is spliced into the
   elaborated argument, so evaluation sees an ordinary application. */

let show_prelude = {|type SHOW = { type T; let show : T -> String } in
let implicit ShowInt = { type T = Int; let show = string_of_int } in
let implicit ShowBool = { type T = Bool; let show = fun b -> if b then "true" else "false" } in
let show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in
|};
let with_show = body => show_prelude ++ body;

let test_resolved =
  test_case("Resolved instances", `Quick, () => {
    parse_and_evaluate_test(
      {|"3true"|},
      with_show({|show(3) ++ show(true)|}),
    )
  });

let test_explicit =
  test_case("Explicitly passed instances", `Quick, () => {
    parse_and_evaluate_test(
      {|"true3"|},
      with_show({|show(ShowBool, true) ++ show(ShowInt, 3)|}),
    )
  });

let test_nested =
  test_case("A parameter used as an instance inside the body", `Quick, () => {
    parse_and_evaluate_test(
      {|"33"|},
      with_show(
        {|let twice = fun (implicit S : SHOW, x : S.T) -> show(x) ++ show(x) in twice(3)|},
      ),
    )
  });

let test_only_implicit =
  test_case("Only an implicit parameter", `Quick, () => {
    parse_and_evaluate_test(
      {|"n"|},
      {|type NAMED = { let name : String } in let implicit N = { let name = "n" } in let f = fun implicit X : NAMED -> X.name in f()|},
    )
  });

let test_expected_type =
  test_case("Resolved by the expected type", `Quick, () => {
    parse_and_evaluate_test(
      "0",
      {|type DEF = { type T; let default : T } in let implicit DI = { type T = Int; let default = 0 } in let implicit DB = { type T = Bool; let default = true } in let default = fun implicit D : DEF -> D.default in let x : Int = default() in x|},
    )
  });

let test_module_body_instance =
  test_case("An instance declared in a module body", `Quick, () => {
    parse_and_evaluate_test(
      {|"n3"|},
      {|type SHOW = { type T; let show : T -> String } in let show = fun (implicit S : SHOW, x : S.T) -> S.show(x) in { let implicit si = { type T = Int; let show = fun n -> "n" ++ string_of_int(n) }; let r = show(3) }.r|},
    )
  });

let test_shorthand =
  test_case("The function shorthand", `Quick, () => {
    parse_and_evaluate_test(
      {|"3"|},
      {|type SHOW = { type T; let show : T -> String } in let implicit ShowInt = { type T = Int; let show = string_of_int } in let show(implicit S : SHOW, x : S.T) = S.show(x) in show(3)|},
    )
  });

let test_annotation_form =
  test_case(
    "An implicit function ascribed with the annotation form", `Quick, () => {
    parse_and_evaluate_test(
      {|"3"|},
      with_show(
        {|let show2 : (implicit S : SHOW, S.T) -> String = show in show2(3)|},
      ),
    )
  });

let test_mixed_arity =
  test_case("Two implicits, one passed explicitly", `Quick, () => {
    parse_and_evaluate_test(
      {|"3a"|},
      with_show(
        {|type NAMED = { let name : String } in let implicit N1 = { let name = "a" } in let both = fun (implicit S : SHOW, implicit N : NAMED, x : S.T) -> S.show(x) ++ N.name in both(N1, 3)|},
      ),
    )
  });

let test_nonliteral_tuple_argument =
  test_case(
    "A tuple-typed variable is destructured around the instance", `Quick, () => {
    parse_and_evaluate_test(
      {|"true2"|},
      with_show(
        {|let f = fun (implicit S : SHOW, a : S.T, n : Int) -> S.show(a) ++ string_of_int(n) in let pair = (true, 2) in f(pair)|},
      ),
    )
  });

let test_recursive =
  test_case("A recursive implicit function", `Quick, () => {
    parse_and_evaluate_test(
      {|"3"|},
      with_show(
        {|let count = fun (implicit S : SHOW, x : S.T, n : Int) -> if n == 0 then S.show(x) else count(x, n - 1) in count(3, 2)|},
      ),
    )
  });

let test_member =
  test_case("An implicit function reached through a module member", `Quick, () => {
    parse_and_evaluate_test(
      {|"true"|},
      with_show(
        {|module Lib = { let render = fun (implicit S : SHOW, x : S.T) -> S.show(x) } in Lib.render(true)|},
      ),
    )
  });

let test_tuple_typed_single_component =
  test_case("A tuple-typed single explicit component", `Quick, () => {
    parse_and_evaluate_test(
      {|"true"|},
      with_show(
        {|let g = fun (implicit S : SHOW, p : (S.T, Int)) -> case p | (x, n) => S.show(x) end in g((true, 1))|},
      ),
    )
  });

let test_unresolved_evaluates =
  test_case(
    "An unresolved implicit evaluates to an indeterminate",
    `Quick,
    () => {
      let result = parse_and_evaluate(with_show({|show("s")|}));
      check(
        bool,
        "not a value",
        false,
        Language.ValueChecker.is_value(result),
      );
    },
  );

let test_preservation =
  test_case("Evaluation preserves the type of a resolved program", `Quick, () => {
    full_preservation_test(parse_exp(with_show({|show(3) ++ show(true)|})))
  });

let tests = (
  "Evaluator.Implicits",
  [
    test_resolved,
    test_explicit,
    test_nested,
    test_only_implicit,
    test_expected_type,
    test_module_body_instance,
    test_shorthand,
    test_annotation_form,
    test_mixed_arity,
    test_nonliteral_tuple_argument,
    test_recursive,
    test_member,
    test_tuple_typed_single_component,
    test_unresolved_evaluates,
    test_preservation,
  ],
);

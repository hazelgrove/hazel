open Alcotest;
open Test_Evaluator_Prelude;
open Language;

/* Kinds of the steps the stepper stops at (not the ones it takes itself). */
let shown_steps = (~show_ascription_steps, program: string) => {
  let settings = {
    ...CoreSettings.on,
    evaluation: {
      ...CoreSettings.on.evaluation,
      show_ascription_steps,
    },
  };
  let rec go = (n, exp, acc) =>
    n > 200
      ? acc
      : (
        switch (EvaluatorStep.get_status(~settings, exp, Environment.empty)) {
        | AutoStep(step) =>
          switch (EvaluatorStep.take_step(step)) {
          | Some(exp) => go(n + 1, exp, acc)
          | None => acc
          }
        | AvailableSteps([step, ..._]) =>
          switch (EvaluatorStep.take_step(step)) {
          | Some(exp) =>
            go(n + 1, exp, [EvaluatorStep.get_step_kind(step), ...acc])
          | None => acc
          }
        | AvailableSteps([]) => acc
        }
      );
  go(0, elaborate(parse_exp(program)), []);
};

let tests = (
  "Evaluator.TypAp",
  [
    test_case("Explicit polymorphism with type alias", `Quick, () =>
      parse_and_evaluate_test(
        "[5, 3]",
        {|type T=String in
let map = typfun a ->typfun b -> fun f :(a ->b), as : [a] -> let bs : [b] =
  case as
    | [] => []
    | (a :: as) => f(a) :: map@<a>@<b>(f, as)
  end
in bs in

map@<T>@<Int>(fun e -> string_length(e), ["hello","bar"])|},
      )
    ),
    /* Test_Statics_Polymorphism pins the type this result has to agree with. */
    test_case(
      "typfun binder shadows a type alias, so the body keeps `y` (#1717)",
      `Quick,
      () =>
      parse_and_evaluate_test(
        {|typfun y -> (?:y)|},
        {|type y = Int in typfun y -> (? : y)|},
      )
    ),
    /* Implicit polymorphism elaborates to hidden typfuns and @<…>s. */
    test_case("Generalized let used at two types", `Quick, () =>
      parse_and_evaluate_test(
        "(1, true)",
        {|let id = fun x -> x in (id(1), id(true))|},
      )
    ),
    test_case("Explicitly poly function applied without @<>", `Quick, () =>
      parse_and_evaluate_test(
        "3",
        {|let id : poly a -> a -> a = typfun a -> fun x : a -> x in id(3)|},
      )
    ),
    test_case(
      "The stepper takes implicit type applications itself",
      `Quick,
      () => {
        let program = {|let id = fun x -> x in id(1)|};
        check(
          bool,
          "hidden",
          false,
          List.mem(
            Transition.TypFunAp,
            shown_steps(~show_ascription_steps=false, program),
          ),
        );
        check(
          bool,
          "shown with ascription steps",
          true,
          List.mem(
            Transition.TypFunAp,
            shown_steps(~show_ascription_steps=true, program),
          ),
        );
      },
    ),
    test_case(
      "A generalized function used through ? still applies", `Quick, () =>
      parse_and_evaluate_test(
        "(1, 2)",
        {|let id = fun x -> x in
let g : ? = id in
(g(1), (fun (h : ?) -> h(2))(id))|},
      )
    ),
    test_case("Generalized recursive map", `Quick, () =>
      parse_and_evaluate_test(
        "([2, 3], [false])",
        {|let map = fun f -> fun xs ->
  case xs
    | [] => []
    | x :: t => f(x) :: map(f)(t)
  end
in (map(fun x -> x + 1)([1, 2]), map(fun b -> !b)([true]))|},
      )
    ),
  ],
);

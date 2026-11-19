open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Steps that substitute into a body (let, function application, case
   selection) record which variables they substitute, so the stepper can
   show them next to the step. */

let step_kind =
  testable(Fmt.using(Transition.show_step_kind, Fmt.string), (==));

/* The kinds of the steps the stepper takes on `s`, in order, always taking
   the first available step. */
let step_kinds = (s: string): list(Transition.step_kind) => {
  let rec go = (fuel, exp) =>
    if (fuel == 0) {
      [];
    } else {
      switch (
        EvaluatorStep.get_status(
          ~settings=CoreSettings.on,
          exp,
          Environment.empty,
        )
      ) {
      | AutoStep(step)
      | AvailableSteps([step, ..._]) => [
          EvaluatorStep.get_step_kind(step),
          ...switch (EvaluatorStep.take_step(step)) {
             | Some(exp) => go(fuel - 1, exp)
             | None => []
             },
        ]
      | AvailableSteps([]) => []
      };
    };
  go(200, elaborate(parse_exp(s)));
};

let first_kind = (p: Transition.step_kind => bool, s: string) =>
  switch (List.find_opt(p, step_kinds(s))) {
  | Some(k) => k
  | None => Alcotest.fail("No matching step in: " ++ s)
  };

let let_bind_of =
  first_kind(
    fun
    | Transition.LetBind(_) => true
    | _ => false,
  );
let fun_ap_of =
  first_kind(
    fun
    | Transition.FunAp(_) => true
    | _ => false,
  );
let case_apply_of =
  first_kind(
    fun
    | Transition.CaseApply(_) => true
    | _ => false,
  );

let check_step = (~kind, ~justification, ~hover_vars, k: Transition.step_kind) => {
  check(step_kind, "kind", kind, k);
  check(
    string,
    "justification",
    justification,
    Transition.stepper_justification(k),
  );
  check(
    list(string),
    "hover vars",
    hover_vars,
    Transition.stepper_justification_vars(k),
  );
};

let tests = (
  "Evaluator.StepSubstitution",
  [
    test_case("Let binding one variable names it", `Quick, () =>
      let_bind_of("let x = 1 in x + 1")
      |> check_step(
           ~kind=LetBind(["x"]),
           ~justification="substitution for x",
           ~hover_vars=[],
         )
    ),
    test_case("Let binding several variables lists them on hover", `Quick, () =>
      let_bind_of("let (a, b, c) = (1, 2, 3) in a + b + c")
      |> check_step(
           ~kind=LetBind(["a", "b", "c"]),
           ~justification="substitution for ...",
           ~hover_vars=["a", "b", "c"],
         )
    ),
    test_case("Let binding no variables", `Quick, () =>
      let_bind_of("let _ = 1 in 2")
      |> check_step(
           ~kind=LetBind([]),
           ~justification="substitution",
           ~hover_vars=[],
         )
    ),
    test_case("Function application lists its parameters", `Quick, () =>
      fun_ap_of("(fun (x, y) -> x + y)(1, 2)")
      |> check_step(
           ~kind=FunAp(["x", "y"]),
           ~justification="apply function",
           ~hover_vars=["x", "y"],
         )
    ),
    test_case("Case selection lists the matched rule's variables", `Quick, () =>
      case_apply_of("case [1, 2] | [] => 0 | hd :: tl => hd end")
      |> check_step(
           ~kind=CaseApply(["hd", "tl"]),
           ~justification="case selection",
           ~hover_vars=["hd", "tl"],
         )
    ),
    test_case("Case selection binding nothing has no hover", `Quick, () =>
      case_apply_of("case 1 | 1 => true | _ => false end")
      |> check_step(
           ~kind=CaseApply([]),
           ~justification="case selection",
           ~hover_vars=[],
         )
    ),
  ],
);

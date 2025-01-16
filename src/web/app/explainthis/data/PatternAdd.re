open Haz3lcore;
open ExplainThisForm;
open Example;

let single = (~p1_id: Id.t, ~p2_id: Id.t): Simple.t => {
  group_id: PatternAdd,
  form_id: PatternAdd,
  abstract:
    Simple.mk_2(("p1", p1_id), ("p2", p2_id), (p1', p2') =>
      [p1', space(), plus(), space(), p2']
    ),
  explanation: "Matches against integers by implicitly subtracting one of the sides of the addition. One side of the addition must be a constant.",
  examples: [
    {
      sub_id: PatternAdd1,
      term: mk_example("let x + 1 = 5 in x"),
      message: "Matches x + 1 against 5 yielding x = 4.",
    },
    {
      sub_id: PatternAdd2,
      term:
        mk_example(
          "let fact = fun n -> case n\n| 0 => 1\n| n + 1 => (n + 1) * fact(n)",
        ),
      message: "Matches n + 1 against 5 yielding x = 4.",
    },
  ],
};

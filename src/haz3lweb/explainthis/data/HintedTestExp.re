open Haz3lcore;
open Example;
open ExplainThisForm;
let hinted_test_true_ex = {
  sub_id: HintedTestTrue,
  term: mk_example("hintedtest \"Always true\" true end"),
  message: "This is reported as a passing test because the body of the test is true.",
};
// TODO are these really the correct messages/explanations; maybe include something about the result being triv
let hinted_test_false_ex = {
  sub_id: HintedTestFalse,
  term: mk_example("hintedtest \"Always false\" 3 < 1 end"),
  message: "This is reported as a failing test because the body of the test is 3 < 1 which evaluates to false.",
};
let _exp_body = exp("e");
let hinted_test_exp_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_body), body_id),
];
let hinted_test_exp: form = {
  let explanation = "If the [*body*](%s) of the test evalutes to `true`, the test passes. Otherwise, the test fails.";
  {
    id: HintedTestExp,
    syntactic_form: [mk_test([[space(), _exp_body, space()]])],
    expandable_id: None,
    explanation,
    examples: [hinted_test_true_ex, hinted_test_false_ex],
  };
};
let tests: group = {id: HintedTestExp, forms: [hinted_test_exp]};

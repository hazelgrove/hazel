open Haz3lcore;
open Example;
open ExplainThisForm;
let hinted_test_true_ex = {
  sub_id: HintedTestTrue,
  term: mk_example("hint \"Always true\"\n test true end"),
  message: "This is reported as a passing test because the body of the test is true.",
};
// TODO are these really the correct messages/explanations; maybe include something about the result being triv
let hinted_test_false_ex = {
  sub_id: HintedTestFalse,
  term: mk_example("hint \"Always false\"\n test 3 < 1 end"),
  message: "This is reported as a failing test because the body of the test is 3 < 1 which evaluates to false.",
};
let _exp_body = exp("e");
let _hint = exp("h");
let hinted_test_exp_coloring_ids =
    (~body_id: Id.t, ~hint_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_body), body_id),
  (Piece.id(_hint), hint_id),
];
let hinted_test_exp: form = {
  let explanation = "The [*hint*](%s) is displayed in the \"Implementation Grading\" section. If the [*body*](%s) of the test evalutes to `true`, the test passes. Otherwise, the test fails.";
  {
    id: HintedTestExp,
    syntactic_form: [
      mk_hinted_test([
        [space(), _hint, space()],
        [space(), _exp_body, space()],
      ]),
    ],
    expandable_id: None,
    explanation,
    examples: [hinted_test_true_ex, hinted_test_false_ex],
  };
};
let tests: group = {id: HintedTestExp, forms: [hinted_test_exp]};

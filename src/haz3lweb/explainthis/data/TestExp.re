open Haz3lcore;
open Example;
open ExplainThisForm;

let test_true_ex = {
  sub_id: TestTrue,
  term: mk_example("test true end"),
  message: "This is reported as a passing test because the body of the test is true.",
};
// TODO are these really the correct messages/explanations; maybe include something about the result being triv
let test_false_ex = {
  sub_id: TestFalse,
  term: mk_example("test 3 < 1 end"),
  message: "This is reported as a failing test because the body of the test is 3 < 1 which evaluates to false.",
};
let _exp_body = exp("e");
let test_exp_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_body), body_id),
];
let test_exp: form = {
  let explanation = "Test expression. If the [*body*](%i) of the test evalutes to `true`, the test passes. Otherwise, the test fails.";
  {
    id: TestExp,
    syntactic_form: [mk_test([[space(), _exp_body, space()]])],
    expandable_id: None,
    explanation,
    examples: [test_true_ex, test_false_ex],
  };
};

let tests: group = {id: TestExp, forms: [test_exp]};

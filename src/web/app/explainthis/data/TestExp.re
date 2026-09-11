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
let exp_body = exp("e");
let test_exp_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_body), body_id),
];
let test_exp_form = [mk_test([[space(), exp_body, space()]])];
let test_exp = (~body_id: Id.t): form => {
  id: TestExp,
  syntactic_form: test_exp_form,
  colorings: test_exp_coloring_ids(~body_id),
  expandable_id: None,
  explanation:
    Stdlib.Printf.sprintf(
      "If the [*body*](%s) of the test evaluates to `true`, the test passes. Otherwise, the test fails.",
      Id.to_string(body_id),
    ),
  examples: [test_true_ex, test_false_ex],
};

let tests = (~body_id: Id.t): group => singleton(test_exp(~body_id));

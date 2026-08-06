open Haz3lcore;
open Example;
open ExplainThisForm;

let seq_basic_exp_ex = {
  sub_id: SeqBasic,
  term: mk_example("1; 2"),
  message: "The left expression evaluates to 1, which is ignored. Then the right expression is evaluated to 2.",
};
// TODO are these really the correct messages/explanations
let seq_test_exp_ex = {
  sub_id: SeqTest,
  term: mk_example("test true end; 3"),
  message: "The left expression is evaluated and recorded as a passing test because the body of the test is true. Then the right expression is evalautes to 3.",
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let seq_exp_coloring_ids =
    (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp1), exp1_id),
  (Piece.id(exp2), exp2_id),
];
let seq_exp_form = [exp1, seq(), space(), exp2];
let seq_exp = (~exp1_id: Id.t, ~exp2_id: Id.t): form => {
  id: SeqExp,
  syntactic_form: seq_exp_form,
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "The [left expression](%s) is evaluated, then the [right expression](%s) is evaluated.",
      Id.to_string(exp1_id),
      Id.to_string(exp2_id),
    ),
  examples: [seq_basic_exp_ex, seq_test_exp_ex],
};

let seqs = (~exp1_id: Id.t, ~exp2_id: Id.t): group => {
  id: SeqExp,
  forms: [seq_exp(~exp1_id, ~exp2_id)],
};

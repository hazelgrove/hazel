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
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let seq_exp_coloring_ids =
    (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp1), exp1_id),
  (Piece.id(_exp2), exp2_id),
];
let seq_exp: form = {
  let explanation = "The [left expression](%s) is evaluated, then the [right expression](%s) is evaluated.";
  {
    id: SeqExp,
    syntactic_form: [_exp1, seq(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [seq_basic_exp_ex, seq_test_exp_ex],
  };
};

let seqs: group = {id: SeqExp, forms: [seq_exp]};

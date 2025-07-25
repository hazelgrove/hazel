open Haz3lcore;
open Example;
open ExplainThisForm;

let _pat = pat("p");
let _exp_body = exp("e");
let test_exp_coloring_ids =
    (~body_id: Id.t, ~pat_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat), pat_id),
  (Piece.id(_exp_body), body_id),
];
let theorem_exp: form = {
  let explanation = "Asserts that the [*goal*](%s) is true in the following expression.";
  {
    id: TheoremExp,
    syntactic_form: [
      mk_theorem([[space(), _pat, space()]]),
      linebreak(),
      _exp_body,
    ],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tests: group = {
  id: TheoremExp,
  forms: [theorem_exp],
};

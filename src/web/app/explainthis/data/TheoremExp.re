open Haz3lcore;
open Example;
open ExplainThisForm;

let _pat = pat("p");
let _exp_thm = exp("h");
let _exp_body = exp("e");
let test_exp_coloring_ids =
    (~body_id: Id.t, ~pat_id: Id.t, ~thm_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat), pat_id),
  (Piece.id(_exp_thm), thm_id),
  (Piece.id(_exp_body), body_id),
];
let theorem_exp: form = {
  let explanation = "Asserts that the [*goal*](%s) is true in the following expression, and [*names*](%s) the theorem for later reuse.";
  {
    id: TheoremExp,
    syntactic_form: [
      mk_theorem([[space(), _pat, space()], [space(), _exp_thm, space()]]),
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

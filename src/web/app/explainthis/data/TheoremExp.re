open Haz3lcore;
open Example;
open ExplainThisForm;

let p = pat("p");
let exp_thm = exp("h");
let _proof = proof("?");
let exp_body = exp("e");
let test_exp_coloring_ids =
    (~body_id: Id.t, ~pat_id: Id.t, ~thm_id: Id.t, ~proof_id: Id.t)
    : list((Id.t, Id.t)) => [
  (Piece.id(p), pat_id),
  (Piece.id(exp_thm), thm_id),
  (Piece.id(_proof), proof_id),
  (Piece.id(exp_body), body_id),
];
let theorem_exp: form = {
  let explanation = "Asserts that the [*goal*](%s) is true via the following [*proof*](%s), and [*names*](%s) the theorem for later reuse in the body.";
  {
    id: TheoremExp,
    syntactic_form: [
      mk_theorem([
        [space(), p, space()],
        [space(), exp_thm, space()],
        [space(), _proof, space()],
      ]),
      linebreak(),
      exp_body,
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

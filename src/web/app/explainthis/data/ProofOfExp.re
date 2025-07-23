open Haz3lcore;
open Example;
open ExplainThisForm;

let _typ = typ("t");
let proof_of_exp_coloring_ids = (~typ_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ), typ_id),
];
let proof_of_exp: form = {
  let explanation = "A placeholder for a proof of [*goal*](%s).";
  {
    id: ProofOfExp,
    syntactic_form: [mk_proof_of([[space(), _typ, space()]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let proof_of_exps: group = {
  id: ProofOfExp,
  forms: [proof_of_exp],
};

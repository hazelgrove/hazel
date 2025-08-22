open Haz3lcore;
open Example;
open ExplainThisForm;

let _exp = exp("exp");
let proof_of_typ_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp), body_id),
];
let proof_of_typ: form = {
  let explanation = "This type asserts that the [*enclosed boolean*](%s) is in fact true.";
  {
    id: ProofOfTyp,
    syntactic_form: [mk_proof_of([[space(), _exp, space()]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let proof_of: group = {
  id: ProofOfTyp,
  forms: [proof_of_typ],
};

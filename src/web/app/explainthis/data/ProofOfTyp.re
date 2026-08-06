open Haz3lcore;
open Example;
open ExplainThisForm;

let e = exp("e");
let proof_of_typ_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(e), body_id),
];
let proof_of_typ_form = [mk_proof_of([[space(), e, space()]])];
let proof_of_typ = (~body_id: Id.t): form => {
  id: ProofOfTyp,
  syntactic_form: proof_of_typ_form,
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "This type asserts that the [*enclosed boolean*](%s) is in fact true.",
      Id.to_string(body_id),
    ),
  examples: [],
};

let proof_of = (~body_id: Id.t): group => {
  id: ProofOfTyp,
  forms: [proof_of_typ(~body_id)],
};

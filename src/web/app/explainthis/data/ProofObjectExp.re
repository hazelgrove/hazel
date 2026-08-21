open Haz3lcore;
open Example;
open ExplainThisForm;

let typ = typ("t");
let proof_of_exp_coloring_ids = (~typ_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(typ), typ_id),
];
let proof_of_exp_form = [mk_proof_of([[space(), typ, space()]])];
let proof_of_exp = (~typ_id: Id.t): form => {
  id: ProofObjectExp,
  syntactic_form: proof_of_exp_form,
  colorings: proof_of_exp_coloring_ids(~typ_id),
  expandable_id: None,
  explanation:
    Stdlib.Printf.sprintf(
      "A placeholder for a proof of [*goal*](%s).",
      Id.to_string(typ_id),
    ),
  examples: [],
};

let proof_of_exps = (~typ_id: Id.t): group =>
  singleton(proof_of_exp(~typ_id));

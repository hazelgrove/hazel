open Haz3lcore;
open Example;
open ExplainThisForm;

let _e1 = exp("e1");
let _e2 = exp("e2");
let equals_typ_coloring_ids =
    (~e1_id: Id.t, ~e2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_e1), e1_id),
  (Piece.id(_e2), e2_id),
];
let equals_typ: form = {
  let explanation = "This equals type is the type of proofs that [*exp variable*](%s) equals [*exp variable*](%s).";
  {
    id: EqualsTyp,
    syntactic_form: [mk_type([[space(), _e1, space()]]), _e2],
    expandable_id: Some((Piece.id(_e1), [_e2])),
    explanation,
    examples: [],
  };
};

let equals_typ: group = {id: EqualsTyp, forms: [equals_typ]};

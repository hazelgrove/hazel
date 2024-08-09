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

let single = (~e1_id: Id.t, ~e2_id: Id.t): Simple.t => {
  group_id: EqualsTyp,
  form_id: EqualsTyp,
  abstract:
    Simple.mk_2(("e1", e1_id), ("e2", e2_id), (e1', e2') =>
      [mk_equals([[space(), e1', space()], [space(), e2', space()]])]
    ),
  explanation:
    Printf.sprintf(
      "This equals type is the type of proofs that expression [*e1*](%s) equals [*e2*](%s).",
      e1_id |> Id.to_string,
      e2_id |> Id.to_string,
    ),
  examples: [],
};

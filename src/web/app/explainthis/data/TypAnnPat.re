open Haz3lcore;
open Example;
open ExplainThisForm;
let p = pat("p");
let typ = typ("ty");
let typann_pat_coloring_ids =
    (~pat_id: Id.t, ~typ_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(p), pat_id),
  (Piece.id(typ), typ_id),
];
let typann_pat_form = [p, space(), typeann(), space(), typ];
let typann_pat = (~pat_id: Id.t, ~typ_id: Id.t): form => {
  id: TypAnnPat,
  syntactic_form: typann_pat_form,
  colorings: typann_pat_coloring_ids(~pat_id, ~typ_id),
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only expressions that match the [type annotated pattern](%s) and have the [indicated type](%s) match this type annotation pattern.",
      Id.to_string(pat_id),
      Id.to_string(typ_id),
    ),
  examples: [],
};

let typann = (~pat_id: Id.t, ~typ_id: Id.t): group => {
  id: TypAnnPat,
  forms: [typann_pat(~pat_id, ~typ_id)],
};

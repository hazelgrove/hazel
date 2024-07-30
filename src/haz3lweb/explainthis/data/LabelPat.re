// open Haz3lcore;
open Example;
open ExplainThisForm;
// let _pat = pat("p");
// let _typ = typ("ty");
// let labeled_pat_coloring_ids =
//     (~pat_id: Id.t, ~typ_id: Id.t): list((Id.t, Id.t)) => [
//   (Piece.id(_pat), pat_id),
//   (Piece.id(_typ), typ_id),
// ];
let label_pat: form = {
  let explanation = "labeled pattern explanation";
  {
    id: LabelPat,
    syntactic_form: [pat("x"), label_pat(), pat("p")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let label_pat: group = {id: LabelPat, forms: [label_pat]};

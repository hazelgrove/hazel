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
let labeled_pat: form = {
  let explanation = "labeled pattern explanation";
  {
    id: LabeledPat,
    syntactic_form: [pat("x"), labeled_pat(), pat("p")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let labeled_pat: group = {id: LabeledPat, forms: [labeled_pat]};

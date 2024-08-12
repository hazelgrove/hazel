// open Haz3lcore;
open ExplainThisForm;
open Example;
let dot_typ: form = {
  let explanation = "Dot Operator Typ explanation";
  {
    id: DotTyp,
    syntactic_form: [exp("(x=t)"), dot_typ(), pat("x")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
// let _exp1 = exp("e1");
// let _exp2 = exp("e2");
// let tuple_exp_size2_coloring_ids =
//     (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => {
//   [(Piece.id(_exp1), exp1_id), (Piece.id(_exp2), exp2_id)];
// }

let dot_typ: group = {id: DotTyp, forms: [dot_typ]};

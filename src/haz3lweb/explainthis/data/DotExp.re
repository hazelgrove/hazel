// open Haz3lcore;
open ExplainThisForm;
open Example;

let dot_example_1 = {
  sub_id: Dot1,
  term: mk_example("(x=1, y=2).x"),
  message: "Gives the element in the tuple associated with the label 'x', which in this example is 1.",
};
let dot_exp: form = {
  let explanation = "Dot Operator explanation";
  {
    id: DotExp,
    syntactic_form: [exp("(x=e)"), dot(), pat("x")],
    expandable_id: None,
    explanation,
    examples: [dot_example_1],
  };
};
// let _exp1 = exp("e1");
// let _exp2 = exp("e2");
// let tuple_exp_size2_coloring_ids =
//     (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => {
//   [(Piece.id(_exp1), exp1_id), (Piece.id(_exp2), exp2_id)];
// }

let dot_exp: group = {id: DotExp, forms: [dot_exp]};

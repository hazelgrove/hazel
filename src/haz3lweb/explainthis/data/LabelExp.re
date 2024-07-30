// open Haz3lcore;
open ExplainThisForm;
open Example;

let label_example_1 = {
  sub_id: Label1,
  term: mk_example("(x=1)"),
  message: "A labeled expression within a singleton tuple, where the element 1 is assigned the label 'x'. ",
};
let label_example_2 = {
  sub_id: Label2,
  term: mk_example("(1, 2, y=3)"),
  message: "A tuple with first element 1, second element 2, and third element 3 with the label 'y'.",
};
let label_exp: form = {
  let explanation = "Labeled Expession explanation";
  {
    id: LabelExp,
    syntactic_form: [exp("x"), label_exp(), exp("e")],
    expandable_id: None,
    explanation,
    examples: [label_example_1, label_example_2],
  };
};
// let _exp1 = exp("e1");
// let _exp2 = exp("e2");
// let tuple_exp_size2_coloring_ids =
//     (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => {
//   [(Piece.id(_exp1), exp1_id), (Piece.id(_exp2), exp2_id)];
// }

let label_exp: group = {id: LabelExp, forms: [label_exp]};

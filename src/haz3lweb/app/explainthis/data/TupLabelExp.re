open Haz3lcore;
open ExplainThisForm;
open Example;

let labeled_example_1 = {
  sub_id: Label1,
  term: mk_example("(x=1)"),
  message: "A labeled expression within a singleton tuple, where the element 1 is assigned the label 'x'.",
};
let labeled_example_2 = {
  sub_id: Label2,
  term: mk_example("(1, 2, y=3)"),
  message: "A tuple with first element 1, second element 2, and third element 3 with the label 'y'.",
};

let lab = exp("x");
let e = exp("e");

let labeled_exps_coloring_ids =
    (~label_id: Id.t, ~exp_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(lab), label_id),
  (Piece.id(e), exp_id),
];
let labeled_exp: form = {
  let explanation = "Assigns a [*label*](%s) to an [*expression*](%s) appearing as an element within a tuple. Labeled tuple items cannot exist outside of a tuple. Labeled tuple items that are not contained within a tuple are autmatically converted into a singleton tuple.";
  {
    id: LabeledExp,
    syntactic_form: [lab, labeled_exp(), e],
    expandable_id: None,
    explanation,
    examples: [labeled_example_1, labeled_example_2],
  };
};
let labeled_exps: group = {
  id: LabeledExp,
  forms: [labeled_exp],
};

open Haz3lcore;
open ExplainThisForm;
open Example;

let labeled_example_1: example = {
  sub_id: Label1,
  term: mk_example("let (x=a) = (x=1) in a"),
  message: "A labeled pattern for a singleton tuple, where the label x is bound to the variable a.",
};
let labeled_example_2: example = {
  sub_id: Label2,
  term: mk_example("let (a, b, y=c) =\n (1, 2, y=3)\n in c"),
  message: "A tuple with first element a, second element b, and third element c with the label 'y'.",
};

let lab = pat("x");
let p = pat("p");

let labeled_exps_coloring_ids =
    (~label_id: Id.t, ~pat_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(lab), label_id),
  (Piece.id(p), pat_id),
];
let labeled_pat: form = {
  let explanation = "Assigns a [*label*](%s) to an [*pattern*](%s) appearing as an element within a tuple. Labeled tuple items cannot exist outside of a tuple. Labeled tuple items that are not contained within a tuple are autmatically converted into a singleton tuple.";
  {
    id: LabeledPat,
    syntactic_form: [lab, labeled_pat(), p],
    expandable_id: None,
    explanation,
    examples: [labeled_example_1, labeled_example_2],
  };
};
let labeled_pats: group = {
  id: LabeledPat,
  forms: [labeled_pat],
};

open Example;
open ExplainThisForm;
// open Haz3lcore;

let labeled_typ: form = {
  let explanation = "label type explanation";
  {
    id: LabeledTyp,
    syntactic_form: [pat("x"), labeled_typ(), typ("t")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let labeled_typ: group = {id: LabeledTyp, forms: [labeled_typ]};

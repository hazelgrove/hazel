open Example;
open ExplainThisForm;
// open Haz3lcore;

let label_typ: form = {
  let explanation = "label type explanation";
  {
    id: LabelTyp,
    syntactic_form: [pat("x"), label_typ(), typ("t")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let label_typ: group = {id: LabelTyp, forms: [label_typ]};

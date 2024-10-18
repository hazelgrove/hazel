open Example;
open ExplainThisForm;
// open Haz3lcore;

let labeled_typ: form = {
  let explanation = "Assigns a label (name) to a type within a tuple. Labeled types cannot exist outside of a tuple; by default, labeled pattens that are not contained within a tuple are implied to be in a singleton tuple.";
  {
    id: LabeledTyp,
    syntactic_form: [pat("x"), labeled_typ(), typ("t")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let labeled_typs: group = {id: LabeledTyp, forms: [labeled_typ]};

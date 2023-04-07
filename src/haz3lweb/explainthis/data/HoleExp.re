open ExplainThisForm;
open Example;

let empty_hole_exp: form = {
  let explanation = "Empty hole. This marks an expression that needs to be filled in.";
  {
    id: EmptyHoleExp,
    syntactic_form: [exp("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let empty_hole_exps = {id: EmptyHoleExp, forms: [empty_hole_exp]};

let multi_hole_exp: form = {
  let explanation = "Not recognized. This is an invalid term.";
  {
    id: MultiHoleExp,
    syntactic_form: [exp("INVALID")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let multi_hole_exps = {id: MultiHoleExp, forms: [multi_hole_exp]};

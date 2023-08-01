open Example;
open ExplainThisForm;
let empty_hole_pat: form = {
  let explanation = "Empty hole pattern. Expressions are not matched against the *empty hole pattern* until it is filled.";
  {
    id: EmptyHolePat,
    syntactic_form: [pat("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let multi_hole_pat: form = {
  let explanation = "Unrecognized pattern. Expressions are not matched against the invalid pattern until it is corrected.";
  {
    id: MultiHolePat,
    syntactic_form: [pat("Invalid")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let empty_hole: group = {id: EmptyHolePat, forms: [empty_hole_pat]};

let multi_hole: group = {id: MultiHolePat, forms: [multi_hole_pat]};

open Example;
open ExplainThisForm;
open Haz3lcore;
let empty_hole_pat: form = {
  let explanation = "Expressions are not matched against the *empty hole pattern* until it is filled.";
  {
    id: EmptyHolePat,
    syntactic_form: [Grout({id: Id.mk(), shape: Convex})],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let multi_hole_pat: form = {
  let explanation = "Expressions are not matched against the invalid pattern until it is corrected.";
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

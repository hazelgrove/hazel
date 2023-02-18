open ExplainThisForm;
open ExampleUtil;

let empty_hole_exp_group = "empty_hole_exp_group";
let empty_hole_exp: form = {
  let explanation = {
    message: "Empty hole. This marks an expression that needs to be filled in.",
    feedback: Unselected,
  };
  {
    id: "empty_hole_exp",
    syntactic_form: [exp("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let multi_hole_exp_group = "multi_hole_exp_group";
let multi_hole_exp: form = {
  let explanation = {
    message: "Not recognized. This is an invalid term.",
    feedback: Unselected,
  };
  {
    id: "multi_hole_exp",
    syntactic_form: [exp("INVALID")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

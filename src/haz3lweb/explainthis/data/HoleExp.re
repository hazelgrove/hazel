open ExplainThisForm;
open Example;

[@deriving (show({with_path: false}), sexp, yojson)]
type empty_hole_exp_group = {
  id: group_id,
  empty_hole_exp: form,
};
let empty_hole_exp: form = {
  let explanation = {
    message: "Empty hole. This marks an expression that needs to be filled in.",
    feedback: Unselected,
  };
  {
    id: EmptyHoleExp,
    syntactic_form: [exp("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let empty_hole_exp_group = {id: EmptyHoleExp, empty_hole_exp};

[@deriving (show({with_path: false}), sexp, yojson)]
type multi_hole_exp_group = {
  id: group_id,
  multi_hole_exp: form,
};
let multi_hole_exp: form = {
  let explanation = {
    message: "Not recognized. This is an invalid term.",
    feedback: Unselected,
  };
  {
    id: MultiHoleExp,
    syntactic_form: [exp("INVALID")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let multi_hole_exp_group = {id: MultiHoleExp, multi_hole_exp};

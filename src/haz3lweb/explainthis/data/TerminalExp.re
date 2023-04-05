open ExplainThisForm;
open Example;

[@deriving (show({with_path: false}), sexp, yojson)]
type triv_exp_group = {
  id: group_id,
  triv_exp: form,
};
let triv_exp: form = {
  let explanation = {message: "Trivial expression.", feedback: Unselected};
  {
    id: TrivExp,
    syntactic_form: [exp("Triv")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let triv_exp_group = {id: TrivExp, triv_exp};

[@deriving (show({with_path: false}), sexp, yojson)]
type bool_exp_group = {
  id: group_id,
  bool_exp: form,
};
let bool_exp: form = {
  let explanation = {message: "Boolean literal.", feedback: Unselected};
  {
    id: BoolExp,
    syntactic_form: [exp("BoolLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let bool_exp_group = {id: BoolExp, bool_exp};

[@deriving (show({with_path: false}), sexp, yojson)]
type int_exp_group = {
  id: group_id,
  int_exp: form,
};
let int_exp: form = {
  let explanation = {message: "Integer literal.", feedback: Unselected};
  {
    id: IntExp,
    syntactic_form: [exp("IntLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let int_exp_group = {id: IntExp, int_exp};

[@deriving (show({with_path: false}), sexp, yojson)]
type float_exp_group = {
  id: group_id,
  float_exp: form,
};
let float_exp: form = {
  let explanation = {
    message: "Floating-point literal.",
    feedback: Unselected,
  };
  {
    id: FloatExp,
    syntactic_form: [exp("FloatLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let float_exp_group = {id: FloatExp, float_exp};

[@deriving (show({with_path: false}), sexp, yojson)]
type string_exp_group = {
  id: group_id,
  string_exp: form,
};
let string_exp: form = {
  let explanation = {message: "String literal.", feedback: Unselected};
  {
    id: StringExp,
    syntactic_form: [exp("StringLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let string_exp_group = {id: StringExp, string_exp};

[@deriving (show({with_path: false}), sexp, yojson)]
type var_exp_group = {
  id: group_id,
  var_exp: form,
};
let var_exp: form = {
  let explanation = {
    message: "Variable. Takes the value of the expression that it was bound to.",
    feedback: Unselected,
  };
  {
    id: VarExp,
    syntactic_form: [exp("x")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let var_exp_group = {id: VarExp, var_exp};

[@deriving (show({with_path: false}), sexp, yojson)]
type tag_exp_group = {
  id: group_id,
  tag_exp: form,
};
let tag_exp: form = {
  let explanation = {message: "`%s` constructor.", feedback: Unselected};
  {
    id: TagExp,
    syntactic_form: [exp("C")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let tag_exp_group = {id: TagExp, tag_exp};

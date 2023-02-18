open ExplainThisForm;
open ExampleUtil;

let triv_exp_group = "triv_exp_group";
let triv_exp: form = {
  let explanation = {message: "Trivial expression.", feedback: Unselected};
  {
    id: "triv_exp",
    syntactic_form: [exp("Triv")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let bool_exp_group = "bool_exp_group";
let bool_exp: form = {
  let explanation = {message: "Boolean literal.", feedback: Unselected};
  {
    id: "bool_exp",
    syntactic_form: [exp("BoolLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let int_exp_group = "int_exp_group";
let int_exp: form = {
  let explanation = {message: "Integer literal.", feedback: Unselected};
  {
    id: "int_exp",
    syntactic_form: [exp("IntLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let float_exp_group = "float_exp_group";
let float_exp: form = {
  let explanation = {
    message: "Floating-point literal.",
    feedback: Unselected,
  };
  {
    id: "float_exp",
    syntactic_form: [exp("FloatLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let string_exp_group = "string_exp_group";
let string_exp: form = {
  let explanation = {message: "String literal.", feedback: Unselected};
  {
    id: "string_exp",
    syntactic_form: [exp("StringLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_exp_group = "var_exp_group";
let var_exp: form = {
  let explanation = {
    message: "Variable. Takes the value of the expression that it was bound to.",
    feedback: Unselected,
  };
  {
    id: "var_exp",
    syntactic_form: [exp("x")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tag_exp_group = "tag_exp_group";
let tag_exp: form = {
  let explanation = {message: "`%s` constructor.", feedback: Unselected};
  {
    id: "tag_exp",
    syntactic_form: [exp("C")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

open ExplainThisForm;
open Example;

let triv_exp: form = {
  id: TrivExp,
  syntactic_form: [exp("()")],
  expandable_id: None,
  explanation: "Trivial value.",
  examples: [],
};
let triv_exps: group = {id: TrivExp, forms: [triv_exp]};

let bool_exp: form = {
  id: BoolExp,
  syntactic_form: [exp("BoolLit")],
  expandable_id: None,
  explanation: "Boolean literal.",
  examples: [],
};
let bool_exps: group = {id: BoolExp, forms: [bool_exp]};

let int_exp: form = {
  id: IntExp,
  syntactic_form: [exp("IntLit")],
  expandable_id: None,
  explanation: "Integer literal.",
  examples: [],
};
let int_exps: group = {id: IntExp, forms: [int_exp]};

let float_exp: form = {
  id: FloatExp,
  syntactic_form: [exp("FloatLit")],
  expandable_id: None,
  explanation: "Floating-point literal.",
  examples: [],
};
let float_exps: group = {id: FloatExp, forms: [float_exp]};

let string_exp: form = {
  id: StringExp,
  syntactic_form: [exp("StringLit")],
  expandable_id: None,
  explanation: "String literal.",
  examples: [],
};
let string_exps: group = {id: StringExp, forms: [string_exp]};

let var_exp: form = {
  id: VarExp,
  syntactic_form: [exp("x")],
  expandable_id: None,
  explanation: "Variable. Takes the value of the expression that it was bound to.",
  examples: [],
};
let var_exps: group = {id: VarExp, forms: [var_exp]};

let tag_exp: form = {
  id: TagExp,
  syntactic_form: [exp("C")],
  expandable_id: None,
  explanation: "`%s` constructor.",
  examples: [],
};
let tag: group = {id: TagExp, forms: [tag_exp]};

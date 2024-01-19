open ExplainThisForm;
open Example;

let triv_exp: form = {
  id: TrivExp,
  syntactic_form: [exp("()")],
  expandable_id: None,
  explanation: "The unique value of type Unit",
  examples: [],
};
let triv_exps: group = {id: TrivExp, forms: [triv_exp]};

let bool_exp: form = {
  id: BoolExp,
  syntactic_form: [exp("BoolLit")],
  expandable_id: None,
  explanation: "One of two truth values",
  examples: [],
};
let bool_exps: group = {id: BoolExp, forms: [bool_exp]};

let int_exp: form = {
  id: IntExp,
  syntactic_form: [exp("IntLit")],
  expandable_id: None,
  explanation: "An integer literal.",
  examples: [],
};
let int_exps: group = {id: IntExp, forms: [int_exp]};

let float_exp: form = {
  id: FloatExp,
  syntactic_form: [exp("FloatLit")],
  expandable_id: None,
  explanation: "A floating-point literal.",
  examples: [],
};
let float_exps: group = {id: FloatExp, forms: [float_exp]};

let string_exp: form = {
  id: StringExp,
  syntactic_form: [exp("StringLit")],
  expandable_id: None,
  explanation: "A string literal.",
  examples: [],
};
let string_exps: group = {id: StringExp, forms: [string_exp]};

let var_exp: form = {
  id: VarExp,
  syntactic_form: [exp("x")],
  expandable_id: None,
  explanation: "Takes the value of the expression that it was bound to.",
  examples: [],
};
let var_exps: group = {id: VarExp, forms: [var_exp]};

let ctr_exp: form = {
  id: CtrExp,
  syntactic_form: [exp("C")],
  expandable_id: None,
  explanation: "`%s` is a constructor for a sum type variant.",
  examples: [],
};
let ctr: group = {id: CtrExp, forms: [ctr_exp]};

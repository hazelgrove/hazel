open Haz3lcore;
open ExplainThisForm;
open Example;

let triv_exp: form = {
  id: TrivExp,
  syntactic_form: [exp("()")],
  expandable_id: None,
  explanation: "The unique value of type `()`.",
  examples: [],
};
let triv_exps: group = {id: TrivExp, forms: [triv_exp]};

let deferral_exp_ex = {
  sub_id: Deferral,
  term:
    mk_example(
      "let plus = fun (x, y) -> x + y in\nlet incr = plus(_, 1) in\nincr(5)",
    ),
  message: "In the partial application plus(_, 1), the deferral expression marks the first argument, which was not applied until in the full function application incr(5).",
};
let deferral_exp: form = {
  id: DeferralExp,
  syntactic_form: [exp("_")],
  expandable_id: None,
  explanation: "Marks an argument that has not yet been applied in a partial application.",
  examples: [deferral_exp_ex],
};
let deferral_exps: group = {id: DeferralExp, forms: [deferral_exp]};

let bool_exp = (b: bool): form => {
  id: BoolExp,
  syntactic_form: [exp(b |> string_of_bool)],
  expandable_id: None,
  explanation: "A boolean literal, either `true` or `false`.",
  examples: [],
};
let bool_exps = (b: bool): group => {id: BoolExp, forms: [bool_exp(b)]};

let int_exp = (n: int): form => {
  id: IntExp,
  syntactic_form: [n |> string_of_int |> exp],
  expandable_id: None,
  explanation: "A signed integer literal.",
  examples: [],
};
let int_exps = (i: int): group => {id: IntExp, forms: [int_exp(i)]};

let float_exp = (f: float): form => {
  id: FloatExp,
  syntactic_form: [f |> string_of_float |> exp],
  expandable_id: None,
  explanation: "A floating-point literal.",
  examples: [],
};
let float_exps = (f: float): group => {
  id: FloatExp,
  forms: [float_exp(f)],
};

let string_exp = (s: string): form => {
  id: StringExp,
  syntactic_form: [s |> abbreviate |> Haz3lcore.Form.string_quote |> exp],
  expandable_id: None,
  explanation: "A string literal. Any character besides double quotes (`\"`) can be used.",
  examples: [],
};
let string_exps = (s: string): group => {
  id: StringExp,
  forms: [string_exp(s)],
};

let prop_exp = (p: Derivation.Prop.t): form => {
  id: PropExp,
  syntactic_form: [p |> Derivation.Prop.repr |> exp],
  expandable_id: None,
  explanation: "A proposition literal.",
  examples: [],
};
let prop_exps = (p: Derivation.Prop.t): group => {
  id: PropExp,
  forms: [prop_exp(p)],
};

let var_exp = (n: string): form => {
  id: VarExp,
  syntactic_form: [n |> abbreviate |> exp],
  expandable_id: None,
  explanation: "Takes the value of the expression that it was bound to.",
  examples: [],
};
let var_exps = (x: string): group => {id: VarExp, forms: [var_exp(x)]};

let ctr_exp = (c: string): form => {
  id: CtrExp,
  syntactic_form: [c |> abbreviate |> exp],
  expandable_id: None,
  explanation: "`%s` is a constructor for a sum type variant.",
  examples: [],
};
let ctr = (c: string): group => {id: CtrExp, forms: [ctr_exp(c)]};

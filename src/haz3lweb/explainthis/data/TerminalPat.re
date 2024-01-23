open Example;
open ExplainThisForm;
let wild_pat: form = {
  let explanation = "The *wildcard pattern* matches any expression.";
  {
    id: WildPat,
    syntactic_form: [pat("_")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let intlit_pat = (i: int): form => {
  let explanation = "Only expressions with value `%i` match the *`%i` pattern*.";
  {
    id: IntPat,
    syntactic_form: [i |> string_of_int |> abbreviate |> pat],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let floatlit_pat = (f: float): form => {
  let explanation = "Only expressions with value `%f` match the *`%f` pattern*.";
  {
    id: FloatPat,
    syntactic_form: [f |> string_of_float |> abbreviate |> pat],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let boollit_pat = (b: bool): form => {
  let explanation = "Only expressions with value `%b` match the *`%b` pattern*.";
  {
    id: BoolPat,
    syntactic_form: [b |> string_of_bool |> abbreviate |> pat],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let strlit_pat = (s: string): form => {
  let explanation = "Only expressions with value `%s` match the *`%s` pattern*.";
  {
    id: StrPat,
    syntactic_form: [s |> abbreviate |> Haz3lcore.Form.string_quote |> pat],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let triv_pat: form = {
  let explanation = "Only expressions with the trivial value `()` match the *trivial pattern `()`*.";
  {
    id: TrivPat,
    syntactic_form: [pat("()")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_pat = (name: string): form => {
  let explanation = "This *pattern variable* matches any expression, binding its value to variable `%s`.";
  {
    id: VarPat,
    syntactic_form: [name |> abbreviate |> pat],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let ctr_pat = (name: string): form => {
  let explanation = "Only expressions that match the *`%s` constructor* match this constructor pattern.";
  {
    id: CtrPat,
    syntactic_form: [name |> abbreviate |> pat],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let wild: group = {id: WildPat, forms: [wild_pat]};

let intlit = (i: int): group => {id: IntPat, forms: [intlit_pat(i)]};

let floatlit = (f: float): group => {
  id: FloatPat,
  forms: [floatlit_pat(f)],
};

let boollit = (b: bool): group => {id: BoolPat, forms: [boollit_pat(b)]};

let strlit = (s: string): group => {id: StrPat, forms: [strlit_pat(s)]};

let triv: group = {id: TrivPat, forms: [triv_pat]};

let var = (name: string): group => {id: VarPat, forms: [var_pat(name)]};

let ctr = (name: string): group => {id: CtrPat, forms: [ctr_pat(name)]};

open Example;
open ExplainThisForm;
let wild_pat: form = {
  let explanation = "The *wildcard pattern* matches any expression.";
  {
    id: WildPat,
    syntactic_form: [pat("_")],
    colorings: [],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let sintlit_pat = (i: int): form => {
  id: SIntPat,
  syntactic_form: [i |> string_of_int |> abbreviate |> pat],
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only expressions with value `%i` match the *`%i` pattern*.",
      i,
      i,
    ),
  examples: [],
};

let intlit_pat = (i: string): form => {
  id: IntPat,
  syntactic_form: [i |> abbreviate |> pat],
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only expressions with value `%s` match the *`%s` pattern*.",
      i,
      i,
    ),
  examples: [],
};

let floatlit_pat = (f: float): form => {
  id: FloatPat,
  syntactic_form: [f |> string_of_float |> abbreviate |> pat],
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only expressions with value `%f` match the *`%f` pattern*.",
      f,
      f,
    ),
  examples: [],
};

let boollit_pat = (b: bool): form => {
  id: BoolPat,
  syntactic_form: [b |> string_of_bool |> abbreviate |> pat],
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only expressions with value `%b` match the *`%b` pattern*.",
      b,
      b,
    ),
  examples: [],
};

let strlit_pat = (s: string): form => {
  id: StrPat,
  syntactic_form: [s |> abbreviate |> Haz3lcore.Token.string_quote |> pat],
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only expressions with value `%s` match the *`%s` pattern*.",
      s,
      s,
    ),
  examples: [],
};

let triv_pat: form = {
  let explanation = "Only expressions with the trivial value `()` match the *trivial pattern `()`*.";
  {
    id: TrivPat,
    syntactic_form: [pat("()")],
    colorings: [],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_pat = (name: string): form => {
  id: VarPat,
  syntactic_form: [name |> abbreviate |> pat],
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "This *pattern variable* matches any expression, binding its value to variable `%s`.",
      name,
    ),
  examples: [],
};

let ctr_pat = (name: string): form => {
  id: CtrPat,
  syntactic_form: [name |> abbreviate |> pat],
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only expressions that match the *`%s` constructor* match this constructor pattern.",
      name,
    ),
  examples: [],
};

let wild: group = {
  id: WildPat,
  forms: [wild_pat],
};

let intlit = (i: Bigint.t): group => {
  id: IntPat,
  forms: [intlit_pat(i |> Bigint.to_string)],
};
let sintlit = (i: int): group => {
  id: SIntPat,
  forms: [sintlit_pat(i)],
};

let floatlit = (f: float): group => {
  id: FloatPat,
  forms: [floatlit_pat(f)],
};

let boollit = (b: bool): group => {
  id: BoolPat,
  forms: [boollit_pat(b)],
};

let strlit = (s: string): group => {
  id: StrPat,
  forms: [strlit_pat(s)],
};

let triv: group = {
  id: TrivPat,
  forms: [triv_pat],
};

let var = (name: string): group => {
  id: VarPat,
  forms: [var_pat(name)],
};

let ctr = (name: string): group => {
  id: CtrPat,
  forms: [ctr_pat(name)],
};

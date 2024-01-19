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

let intlit_pat: form = {
  let explanation = "Only expressions with value `%i` match the *`%i` pattern*.";
  {
    id: IntPat,
    syntactic_form: [pat("IntLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let floatlit_pat: form = {
  let explanation = "Only expressions with value `%f` match the *`%f` pattern*.";
  {
    id: FloatPat,
    syntactic_form: [pat("FloatLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let boollit_pat: form = {
  let explanation = "Only expressions with value `%b` match the *`%b` pattern*.";
  {
    id: BoolPat,
    syntactic_form: [pat("BoolLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let strlit_pat: form = {
  let explanation = "Only expressions with value `%s` match the *`%s` pattern*.";
  {
    id: StrPat,
    syntactic_form: [pat("StringLit")],
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

let var_pat: form = {
  let explanation = "This *pattern variable* matches any expression, binding its value to variable `%s`.";
  {
    id: VarPat,
    syntactic_form: [pat("x")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let ctr_pat: form = {
  let explanation = "Only expressions that match the *`%s` constructor* match this constructor pattern.";
  {
    id: CtrPat,
    syntactic_form: [pat("C")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let wild: group = {id: WildPat, forms: [wild_pat]};

let intlit: group = {id: IntPat, forms: [intlit_pat]};

let floatlit: group = {id: FloatPat, forms: [floatlit_pat]};

let boollit: group = {id: BoolPat, forms: [boollit_pat]};

let strlit: group = {id: StrPat, forms: [strlit_pat]};

let triv: group = {id: TrivPat, forms: [triv_pat]};

let var: group = {id: VarPat, forms: [var_pat]};

let ctr: group = {id: CtrPat, forms: [ctr_pat]};

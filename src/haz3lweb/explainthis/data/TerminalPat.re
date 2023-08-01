open Example;
open ExplainThisForm;
let wild_pat: form = {
  let explanation = "Wildcard pattern. All expressions match the *wildcard pattern*.";
  {
    id: Wild,
    syntactic_form: [pat("_")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let intlit_pat: form = {
  let explanation = "Integer literal pattern. Only expressions with value `%i` match the *`%i` pattern*.";
  {
    id: IntPat,
    syntactic_form: [pat("IntLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let floatlit_pat: form = {
  let explanation = "Floating-point literal pattern. Only expressions with value `%f` match the *`%f` pattern*.";
  {
    id: FloatPat,
    syntactic_form: [pat("FloatLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let boollit_pat: form = {
  let explanation = "Boolean literal pattern. Only expressions with value `%b` match the *`%b` pattern*.";
  {
    id: BoolPat,
    syntactic_form: [pat("BoolLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let strlit_pat: form = {
  let explanation = "String literal pattern. Only expressions with value `%s` match the *`%s` pattern*.";
  {
    id: StrPat,
    syntactic_form: [pat("StringLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let triv_pat: form = {
  let explanation = "Triv pattern. Only expressions with the trivial value `triv` match the *trivial pattern `triv`*.";
  {
    id: TrivPat,
    syntactic_form: [pat("triv")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_pat: form = {
  let explanation = "Variable pattern. All expressions match the *variable pattern*. The matching expression will be bound to variable `%s`.";
  {
    id: VarPat,
    syntactic_form: [pat("x")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tag_pat: form = {
  let explanation = "Constructor pattern. Only expressions that match the *`%s` constructor* match this constructor pattern.";
  {
    id: TagPat,
    syntactic_form: [pat("C")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let wild: group = {id: Wild, forms: [wild_pat]};

let intlit: group = {id: IntPat, forms: [intlit_pat]};

let floatlit: group = {id: FloatPat, forms: [floatlit_pat]};

let boollit: group = {id: BoolPat, forms: [boollit_pat]};

let strlit: group = {id: StrPat, forms: [strlit_pat]};

let triv: group = {id: TrivPat, forms: [triv_pat]};

let var: group = {id: VarPat, forms: [var_pat]};

let tag: group = {id: TagPat, forms: [tag_pat]};

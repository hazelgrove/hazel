open ExampleUtil;
open ExplainThisForm;

let wild_pat_group = "wild_pat_group";
let wild_pat: form = {
  let explanation = {
    message: "Wildcard pattern. All expressions match the *wildcard pattern*.",
    feedback: Unselected,
  };
  {
    id: "wild_pat",
    syntactic_form: [pat("_")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let intlit_pat_group = "intlit_pat_group";
let intlit_pat: form = {
  let explanation = {
    message: "Integer literal pattern. Only expressions with value `%i` match the *`%i` pattern*.",
    feedback: Unselected,
  };
  {
    id: "intlit_pat",
    syntactic_form: [pat("IntLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let floatlit_pat_group = "floatlit_pat_group";
let floatlit_pat: form = {
  let explanation = {
    message: "Floating-point literal pattern. Only expressions with value `%f` match the *`%f` pattern*.",
    feedback: Unselected,
  };
  {
    id: "floatlit_pat",
    syntactic_form: [pat("FloatLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let boollit_pat_group = "boollit_pat_group";
let boollit_pat: form = {
  let explanation = {
    message: "Boolean literal pattern. Only expressions with value `%b` match the *`%b` pattern*.",
    feedback: Unselected,
  };
  {
    id: "boollit_pat",
    syntactic_form: [pat("BoolLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let strlit_pat_group = "strlit_pat_group";
let strlit_pat: form = {
  let explanation = {
    message: "String literal pattern. Only expressions with value `%s` match the *`%s` pattern*.",
    feedback: Unselected,
  };
  {
    id: "strlit_pat",
    syntactic_form: [pat("StringLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let triv_pat_group = "triv_pat_group";
let triv_pat: form = {
  let explanation = {
    message: "Triv pattern. Only expressions with the trivial value `triv` match the *trivial pattern `triv`*.",
    feedback: Unselected,
  };
  {
    id: "triv_pat",
    syntactic_form: [pat("triv")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_pat_group = "var_pat_group";
let var_pat: form = {
  let explanation = {
    message: "Variable pattern. All expressions match the *variable pattern*. The matching expression will be bound to variable `%s`.",
    feedback: Unselected,
  };
  {
    id: "var_pat",
    syntactic_form: [pat("x")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tag_pat_group = "tag_pat_group";
let tag_pat: form = {
  let explanation = {
    message: "Constructor pattern. Only expressions that match the *`%s` constructor* match this constructor pattern.",
    feedback: Unselected,
  };
  {
    id: "tag_pat",
    syntactic_form: [pat("C")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

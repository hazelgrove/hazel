open Example;
open ExplainThisForm;

let int_typ: form = {
  let explanation = "The `Int` type classifies integer values.";
  {
    id: IntTyp,
    syntactic_form: [typ("Int")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let float_typ: form = {
  let explanation = "The `Float` type classifies floating-point values.";
  {
    id: FloatTyp,
    syntactic_form: [typ("Float")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let bool_typ: form = {
  let explanation = "The `Bool` type classifies boolean values.";
  {
    id: BoolTyp,
    syntactic_form: [typ("Bool")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let str_typ: form = {
  let explanation = "The `String` type classifies string values.";
  {
    id: StrTyp,
    syntactic_form: [typ("String")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_typ: form = {
  let explanation = "`%s` is a type variable.";
  {
    id: VarTyp,
    syntactic_form: [typ("T")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let int: group = {id: IntTyp, forms: [int_typ]};

let float: group = {id: FloatTyp, forms: [float_typ]};

let bool: group = {id: BoolTyp, forms: [bool_typ]};

let str: group = {id: StrTyp, forms: [str_typ]};

let var: group = {id: VarTyp, forms: [var_typ]};

open Example;
open ExplainThisForm;

let int_typ: form = {
  let explanation = "The `Int` type classifies 32-bit signed integer values.";
  {
    id: IntTyp,
    syntactic_form: [typ("Int")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let float_typ: form = {
  let explanation = "The `Float` type classifies 64-bit floating-point values according to the IEEE 754 standard.";
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

let prop_typ: form = {
  let explanation = "The `Prop` type classifies propositions.";
  {
    id: PropTyp,
    syntactic_form: [typ("Prop")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let judgement_typ: form = {
  let explanation = "The `Judgement` type classifies judgements.";
  {
    id: JudgementTyp,
    syntactic_form: [typ("Judgement")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_typ = (name: string): form => {
  let explanation = "`%s` is a type variable.";
  {
    id: VarTyp,
    syntactic_form: [name |> abbreviate |> typ],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let int: group = {id: IntTyp, forms: [int_typ]};

let float: group = {id: FloatTyp, forms: [float_typ]};

let bool: group = {id: BoolTyp, forms: [bool_typ]};

let str: group = {id: StrTyp, forms: [str_typ]};

let prop: group = {id: PropTyp, forms: [prop_typ]};

let judgement: group = {id: JudgementTyp, forms: [judgement_typ]};

let var = (name: string): group => {id: VarTyp, forms: [var_typ(name)]};

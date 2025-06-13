open Example;
open ExplainThisForm;

let int_typ: form = {
  let explanation = "The `Int` type classifies (unbounded) integer values.";
  {
    id: IntTyp,
    syntactic_form: [typ("SInt")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let sint_typ: form = {
  let explanation = "The `SInt` type classifies 32-bit signed integer values.";
  {
    id: SIntTyp,
    syntactic_form: [typ("SInt")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let nat_typ: form = {
  let explanation = "The `Nat` type classifies natural numbers (integers >= 0).";
  {
    id: NatTyp,
    syntactic_form: [typ("Nat")],
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

let int: group = {
  id: IntTyp,
  forms: [int_typ],
};
let sint: group = {
  id: IntTyp,
  forms: [int_typ],
};
let nat: group = {
  id: NatTyp,
  forms: [nat_typ],
};

let float: group = {
  id: FloatTyp,
  forms: [float_typ],
};

let bool: group = {
  id: BoolTyp,
  forms: [bool_typ],
};

let str: group = {
  id: StrTyp,
  forms: [str_typ],
};

let var = (name: string): group => {
  id: VarTyp,
  forms: [var_typ(name)],
};

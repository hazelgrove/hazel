open Example;
open ExplainThisForm;

let int_typ: form = {
  let explanation = "The `Int` type classifies (unbounded) integer values.";
  {
    id: IntTyp,
    syntactic_form: [typ("Int")],
    colorings: [],
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
    colorings: [],
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
    colorings: [],
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
    colorings: [],
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
    colorings: [],
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
    colorings: [],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let void_absurd_ex = {
  sub_id: VoidAbsurd,
  term:
    mk_example(
      "let absurd : Void -> Int =\nfun (v : Void) -> case v end\nin absurd",
    ),
  message: "An absurd eliminator: a function that takes a `Void` argument and case-analyzes on it with zero rules. The case is vacuously exhaustive because `Void` has no constructors, so the function is well-typed at every return type. It can never actually be called, since no value of type `Void` exists.",
};

let void_typ: form = {
  let explanation = "The `Void` type is the empty type: it has no values. It is a nullary sum (a sum type with zero constructors), so any case analysis on a value of type `Void` is vacuously exhaustive. `Void` is useful as the return type of functions that never return, and as the argument type of absurd eliminators.";
  {
    id: VoidTyp,
    syntactic_form: [typ("Void")],
    colorings: [],
    expandable_id: None,
    explanation,
    examples: [void_absurd_ex],
  };
};

let var_typ = (name: string): form => {
  id: VarTyp,
  syntactic_form: [name |> abbreviate |> typ],
  colorings: [],
  expandable_id: None,
  explanation: Printf.sprintf("`%s` is a type variable.", name),
  examples: [],
};

let int: group = {
  id: IntTyp,
  forms: [int_typ],
};
let sint: group = {
  id: SIntTyp,
  forms: [sint_typ],
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

let void: group = {
  id: VoidTyp,
  forms: [void_typ],
};

let var = (name: string): group => singleton(var_typ(name));

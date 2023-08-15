open Haz3lcore;
open ExplainThisForm;
open Example;

let labelled_sum_typ: form = {
  let explanation = "Sum type. Sum types express finite labeled choices. Values of this type consist of one of the specified constructors applied to a parameter of the corresponding parameter type, if specified. Constructor names must be unique within a sum.";
  let divider = Example.mk_monotile(Form.get("typ_plus"));
  {
    id: LabelledSumTyp,
    syntactic_form: [
      space(),
      typ("Cons(ty)"),
      space(),
      divider,
      space(),
      typ("..."),
      space(),
    ],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let sum_typ_unary_constructor_def: form = {
  let explanation = "Parameterized constructor definition. This specifies one possible way of constructing the parent sum type, when applied to a parameter of the specified parameter type.";
  {
    id: SumTypUnaryConstructorDef,
    syntactic_form: [typ("Constructor(type)")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let sum_typ_nullary_constructor_def: form = {
  let explanation = "Constant constructor definition. This specifies one possible way of constructing the parent sum type. It does not take an argument, so it a constant of that type.";
  {
    id: SumTypNullaryConstructorDef,
    syntactic_form: [typ("Constructor")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let labelled_sum_typs: group = {
  id: LabelledSumTyp,
  forms: [labelled_sum_typ],
};

let sum_typ_unary_constructor_defs: group = {
  id: SumTypUnaryConstructorDef,
  forms: [sum_typ_unary_constructor_def],
};

let sum_typ_nullary_constructor_defs: group = {
  id: SumTypNullaryConstructorDef,
  forms: [sum_typ_nullary_constructor_def],
};

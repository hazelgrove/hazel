open Haz3lcore;
open ExplainThisForm;
open Example;

let labelled_sum_typ: form = {
  let explanation = "Sum types describe a finite set of labeled choices. Values of this type consist of one of the specified constructors applied to a parameter of the corresponding parameter type, if specified. Constructor names must be unique within a sum.";
  let divider = Example.mk_monotile(Form.get(TypPlus));
  {
    id: LabelledSumTyp,
    syntactic_form: [
      space(),
      typ("Constructor(ty)"),
      space(),
      divider,
      space(),
      typ("..."),
      space(),
    ],
    colorings: [],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let sum_typ_nullary_constructor_def = (c: string): form => {
  let explanation = "This specifies one possible way of constructing the parent sum type.";
  {
    id: SumTypNullaryConstructorDef,
    syntactic_form: [c |> abbreviate |> typ],
    colorings: [],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let labelled_sum_typs: group = {
  id: LabelledSumTyp,
  forms: [labelled_sum_typ],
};

let sum_typ_nullary_constructor_defs = (c: string): group =>
  singleton(sum_typ_nullary_constructor_def(c));

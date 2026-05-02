open Example;
open ExplainThisForm;

/* `X, Y, …` appearing as the argument bundle of a type-parameter
   application `T(X, Y, …)`. `TypTuple` has no kind of its own;
   its elements must match the callee's tuple-arrow kind. */

let typ_tuple_tyalias_form: form = {
  let explanation = "A comma-separated bundle of type arguments for a parameterized type application, e.g. the `Int, Bool` in `Either(Int, Bool)`. Its elements must match the arity of the callee's kind.";
  {
    id: TypTupleTyp,
    syntactic_form: [
      typ("X"),
      Example.comma_typ(),
      Example.space(),
      typ("Y"),
    ],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let typ_tuples: group = {
  id: TypTupleTyp,
  forms: [typ_tuple_tyalias_form],
};

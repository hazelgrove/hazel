open Haz3lcore;
open Example;
open ExplainThisForm;

/* `T(X, Y, …)` applies a parameterized type constructor at the type
   level. The callee's kind constrains the arity: `T` of kind
   `(Type, Type) -> Type` accepts exactly two arguments, and the
   result has kind `Type`. */

let _callee = typ("T");
let _arg = typ("X");

let typ_param_ap_coloring_ids =
    (~callee_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_callee), callee_id),
  (Piece.id(_arg), arg_id),
];

let typ_param_ap_form: form = {
  let explanation = "Applies the parameterized type [*%s*](%s) to the type argument [*%s*](%s). Each argument substitutes for the corresponding parameter of `%s`.";
  {
    id: TypParamApTyp,
    syntactic_form: [_callee, mk_parens_typ([[_arg]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let typ_param_aps: group = {
  id: TypParamApTyp,
  forms: [typ_param_ap_form],
};

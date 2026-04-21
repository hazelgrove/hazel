open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("t_var");
let _exp_arg = exp("exp_arg");
let forall_exp_coloring_ids =
    (~pat_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_tpat), pat_id),
  (Piece.id(_exp_arg), body_id),
];
let forall_exp: form = {
  let explanation = "The forall expression asserts that for every value of the [*variables*](%s) the inside of the forall always returns true [*inner type*](%s). This expression does not work at runtime (it would take forever if it did!), but is useful for specifying properties of functions and data types.";
  {
    id: ForallExp,
    syntactic_form: [
      mk_forall([[space(), _tpat, space()]]),
      space(),
      _exp_arg,
    ],
    expandable_id: Some((Piece.id(_tpat), [_exp_arg])),
    explanation,
    examples: [],
  };
};

let forall: group = {
  id: ForallExp,
  forms: [forall_exp],
};

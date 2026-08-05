open Haz3lcore;
open Example;
open ExplainThisForm;

let tpat = tpat("t_var");
let exp_arg = exp("exp_arg");
let forall_exp_coloring_ids =
    (~pat_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(tpat), pat_id),
  (Piece.id(exp_arg), body_id),
];
let forall_exp_form = [
  mk_forall([[space(), tpat, space()]]),
  space(),
  exp_arg,
];
let forall_exp = (~pat_id: Id.t, ~body_id: Id.t): form => {
  id: ForallExp,
  syntactic_form: forall_exp_form,
  expandable_id: Some((Piece.id(tpat), [exp_arg])),
  explanation:
    Printf.sprintf(
      "The forall expression asserts that for every value of the [*variables*](%s) the inside of the forall always returns true [*inner type*](%s). This expression does not work at runtime (it would take forever if it did!), but is useful for specifying properties of functions and data types.",
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [],
};

let forall = (~pat_id: Id.t, ~body_id: Id.t): group => {
  id: ForallExp,
  forms: [forall_exp(~pat_id, ~body_id)],
};

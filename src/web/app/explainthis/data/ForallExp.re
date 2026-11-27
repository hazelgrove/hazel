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
  colorings: forall_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(tpat), [exp_arg])),
  explanation:
    sprintf(
      "Asserts that for every total value of the [*variable*](%s) the [*body*](%s) denotes `true`. Quantifiers range over total values only — terminating, error-free ones — which is what makes a proven `forall` an operational guarantee: every closed instance of it really evaluates to `true`. It states a property rather than computing one, so it does not run (that would take forever).",
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [
    {
      sub_id: Forall,
      term: mk_example("theorem t = forall x -> x + 0 == x proof ? in 0"),
      message: "The statement of `t` quantifies over every Int `x`. Proving it means proving `x + 0 == x` for an arbitrary `x`, not for any particular one.",
    },
  ],
};

let forall = (~pat_id: Id.t, ~body_id: Id.t): group =>
  singleton(forall_exp(~pat_id, ~body_id));

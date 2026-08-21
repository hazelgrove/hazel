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
let forall_exp: form = {
  let explanation = "Asserts that for every total value of the [*variable*](%s) the [*body*](%s) denotes `true`. Quantifiers range over total values only — terminating, error-free ones — which is what makes a proven `forall` an operational guarantee: every closed instance of it really evaluates to `true`. It states a property rather than computing one, so it does not run (that would take forever). Write `forall p where g -> e` to quantify over only the values satisfying `g`.";
  {
    id: ForallExp,
    syntactic_form: [
      mk_forall([[space(), tpat, space()]]),
      space(),
      exp_arg,
    ],
    expandable_id: Some((Piece.id(tpat), [exp_arg])),
    explanation,
    examples: [
      {
        sub_id: Forall,
        term: mk_example("theorem t = forall x -> x + 0 == x proof ? in 0"),
        message: "The statement of `t` quantifies over every Int `x`. Proving it means proving `x + 0 == x` for an arbitrary `x`, not for any particular one.",
      },
    ],
  };
};

let forall: group = {
  id: ForallExp,
  forms: [forall_exp],
};

/* `forall p where g -> e`: the restricted binder (Phase 2 of
   docs/prover-obligations.md). Sugar for `forall p -> g ==> e`, but the
   restriction is on the binder, which is where the proof and every use of
   the theorem can see it. */
let where_single = (~pat_id: Id.t, ~guard_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (p, g, b) = (pat("p"), exp("g"), exp("e"));
  {
    group_id: ForallWhereExp,
    form_id: ForallWhereExp,
    abstract: (
      [
        mk_forall_where([[space(), p, space()], [space(), g, space()]]),
        space(),
        b,
      ],
      [
        (Piece.id(p), pat_id),
        (Piece.id(g), guard_id),
        (Piece.id(b), body_id),
      ],
    ),
    explanation:
      Printf.sprintf(
        "Asserts that the [*body*](%s) denotes `true` for every total value of the [*variable*](%s) that satisfies the [*restriction*](%s) — sugar for `forall p -> g ==> e`, written on the binder. Inside the proof the restriction is a hypothesis citable as `where` — a fixed name, so an inner restricted binder shadows it and `alias` is what reaches past that — and citing it is how obligations about the variable get discharged. Every use of the theorem incurs the restriction, instantiated at that use, as an obligation of its own — so a restricted theorem is a conditional rule.",
        body_id |> Id.to_string,
        pat_id |> Id.to_string,
        guard_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: ForallWhere1,
        term:
          mk_example(
            "theorem inv = forall x where x != 0 -> x / x == 1 proof ? in
theorem u = 2 / 2 == 1 proof axiom inv at 0 on 2 / 2 end; axiom refl_eq at 0 on 1 == 1 end in 0",
          ),
        message: "Without the restriction `x / x == 1` is not a theorem, because `0 / 0` is an error rather than `1`. Citing `inv` at `2 / 2` incurs the restriction as the obligation `2 != 0`, which is closed by evaluating it.",
      },
    ],
  };
};

open Haz3lcore;
open ExplainThisForm;
open Example;

/* `fun p where g -> e`: a function contract (Phase 3b of
   docs/prover-obligations.md). The guard is checked once at the
   definition and incurred at each call, in the caller's own vocabulary. */
let single = (~pat_id: Id.t, ~guard_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (p, g, b) = (pat("p"), exp("g"), exp("e"));
  {
    group_id: FunWhereExp,
    form_id: FunWhereExp,
    abstract: (
      [
        mk_fun_where([[space(), p, space()], [space(), g, space()]]),
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
        "A function with a contract: the [*restriction*](%s) constrains the [*parameter*](%s), and the [*body*](%s) only has to be well defined where it holds. The obligation is split in two. Once, at the definition, the body is checked with the restriction assumed — so `fun x where x != 0 -> 1/x` owes nothing. Then each call `f(e)` incurs the restriction at its own argument, stated in the caller's vocabulary (`e != 0`) rather than in terms of the function's insides. The restriction has no runtime effect: the function evaluates exactly as an unrestricted one.",
        guard_id |> Id.to_string,
        pat_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: FunWhere1,
        term:
          mk_example(
            "let f = fun x where x != 0 -> 100 / x in\ntheorem t = forall y -> f(y) == f(y) proof have y != 0 proof ? => ? in 0",
          ),
        message: "The division in the body is covered by the contract, so the definition owes nothing. The call `f(y)` incurs `y != 0`, which is what the `have` here sets out to establish.",
      },
    ],
  };
};

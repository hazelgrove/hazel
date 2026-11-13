open Haz3lcore;
open ExplainThisForm;
open Example;

let single = (~pat_id: Id.t, ~guard_id: Id.t, ~body_id: Id.t): Simple.t => {
  let (p, g, b) = (pat("p"), exp("g"), exp("e"));
  {
    group_id: FunWhereExp,
    form_id: FunWhereExp,
    abstract: (
      [
        mk_fun_where([space(), p, space()], [space(), g, space()]),
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
        "A function whose argument must satisfy a condition. Applied to an argument, it matches the argument against the [*parameter*](%s), then evaluates the [*condition*](%s), which can use the parameter's variables. If the condition is `true`, the [*body*](%s) runs; if it is `false`, the application is an error.",
        pat_id |> Id.to_string,
        guard_id |> Id.to_string,
        body_id |> Id.to_string,
      ),
    examples: [
      {
        sub_id: FunWhere1,
        term: mk_example("let f = fun x where x != 0 -> 100 / x in f(4)"),
        message: "The argument 4 is not 0, so the body runs and the result is 25. `f(0)` would be an error rather than a division by zero.",
      },
    ],
  };
};

open Haz3lcore;
open ExplainThisForm;
open Example;

let mk_where = mk_tile(Where, Exp);

let single = (~body_id: Id.t, ~pat_id: Id.t, ~def_id: Id.t): Simple.t => {
  group_id: WhereExp,
  form_id: WhereExp,
  abstract: {
    let (body, p, def) = (exp("e_body"), pat("p"), exp("e_def"));
    (
      [body, space(), mk_where([[space(), p, space()]]), space(), def],
      [
        (Piece.id(body), body_id),
        (Piece.id(p), pat_id),
        (Piece.id(def), def_id),
      ],
    );
  },
  explanation:
    Printf.sprintf(
      "The [*definition*](%s) is matched against the [*pattern*](%s), and the [*body*](%s) is evaluated with the pattern's variables bound. Equivalent to `let p = e_def in e_body`.",
      def_id |> Id.to_string,
      pat_id |> Id.to_string,
      body_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: Where1,
      term: mk_example("x + 1 where x = 2"),
      message: {|
              The variable x is bound to 2 in x + 1, so the expression evaluates to 3.
              |},
    },
  ],
};

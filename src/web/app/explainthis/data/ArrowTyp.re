open Haz3lcore;
open Example;
open ExplainThisForm;

let typ_arg = typ("ty_arg");
let typ_out = typ("ty_out");
let arrow_typ_coloring_ids =
    (~arg_id: Id.t, ~result_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(typ_arg), arg_id),
  (Piece.id(typ_out), result_id),
];
let arrow_typ_id: form_id = ArrowTyp;
let arrow_typ_form = [typ_arg, space(), arrow(), space(), typ_out];
let arrow_typ_explanation = (~arg_id: Id.t, ~result_id: Id.t): string =>
  Printf.sprintf(
    "This arrow type classifies functions with [*argument type*](%s) and [*output type*](%s).",
    Id.to_string(arg_id),
    Id.to_string(result_id),
  );
let arrow_typ = (~arg_id: Id.t, ~result_id: Id.t): form => {
  id: arrow_typ_id,
  syntactic_form: arrow_typ_form,
  expandable_id: Some((Piece.id(typ_out), [typ("ty_out")])),
  explanation: arrow_typ_explanation(~arg_id, ~result_id),
  examples: [],
};
let typ_arg1 = typ("ty_arg1");
let typ_arg2 = typ("ty_arg2");
let typ_out = typ("ty_out");
let arrow3_typ_coloring_ids =
    (~arg1_id: Id.t, ~arg2_id: Id.t, ~result_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(typ_arg1), arg1_id),
  (Piece.id(typ_arg2), arg2_id),
  (Piece.id(typ_out), result_id),
];
let arrow3_typ_id: form_id = Arrow3Typ;
let arrow3_typ_arrow2 = arrow();
let arrow3_typ_form = [
  typ_arg1,
  space(),
  arrow(),
  space(),
  typ_arg2,
  space(),
  arrow3_typ_arrow2,
  space(),
  typ_out,
];
let arrow3_typ = (~arg1_id: Id.t, ~arg2_id: Id.t, ~result_id: Id.t): form => {
  id: arrow3_typ_id,
  syntactic_form: arrow3_typ_form,
  expandable_id:
    Some((
      Piece.id(arrow3_typ_arrow2),
      [typ("ty_arg2"), arrow(), typ("ty_out")],
    )),
  explanation:
    Printf.sprintf(
      "This arrow type classifies functions with [*first argument type*](%s), [*second argument type*](%s), and [*output type*](%s).",
      Id.to_string(arg1_id),
      Id.to_string(arg2_id),
      Id.to_string(result_id),
    ),
  examples: [],
};

let arrow = (~arg_id: Id.t, ~result_id: Id.t): group => {
  id: ArrowTyp,
  forms: [arrow_typ(~arg_id, ~result_id)],
};

let arrow3 =
    (
      ~arg1_id: Id.t,
      ~arg2_id: Id.t,
      ~result_id: Id.t,
      ~arg_id: Id.t,
      ~arrow_result_id: Id.t,
    )
    : group => {
  id: Arrow3Typ,
  forms: [
    arrow3_typ(~arg1_id, ~arg2_id, ~result_id),
    arrow_typ(~arg_id, ~result_id=arrow_result_id),
  ],
};

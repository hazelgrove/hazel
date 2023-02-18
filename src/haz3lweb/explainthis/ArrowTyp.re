open Haz3lcore;
open ExampleUtil;
open ExplainThisForm;

let arrow_typ_group = "arrow_typ_group";
let arrow3_typ_group = "arrow3_typ_group";
let _typ_arg = typ("ty_arg");
let _typ_out = typ("ty_out");
let arrow_typ_coloring_ids =
    (~arg_id: Id.t, ~result_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_arg), arg_id),
  (Piece.id(_typ_out), result_id),
];
let arrow_typ: form = {
  let explanation = {
    message: "Arrow type. This arrow type classifies functions with [*argument type*](%i) and [*output type*](%i).",
    feedback: Unselected,
  };
  {
    id: "arrow_typ",
    syntactic_form: [_typ_arg, space(), arrow(), space(), _typ_out],
    expandable_id: Some(Piece.id(_typ_out)),
    explanation,
    examples: [],
  };
};
let _typ_arg1 = typ("ty_arg1");
let _typ_arg2 = typ("ty_arg2");
let _typ_out = typ("ty_out");
let arrow3_typ_coloring_ids =
    (~arg1_id: Id.t, ~arg2_id: Id.t, ~result_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_arg1), arg1_id),
  (Piece.id(_typ_arg2), arg2_id),
  (Piece.id(_typ_out), result_id),
];
let arrow3_typ: form = {
  let explanation = {
    message: "Arrow type. This arrow type classifies functions with [*first argument type*](%i), [*second argument type*](%i), and [*output type*](%i).",
    feedback: Unselected,
  };
  let arrow2 = arrow();
  {
    id: "arrow3_typ",
    syntactic_form: [
      _typ_arg1,
      space(),
      arrow(),
      space(),
      _typ_arg2,
      space(),
      arrow2,
      space(),
      _typ_out,
    ],
    expandable_id: Some(Piece.id(arrow2)),
    explanation,
    examples: [],
  };
};

open Haz3lcore;
open Example;
open ExplainThisForm;

let _typ_arg = typ("ty_arg");
let _typ_out = typ("ty_out");
let arrow_typ_coloring_ids =
    (~arg_id: Id.t, ~result_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_arg), arg_id),
  (Piece.id(_typ_out), result_id),
];
let arrow_typ: form = {
  let explanation = "This arrow type classifies functions with [*argument type*](%s) and [*output type*](%s).";
  {
    id: ArrowTyp,
    syntactic_form: [_typ_arg, space(), arrow(), space(), _typ_out],
    expandable_id: Some((Piece.id(_typ_out), [typ("ty_out")])),
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
  let explanation = "This arrow type classifies functions with [*first argument type*](%s), [*second argument type*](%s), and [*output type*](%s).";
  let arrow2 = arrow();
  {
    id: Arrow3Typ,
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
    expandable_id:
      Some((Piece.id(arrow2), [typ("ty_arg2"), arrow(), typ("ty_out")])),
    explanation,
    examples: [],
  };
};

let arrow: group = {id: ArrowTyp, forms: [arrow_typ]};

let arrow3: group = {id: Arrow3Typ, forms: [arrow3_typ, arrow_typ]};

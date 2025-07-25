open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("t_var");
let _typ_arg = typ("ty_arg");
let forall_typ_coloring_ids =
    (~pat_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_tpat), pat_id),
  (Piece.id(_typ_arg), body_id),
];
let forall_typ: form = {
  let explanation = "This forall type allows you to use the bound [*variables*](%s) inside the [*inner type*](%s).";
  {
    id: ForallTyp,
    syntactic_form: [
      mk_forall([[space(), _tpat, space()]]),
      space(),
      _typ_arg,
    ],
    expandable_id: Some((Piece.id(_tpat), [_typ_arg])),
    explanation,
    examples: [],
  };
};

let forall: group = {
  id: ForallTyp,
  forms: [forall_typ],
};

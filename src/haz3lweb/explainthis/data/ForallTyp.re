open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("t_var");
let _typ_arg = typ("ty_arg");
let forall_typ_coloring_ids =
    (~tpat_id: Id.t, ~tbody_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ_arg), tbody_id),
];
let forall_typ: form = {
  let explanation = "This forall type classifies polymorphic values varying over [*type variable*](%s) with [*instantiated type*](%s).";
  {
    id: ForallTyp,
    syntactic_form: [mk_type([[space(), _tpat, space()]]), _typ_arg],
    expandable_id: Some((Piece.id(_tpat), [_typ_arg])),
    explanation,
    examples: [],
  };
};

let forall: group = {id: ForallTyp, forms: [forall_typ]};

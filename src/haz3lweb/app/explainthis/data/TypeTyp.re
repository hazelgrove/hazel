open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("t_var");
let _typ_arg = typ("ty_arg");
let type_typ_coloring_ids =
    (~tpat_id: Id.t, ~tbody_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ_arg), tbody_id),
];
let type_typ: form = {
  let explanation = "This type type classifies polymorphic values varying over [*type variable*](%s) with [*instantiated type*](%s).";
  {
    id: TypeTyp,
    syntactic_form: [mk_type([[space(), _tpat, space()]]), _typ_arg],
    expandable_id: Some((Piece.id(_tpat), [_typ_arg])),
    explanation,
    examples: [],
  };
};

let type_typ: group = {id: TypeTyp, forms: [type_typ]};

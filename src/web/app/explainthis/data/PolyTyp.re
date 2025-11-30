open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("t_var");
let _typ_arg = typ("ty_arg");
let poly_typ_coloring_ids =
    (~tpat_id: Id.t, ~tbody_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ_arg), tbody_id),
];
let poly_typ: form = {
  let explanation = "This poly type classifies polymorphic values varying over [*type variable*](%s) with [*instantiated type*](%s).";
  {
    id: PolyTyp,
    syntactic_form: [mk_poly([[space(), _tpat, space()]]), _typ_arg],
    expandable_id: Some((Piece.id(_tpat), [_typ_arg])),
    explanation,
    examples: [],
  };
};

let poly: group = {
  id: PolyTyp,
  forms: [poly_typ],
};

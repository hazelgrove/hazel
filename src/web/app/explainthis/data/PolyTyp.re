open Haz3lcore;
open Example;
open ExplainThisForm;

let tpat = tpat("t_var");
let typ_arg = typ("ty_arg");
let poly_typ_coloring_ids =
    (~tpat_id: Id.t, ~tbody_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(tpat), tpat_id),
  (Piece.id(typ_arg), tbody_id),
];
let poly_typ_form = [mk_poly([[space(), tpat, space()]]), typ_arg];
let poly_typ = (~tpat_id: Id.t, ~tbody_id: Id.t): form => {
  id: PolyTyp,
  syntactic_form: poly_typ_form,
  colorings: poly_typ_coloring_ids(~tpat_id, ~tbody_id),
  expandable_id: Some((Piece.id(tpat), [typ_arg])),
  explanation:
    Stdlib.Printf.sprintf(
      "This poly type classifies polymorphic values varying over [*type variable*](%s) with [*instantiated type*](%s).",
      Id.to_string(tpat_id),
      Id.to_string(tbody_id),
    ),
  examples: [],
};

let poly = (~tpat_id: Id.t, ~tbody_id: Id.t): group =>
  singleton(poly_typ(~tpat_id, ~tbody_id));

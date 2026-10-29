open Haz3lcore;
open Example;
open ExplainThisForm;

let tpat = tpat("t_var");
let typ_arg = typ("ty_arg");
let rec_typ_coloring_ids =
    (~tpat_id: Id.t, ~tbody_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(tpat), tpat_id),
  (Piece.id(typ_arg), tbody_id),
];
let peano_ex = {
  sub_id: RecTyp,
  term: mk_example("type Peano = \n rec P -> Z + S(P) \n in S(S(S(Z)))"),
  message: "The type of the Peano numbers and the representation of the number 3.",
};
let rec_typ_form = [mk_rec([[space(), tpat, space()]]), typ_arg];
let rec_typ = (~tpat_id: Id.t, ~tbody_id: Id.t): form => {
  id: RecTyp,
  syntactic_form: rec_typ_form,
  colorings: rec_typ_coloring_ids(~tpat_id, ~tbody_id),
  expandable_id: Some((Piece.id(tpat), [typ_arg])),
  explanation:
    Printf.sprintf(
      "This recursive type classifies the least fixed point of the polymorphic type over the [*type variable*](%s) of body [*instantiated type*](%s).",
      Id.to_string(tpat_id),
      Id.to_string(tbody_id),
    ),
  examples: [peano_ex],
};

let rec_ = (~tpat_id: Id.t, ~tbody_id: Id.t): group =>
  singleton(rec_typ(~tpat_id, ~tbody_id));

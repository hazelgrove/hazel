open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("t_var");
let _typ_arg = typ("ty_arg");
let rec_typ_coloring_ids =
    (~tpat_id: Id.t, ~tbody_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ_arg), tbody_id),
];
let peano_ex = {
  sub_id: RecTyp,
  term: mk_example("type Peano = \n rec P -> Z + S(P) \n in S(S(S(Z)))"),
  message: "The type of the Peano numbers and the representation of the number 3.",
};
let rec_typ: form = {
  let explanation = "This recursive type classifies the least fixed point of the polymorphic type over the [*type variable*](%s) of body [*instantiated type*](%s).";
  {
    id: RecTyp,
    syntactic_form: [mk_rec([[space(), _tpat, space()]]), _typ_arg],
    expandable_id: Some((Piece.id(_tpat), [_typ_arg])),
    explanation,
    examples: [peano_ex],
  };
};

let rec_: group = {id: RecTyp, forms: [rec_typ]};

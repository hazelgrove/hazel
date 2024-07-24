open Haz3lcore;
open Example;
open ExplainThisForm;

let _pat = pat("var");
let _typ_arg = typ("ty_arg");
let forall_typ_coloring_ids =
    (~pat_id: Id.t, ~tbody_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat), pat_id),
  (Piece.id(_typ_arg), tbody_id),
];
let forall_typ: form = {
  let explanation = "This forall type states that for every value [*type variable*](%s), the type [*instantiated type*](%s) is inhabited.";
  {
    id: ForallTyp,
    syntactic_form: [mk_type([[space(), _pat, space()]]), _typ_arg],
    expandable_id: Some((Piece.id(_pat), [_typ_arg])),
    explanation,
    examples: [],
  };
};

let forall_typ: group = {id: ForallTyp, forms: [forall_typ]};

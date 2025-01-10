open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("p");
let _typ_def = typ("ty_def");
let TyDef_base_exp_coloring_ids =
  (~tpat_id: Id.t, ~def_id: Id.t) => [
    (Piece.id(_tpat), tpat_id),
    (Piece.id(_typ_def), def_id),
  ];
let TyDef_exp: form = {
  let explanation = "The [*type*](%s) is bound to the [*type variable*](%s) in the body.";
  let form = [
    mk_TyDef([[space(), _tpat, space()], [space(), _typ_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TyDefExp,
    syntactic_form: form,
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let TyDef_exps: group = {id: TyDefExp, forms: [TyDef_exp]};

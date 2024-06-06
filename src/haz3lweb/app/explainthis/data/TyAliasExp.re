open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("p");
let _typ_def = typ("ty_def");
let tyalias_base_exp_coloring_ids = (~tpat_id: Id.t, ~def_id: Id.t) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ_def), def_id),
];
let tyalias_exp: form = {
  let explanation = "The [*type*](%s) is bound to the [*type variable*](%s) in the body.";
  let form = [
    mk_tyalias([[space(), _tpat, space()], [space(), _typ_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TyAliasExp,
    syntactic_form: form,
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tyalias_exps: group = {id: TyAliasExp, forms: [tyalias_exp]};

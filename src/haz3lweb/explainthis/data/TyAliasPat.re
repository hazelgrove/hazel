open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("p");
let _typ_def = typ("ty_def");
let tyalias_base_pat_coloring_ids = (~tpat_id: Id.t, ~def_id: Id.t) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ_def), def_id),
];
let tyalias_pat: form = {
  let explanation = "Type alias pattern. Used in module's type annotation to indicate the [*type*](%s) is bound to the [*type variable member*](%s) in this module";
  let form = [
    mk_tyalias_pat([[space(), _tpat, space()]]),
    space(),
    _typ_def,
  ];
  {
    id: TyAliasPat,
    syntactic_form: form,
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tyalias_pats: group = {id: TyAliasPat, forms: [tyalias_pat]};

open Haz3lcore;
open Example;
open ExplainThisForm;

let tpat = tpat("p");
let typ_def = typ("ty_def");
let tyalias_base_exp_coloring_ids = (~tpat_id: Id.t, ~def_id: Id.t) => [
  (Piece.id(tpat), tpat_id),
  (Piece.id(typ_def), def_id),
];
let tyalias_exp = (~tpat_id: Id.t, ~def_id: Id.t): form => {
  let explanation =
    Printf.sprintf(
      "The [*type*](%s) is bound to the [*type variable*](%s) in the body.",
      Id.to_string(def_id),
      Id.to_string(tpat_id),
    );
  let form = [
    mk_tyalias([[space(), tpat, space()], [space(), typ_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TyAliasExp,
    syntactic_form: form,
    colorings: tyalias_base_exp_coloring_ids(~tpat_id, ~def_id),
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tyalias_exps = (~tpat_id: Id.t, ~def_id: Id.t): group =>
  singleton(tyalias_exp(~tpat_id, ~def_id));

open Haz3lcore;
open ExplainThisForm;
open Example;

let _tpat = tpat("T");
let _typ = typ("ty");

let mod_type_decl_coloring_ids =
    (~tpat_id: Id.t, ~typ_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ), typ_id),
];

let mod_type_decl_form: form = {
  let explanation = "A type declaration defines a [*type alias*](%s) bound to the [*type*](%s), available to subsequent declarations in the enclosing module.";
  {
    id: ModTypeDecl,
    syntactic_form: [
      mk_mod_type([[space(), _tpat, space()]]),
      space(),
      _typ,
    ],
    expandable_id: None,
    explanation,
    examples: [
      {
        sub_id: ModType1,
        term: mk_example("let m = {\ntype T = Int;\nlet x : T = 5\n} in m.x"),
        message: "A module with a type alias T for Int, used to annotate the field x.",
      },
    ],
  };
};

let mod_type_decls: group = {
  id: ModTypeDecl,
  forms: [mod_type_decl_form],
};

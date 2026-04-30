open Haz3lcore;
open ExplainThisForm;
open Example;

let _pat = pat("p");
let _exp = exp("e");

let mod_let_decl_coloring_ids =
    (~pat_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat), pat_id),
  (Piece.id(_exp), def_id),
];

let mod_let_decl_form: form = {
  let explanation = "A let declaration binds a [*value*](%s) to a [*pattern*](%s), exposing it as a field of the enclosing module.";
  {
    id: ModLetDecl,
    syntactic_form: [
      mk_mod_let([[space(), _pat, space()]]),
      space(),
      _exp,
    ],
    expandable_id: None,
    explanation,
    examples: [
      {
        sub_id: ModLet1,
        term: mk_example("let m = {\nlet x = 1;\nlet y = x + 1\n} in m.y"),
        message: "A module with two let declarations. The field y uses x and is accessed via dot notation.",
      },
    ],
  };
};

let mod_let_decls: group = {
  id: ModLetDecl,
  forms: [mod_let_decl_form],
};

open Haz3lcore;
open ExplainThisForm;
open Example;

let _name = exp("M");
let _def = exp("e");

let module_keyword_decl_coloring_ids =
    (~name_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_name), name_id),
  (Piece.id(_def), def_id),
];

let module_keyword_decl_form: form = {
  let explanation = "A nested module declaration binds the [*module expression*](%s) to the name [*%s*](%s), exposing it as a field of the enclosing module (equivalent to a let declaration).";
  {
    id: ModuleKeywordDecl,
    syntactic_form: [
      exp("module"),
      space(),
      _name,
      space(),
      exp("="),
      space(),
      _def,
    ],
    expandable_id: None,
    explanation,
    examples: [
      {
        sub_id: ModuleKeywordDecl1,
        term:
          mk_example(
            "let m = {\nmodule Inner = {\nlet x = 1\n};\nlet y = Inner.x\n} in m.y",
          ),
        message: "A nested module declaration. Inner is defined inside a module and its field x is accessed via dot notation.",
      },
    ],
  };
};

let module_keyword_decls: group = {
  id: ModuleKeywordDecl,
  forms: [module_keyword_decl_form],
};

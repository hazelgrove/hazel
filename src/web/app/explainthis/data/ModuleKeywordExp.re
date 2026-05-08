open Haz3lcore;
open ExplainThisForm;
open Example;

let _name = mpat("M");
let _def = exp("e");
let _body = exp("body");

let module_keyword_exp_coloring_ids =
    (~name_id: Id.t, ~def_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_name), name_id),
  (Piece.id(_def), def_id),
  (Piece.id(_body), body_id),
];

let module_keyword_exp_form: form = {
  let explanation = "A module definition binds the [*module expression*](%s) to the name [*%s*](%s) for use in [*the body*](%s), equivalent to a let definition. The module's fields are accessed with dot notation.";
  {
    id: ModuleKeywordExp,
    syntactic_form: [
      mk_module_keyword_exp([
        [space(), _name, space()],
        [space(), _def, space()],
      ]),
      linebreak(),
      _body,
    ],
    expandable_id: None,
    explanation,
    examples: [
      {
        sub_id: ModuleKeyword1,
        term: mk_example("module M = {\nlet x = 1;\nlet y = true\n} in M.x"),
        message: "A module definition. The module M is defined and its field x is accessed in the body.",
      },
    ],
  };
};

let module_keyword_exps: group = {
  id: ModuleKeywordExp,
  forms: [module_keyword_exp_form],
};

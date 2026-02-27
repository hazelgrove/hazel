open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: ModLetDecl,
  form_id: ModLetDecl,
  abstract: (
    [exp("let"), space(), pat("p"), space(), exp("="), space(), exp("e")],
    [],
  ),
  explanation: "A let declaration in a module binds a value accessible as a field of the module.",
  examples: [
    {
      sub_id: ModLet1,
      term: mk_example("let m = {\nlet x = 1;\nlet y = x + 1\n} in m.y"),
      message: "A module with two let declarations. The field y uses x and is accessed via dot notation.",
    },
  ],
};

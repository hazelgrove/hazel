open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: ModuleKeywordExp,
  form_id: ModuleKeywordExp,
  abstract: (
    [
      exp("module"),
      space(),
      exp("M"),
      space(),
      exp("="),
      space(),
      exp("e"),
      space(),
      exp("in"),
      space(),
      exp("body"),
    ],
    [],
  ),
  explanation: "A module definition names a module expression for use in the body, equivalent to a let definition. The module's fields are accessed with dot notation.",
  examples: [
    {
      sub_id: ModuleKeyword1,
      term: mk_example("module M = {\nlet x = 1;\nlet y = true\n} in M.x"),
      message: "A module definition. The module M is defined and its field x is accessed in the body.",
    },
  ],
};

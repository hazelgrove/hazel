open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: ModuleKeywordDecl,
  form_id: ModuleKeywordDecl,
  abstract: (
    [
      exp("module"),
      space(),
      exp("M"),
      space(),
      exp("="),
      space(),
      exp("e"),
    ],
    [],
  ),
  explanation: "A module declaration inside a module body defines a nested module accessible as a field, equivalent to a let declaration.",
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

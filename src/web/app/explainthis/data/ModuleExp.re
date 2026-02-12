open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: ModuleExp,
  form_id: ModuleExp,
  abstract: ([exp("{ ... }")], []),
  explanation: "A module groups let and type declarations into a single value. Fields are accessed with dot notation.",
  examples: [
    {
      sub_id: Module1,
      term:
        mk_example(
          "let m = {\ntype T = Int;\nlet x : T = 5;\nlet y = true\n} in m.x",
        ),
      message: "A module with a type alias and two let declarations. The field x is accessed via dot notation.",
    },
  ],
};

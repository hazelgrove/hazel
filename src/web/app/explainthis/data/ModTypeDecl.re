open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: ModTypeDecl,
  form_id: ModTypeDecl,
  abstract: (
    [
      exp("type"),
      space(),
      tpat("T"),
      space(),
      exp("="),
      space(),
      typ("ty"),
    ],
    [],
  ),
  explanation: "A type declaration in a module defines a type alias available to subsequent declarations.",
  examples: [
    {
      sub_id: ModType1,
      term: mk_example("let m = {\ntype T = Int;\nlet x : T = 5\n} in m.x"),
      message: "A module with a type alias T for Int, used to annotate the field x.",
    },
  ],
};

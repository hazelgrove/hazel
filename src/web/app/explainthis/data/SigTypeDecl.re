open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: SigTypeDecl,
  form_id: SigTypeDecl,
  abstract: (
    [
      typ("type"),
      space(),
      tpat("T"),
      space(),
      typ("="),
      space(),
      typ("ty"),
    ],
    [],
  ),
  explanation: "A type declaration in a signature introduces a manifest type member: T stands for the given type, later members may mention T, and a module matching the signature must define T as that type.",
  examples: [
    {
      sub_id: SigType1,
      term:
        mk_example(
          "let m : {\ntype T = Int;\nlet x : T\n} = {\ntype T = Int;\nlet x = 5\n} in m",
        ),
      message: "A signature with a type member T equal to Int, used to annotate member x. The module's own `type T = Int` is checked against it.",
    },
  ],
};

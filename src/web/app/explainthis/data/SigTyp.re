open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: SigTyp,
  form_id: SigTyp,
  abstract: ([typ("{ ... }")], []),
  explanation: "A module signature specifies the names and types of a module's fields.",
  examples: [
    {
      sub_id: Sig1,
      term:
        mk_example(
          "let m : {\nlet x : Int;\nlet y : Bool\n} = {\nlet x = 1;\nlet y = true\n} in m.x",
        ),
      message: "A module annotated with a signature requiring fields x of type Int and y of type Bool.",
    },
  ],
};

open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: SigLetDecl,
  form_id: SigLetDecl,
  abstract: ([typ("let"), space(), pat("p"), space(), typ(":"), space(), typ("ty")], []),
  explanation: "A let declaration in a signature specifies the expected type of a module field.",
  examples: [
    {
      sub_id: SigLet1,
      term:
        mk_example(
          "let m : {\nlet x : Int;\nlet y : Bool\n} = {\nlet x = 1;\nlet y = true\n} in m",
        ),
      message: "A signature with two let declarations specifying the types of fields x and y.",
    },
  ],
};

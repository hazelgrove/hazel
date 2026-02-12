open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: SigTypeDecl,
  form_id: SigTypeDecl,
  abstract: ([typ("type"), space(), tpat("T"), space(), typ("="), space(), typ("ty")], []),
  explanation: "A type declaration in a signature defines a type alias within the signature. Note: type declarations in signatures are currently limited -- they are parsed but not yet used during type checking. See the Modules documentation for details.",
  examples: [
    {
      sub_id: SigType1,
      term:
        mk_example(
          "let m : {\ntype T = Int;\nlet x : T\n} = {\ntype T = Int;\nlet x = 5\n} in m",
        ),
      message: "A signature with a type alias T for Int used to annotate field x. Note: T appears as a static error in `let x : T` because type declarations in signatures are not yet used during type checking.",
    },
  ],
};

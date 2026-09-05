open ExplainThisForm;
open Example;

let single: Simple.t = {
  group_id: SigTypeAbstractDecl,
  form_id: SigTypeAbstractDecl,
  abstract: ([typ("type"), space(), tpat("T")], []),
  explanation: "An abstract type declaration in a signature introduces a type member T without a definition: later members may mention T, a module matching the signature must define T, and outside the module T is opaque, known only as the path M.T.",
  examples: [
    {
      sub_id: SigTypeAbstract1,
      term:
        mk_example(
          "module C : {\ntype T;\nlet zero : T;\nlet get : T -> Int\n} = {\ntype T = Int;\nlet zero = 0;\nlet get = fun t -> t\n} in C.get(C.zero)",
        ),
      message: "The signature hides that T is Int: C.zero has the abstract type C.T, so C.zero + 1 would be an error, while C.get(C.zero) is fine.",
    },
  ],
};

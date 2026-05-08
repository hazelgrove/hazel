open ExplainThisForm;
open Example;

/* A plain module expression `{ … }` is self-contained — the body is
   a list of declarations and has no distinguished sub-term to
   highlight — so this form has no runtime-id-based coloring. */

let module_exp_form: form = {
  let explanation = "A module groups let and type declarations into a single value. Fields are accessed with dot notation.";
  {
    id: ModuleExp,
    syntactic_form: [exp("{ ... }")],
    expandable_id: None,
    explanation,
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
};

let module_exps: group = {
  id: ModuleExp,
  forms: [module_exp_form],
};

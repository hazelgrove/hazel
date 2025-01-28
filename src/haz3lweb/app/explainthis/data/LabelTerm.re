open ExplainThisForm;
open Example;

let label = (n: string): form => {
  let explanation = "`%s` is a label for an element within a tuple.";
  {
    id: Label,
    syntactic_form: [n |> abbreviate |> exp],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let labels = (n: string): group => {id: Label, forms: [label(n)]};

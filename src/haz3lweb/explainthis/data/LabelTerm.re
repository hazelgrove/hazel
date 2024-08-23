open ExplainThisForm;
open Example;

let label = (n: string): form => {
  let explanation = "`%s` is a label (or name) for an item within a tuple.";
  {
    id: Label,
    syntactic_form: [n |> abbreviate |> tpat], // TODO: Fix this
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let labels = (n: string): group => {id: Label, forms: [label(n)]};

open ExplainThisForm;
open Example;

let label = (n: string): form => {
  id: Label,
  syntactic_form: [n |> abbreviate |> exp],
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "`%s` is a label for an element within a Tuple or Tuple type.",
      n,
    ),
  examples: [],
};
let labels = (n: string): group => singleton(label(n));

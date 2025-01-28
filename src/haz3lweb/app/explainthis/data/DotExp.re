open Haz3lcore;
open ExplainThisForm;
open Example;

let dot_example_1 = {
  sub_id: Dot1,
  term: mk_example("(x=1, y=2).x"),
  message: "Retrieves the element in the tuple associated with the label 'x', which in this example is 1.",
};
let syntactic_form: Exp.t =
  Dot(
    Tuple([
      TupLabel(Label("label") |> Exp.fresh, Var("e") |> Exp.fresh)
      |> Exp.fresh,
    ])
    |> Exp.fresh,
    Label("label") |> Exp.fresh,
  )
  |> Exp.fresh;

let dot_exp: form = {
  let explanation = "Dot Operator explanation";
  {
    id: DotExp,
    syntactic_form:
      ExpToSegment.(
        exp_to_segment(
          ~settings=Settings.of_core(~inline=true, CoreSettings.on),
          syntactic_form,
        )
      ),
    expandable_id: None,
    explanation,
    examples: [dot_example_1],
  };
};

let dot_exp: group = {id: DotExp, forms: [dot_exp]};

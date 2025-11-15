open Haz3lcore;
open Language;
open ExplainThisForm;
open Example;

let dot_example_1 = {
  sub_id: Dot1,
  term: mk_example("(x=1, 4, y=2).x"),
  message: "Retrieves the element in the tuple associated with the label 'x', which in this example is 1.",
};

let dot_example_2 = {
  sub_id: Dot2,
  term: mk_example("[(x=1, y=2), (x=3, y=4)].x"),
  message: "Broadcasts the label 'x' across the list of labeled tuples, retrieving [1, 3] in this example.",
};

let tup = Var("e") |> Exp.fresh;
let lab = Label("label") |> Exp.fresh;

let dot_coloring_ids = (~tup_id: Id.t, ~lab_id: Id.t): list((Id.t, Id.t)) => {
  [(Exp.rep_id(tup), tup_id), (Exp.rep_id(lab), lab_id)];
};
let syntactic_form: Exp.t = Dot(tup, lab) |> Exp.fresh;

let dot_exp: form = {
  let explanation = "The [*label*](%s) is being projected from the [*tuple*](%s).";

  {
    id: DotExp,
    syntactic_form:
      ExpToSegment.(
        exp_to_segment(
          ~settings=Settings.of_core(~inline=Single, CoreSettings.on),
          syntactic_form,
        )
      ),
    expandable_id: None,
    explanation,
    examples: [dot_example_1, dot_example_2],
  };
};

let dot_exp: group = {
  id: DotExp,
  forms: [dot_exp],
};

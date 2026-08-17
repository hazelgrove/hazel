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

let dot_exp_segment =
  ExpToSegment.(
    exp_to_segment(
      ~settings=Settings.of_core(~inline=Inline, CoreSettings.on),
      syntactic_form,
    )
  );

let dot_exp_form = (~lab_id: Id.t, ~tup_id: Id.t): form => {
  id: DotExp,
  syntactic_form: dot_exp_segment,
  colorings: dot_coloring_ids(~tup_id, ~lab_id),
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "The [*label*](%s) is being projected from the [*tuple*](%s).",
      Id.to_string(lab_id),
      Id.to_string(tup_id),
    ),
  examples: [dot_example_1, dot_example_2],
};

let dot_exp = (~lab_id: Id.t, ~tup_id: Id.t): group =>
  singleton(dot_exp_form(~lab_id, ~tup_id));

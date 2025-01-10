open Virtual_dom.Vdom;
open Node;
open ExerciseUtil;

let prompt =
  div([
    p([
      text("let us derive the judgement "),
      code("e_example"),
      text(" ⇓ 4."),
    ]),
  ]);

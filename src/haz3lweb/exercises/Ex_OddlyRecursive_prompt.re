open Virtual_dom.Vdom;
open Node;
open ExerciseUtil;

let prompt =
  div([
    p([
      text(
        "Write a function that determines whether the given integer is odd. ",
      ),
    ]),
    p([
      code("odd(n)"),
      equiv,
      code("true"),
      text(" iff "),
      code("n"),
      text(" is odd."),
    ]),
  ]);

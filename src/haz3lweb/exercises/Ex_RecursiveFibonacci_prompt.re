open Virtual_dom.Vdom;
open Node;
open ExerciseUtil;

let prompt =
  div([
    p([
      div([
        text(
          "Write tests cases for, and then implement, a function, that recursively determines the nth fibonacci number.",
        ),
      ]),
    ]),
    p([
      code("fib n"),
      text(" should return the "),
      code("n"),
      text("th fibonacci number, assuming "),
      code("n >= 0."),
    ]),
  ]);

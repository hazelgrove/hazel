open Virtual_dom.Vdom;
open Node;
open ExerciseUtil;

let prompt =
  div([
    p([
      text(
        "Provide a derivation of the following judgement, which establishes that the curried “min” function in ALFp has type: ",
      ),
      code("Num → Num → Num"),
    ]),
  ]);

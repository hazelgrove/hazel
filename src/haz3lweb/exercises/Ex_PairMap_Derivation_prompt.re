open Virtual_dom.Vdom;
open Node;
open ExerciseUtil;

let prompt =
  div([
    p([
      text(
        "Provide a derivation using the Bidirectional Type System rules of the following judgement, which synthesizes a type for the ",
      ),
      code("pairNegate"),
      text(" function shown above as being defined by partially applying "),
      code("pairmap"),
      text(
        ". This derivation shows how type analysis allows us to avoid having to annotate the function argument.",
      ),
    ]),
  ]);

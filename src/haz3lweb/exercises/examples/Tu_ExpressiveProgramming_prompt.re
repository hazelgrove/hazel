open Virtual_dom.Vdom;
open Node;
open ExerciseUtil;

let prompt =
  div([
    // p([text("    ____     ")]),
    p([
      text(
        "You might not have realized it, but you wrote your
  first computer programs in grade school in the form of
  arithmetic expressions!",
      ),
    ]),
    p([
      text("For example, enter the program "),
      code("2 + 2"),
      text(
        " in the expression editor below.
          Hazel operates like a calculator, computing the value of your expression by equationally simplifying it (i.e. evaluating it), here to the integer value ",
      ),
      code("4"),
      text(". "),
      text("The symbol "),
      code("≡"),
      text(" is pronounced \"is equivalent to\"."),
    ]),
  ]);

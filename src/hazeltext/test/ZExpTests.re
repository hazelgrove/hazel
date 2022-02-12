let%test _ =
  ZExpTester.test(
    "λx : Int.{x}",
    ([0, 0, 0, 0, 1, 0], OnText(3)),
    [Backspace, Backspace, Backspace],
    "λx : ?.{x}",
  );

open Alcotest;
open Haz3lcore;
open Web;

/* Each of these memos once wrapped a curried function, so it cached only
   the partial application on the first argument and reran the body on
   every call. A repeat call must now return the cached value itself. */
let same = (name: string, f: unit => 'a): unit =>
  check(
    bool,
    name ++ ": a repeat call returns the cached value",
    true,
    f() === f(),
  );

let tests = (
  "Memo",
  [
    test_case("Code token per token and styling", `Quick, () =>
      same("Code.of_delim'", () =>
        Code.of_delim'((
          "let",
          3,
          Exp,
          true,
          false,
          true,
          false,
          FontMetrics.init,
        ))
      )
    ),
    test_case("Empty hole glyph per font metrics and shape", `Quick, () =>
      same("EmptyHoleDec.view", () =>
        EmptyHoleDec.view((FontMetrics.init, Concave))
      )
    ),
    test_case("Card per sort and card", `Quick, () =>
      same("CardView.Card.view", () =>
        CardView.Card.view((Exp, (Hearts, Ace)))
      )
    ),
  ],
);

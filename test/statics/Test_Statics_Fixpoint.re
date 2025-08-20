open Test_Statics_Prelude;
open Alcotest;
let tests = [
  test_case("Fixpoint with no arguments", `Quick, () => {
    annotated_tree_test(
      "fix () -> []",
      Language.IdTagged.FreshGrammar.Typ.(prod([])),
      FIError.(
        Exp.(
          fix_f(
            Pat.tuple([]),
            list_lit(
              ~ann=
                Some(
                  Exp(
                    Common(
                      FTemp.Typ.(
                        Inconsistent(
                          Expectation({
                            ana: prod([]),
                            syn: list(unknown(Internal)),
                          }),
                        )
                      ),
                    ),
                  ),
                ),
              [],
            ),
            None,
          )
        )
      ),
    )
  }),
];

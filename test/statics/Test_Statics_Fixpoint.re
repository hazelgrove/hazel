open Test_Statics_Prelude;
open Alcotest;
let tests = (
  "Statics.Fixpoint",
  [
    test_case("Fixpoint with no arguments", `Quick, () => {
      annotated_tree_test(
        "fix () -> []",
        FTemp.Typ.(prod([])),
        FIError.(
          Exp.(
            fix_f(
              Pat.tuple([]),
              list_lit(
                ~ann=
                  Some(
                    Marks([
                      ExpectationMismatch({
                        ana: FTemp.Typ.prod([]),
                        syn: FTemp.Typ.list(FTemp.Typ.unknown(Internal)),
                      }),
                    ]),
                  ),
                [],
              ),
              None,
            )
          )
        ),
      )
    }),
  ],
);

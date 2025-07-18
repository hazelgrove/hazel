open Test_Statics_Prelude;
open Alcotest;
let tests = [
  test_case("Fixpoint with no arguments", `Quick, () => {
    annotated_tree_test(
      "fix () -> []",
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
                            syn:
                              list(
                                unknown(
                                  Syn,
                                  Language.Hole.temp(EmptyHole),
                                  Atom,
                                ),
                              ),
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

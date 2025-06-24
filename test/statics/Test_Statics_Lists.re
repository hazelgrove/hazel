open Test_Statics_Prelude;
open FTemp;
open Alcotest;
open Typ;

let tests = (
  "Statics.Lists",
  [
    test_case("A @ A", `Quick, () => {
      annotated_tree_test(
        "A @ A",
        list(unknown(Internal)),
        FIError.Exp.(
          let a =
            constructor(
              ~ann=Some(Exp(Common(NoType(FreeConstructor("A"))))),
              "A",
              None,
            );
          list_concat(a, a)
        ),
      )
    }),
  ],
);

open Test_Statics_Prelude;
open FTemp;
open Alcotest;
open Typ;
open TypeProvenance;

let tests = (
  "Statics.Lists",
  [
    test_case("A @ A", `Quick, () => {
      annotated_tree_test(
        "A @ A",
        list(unknown(internal())),
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
    test_case("Free variable list concatenation", `Quick, () => {
      annotated_tree_test(
        "a @ a",
        list(unknown(internal())),
        FIError.Exp.(
          let a = var(~ann=Some(Exp(FreeVariable("a"))), "a");
          list_concat(a, a)
        ),
      )
    }),
    inconsistent_typecheck(
      "list cons inconsistent tail",
      {|1::["str"]|} |> parse_exp,
    ),
    fully_consistent_typecheck(
      "list cons consistent tail",
      {|1::[2]|},
      Some(list(int())),
    ),
  ],
);

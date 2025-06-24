open Alcotest;
open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = (
  "Statics.Polymorphism",
  [
    test_case("Example error annotations", `Quick, () => {
      annotated_tree_test(
        "Inconsistent expectation on plus",
        Typ.int(),
        FIError.Exp.(
          bin_op(
            Int(Plus),
            int(1),
            string(
              ~ann=
                Some(
                  FTemp.Typ.(
                    Exp(
                      Common(
                        Inconsistent(
                          Expectation({
                            ana: int(),
                            syn: string(),
                          }),
                        ),
                      ),
                    )
                  ),
                ),
              "hello",
            ),
          )
        ),
      )
    }),
    fully_consistent_typecheck(
      "Forall alpha equivalent in ascription",
      {|let x : forall a -> a = in (x : forall b -> b)|},
      FTemp.Typ.(Some(forall(TPat.var("b"), var("b")))),
    ),
    fully_consistent_typecheck(
      "Forall alpha equivalent in let",
      {|let x : forall a -> a = in let y : forall b -> b = x in 1|},
      Some(int()),
    ),
  ],
);

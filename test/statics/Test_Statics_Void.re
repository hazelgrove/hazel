open Test_Statics_Prelude;
open Alcotest;
open FTemp;
open Typ;

let tests = (
  "Statics.Void",
  [
    fully_consistent_typecheck(
      "Void absurd eliminator",
      {|
let diverge : () -> Void =
  fun () -> diverge()
in
let absurd : Void -> Int =
  fun v -> case v end
in
absurd(diverge())
      |},
      Some(int()),
    ),
    fully_consistent_typecheck(
      "empty case synthesizes unknown",
      "fun v : Void -> case v end",
      Some(arrow(sum([]), unknown(Internal))),
    ),
    test_case("cannot inhabit Void", `Quick, () =>
      annotated_tree_test(
        "let v : Void = 1 in v",
        sum([]),
        FIError.(
          Exp.(
            let_(
              Pat.(asc(var("v"), Typ.(sum([])))),
              int(
                ~ann=
                  Some(
                    FTemp.Typ.(
                      Marks([
                        ExpectationMismatch({
                          ana: sum([]),
                          syn: int(),
                        }),
                      ])
                    ),
                  ),
                1,
              ),
              var("v"),
            )
          )
        ),
      )
    ),
    test_case("shadows base type Void", `Quick, () =>
      annotated_tree_test(
        "type Void = ? in ?",
        unknown(Internal),
        FIError.(
          Exp.(
            ty_alias(
              TPat.var(
                ~ann=Some(Marks([TPatShadowsType("Void", BaseTyp)])),
                "Void",
              ),
              Typ.(unknown(Hole(EmptyHole))),
              empty_hole(),
            )
          )
        ),
      )
    ),
  ],
);

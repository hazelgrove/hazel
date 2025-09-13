open Alcotest;
open Test_Statics_Prelude;
open FTemp;
open Typ;
open TypeProvenance;

let tests = (
  "Statics.Functions",
  [
    fully_consistent_typecheck(
      "Function with unknown param",
      "fun x -> 4 + 5",
      Some(Typ.(arrow(unknown(internal()), int()))),
    ),
    fully_consistent_typecheck(
      "Function with known param",
      "fun x : Int -> 4 + 5",
      Some(arrow(int(), int())),
    ),
    fully_consistent_typecheck(
      "Function with labeled param",
      "fun (a=x) -> 4",
      Some(
        arrow(prod([tup_label(label("a"), unknown(internal()))]), int()),
      ),
    ),
    fully_consistent_typecheck(
      "bifunction",
      "fun x : Int, y: Int -> x + y",
      Some(arrow(prod([int(), int()]), int())),
    ),
    fully_consistent_typecheck(
      "bifunction",
      "fun x : Int, y: Int -> x + y",
      Some(arrow(prod([int(), int()]), int())),
    ),
    fully_consistent_typecheck(
      "function application",
      "float_of_int(1)",
      Some(float()),
    ),
    fully_consistent_typecheck(
      "function deferral",
      "string_sub(\"hello\", 1, _)",
      Some(arrow(int(), string())),
    ),
    fully_consistent_typecheck(
      "Fixpoint in function position",
      {|(fix f : (Int -> Int) -> fun x -> x + 1)(3)|},
      Some(int()),
    ),
    fully_consistent_typecheck(
      "Deferrals applied to known type",
      {|string_sub(_, 2, 3)|},
      Some(arrow(string(), string())),
    ),
    fully_consistent_typecheck(
      "Deferrals applied to unknown type",
      {|?(1, _, _)|},
      Some(
        arrow(
          prod([unknown(internal()), unknown(internal())]),
          unknown(internal()),
        ),
      ),
    ),
    test_case("Wrong number of deferrals", `Quick, () =>
      annotated_tree_test(
        {|string_sub(_, _, 2, 3)|},
        unknown(internal()),
        FIError.Exp.(
          deferred_ap(
            ~ann=
              Some(
                Exp(
                  BadPartialAp(
                    ArityMismatch({
                      expected: 3,
                      actual: 4,
                    }),
                  ),
                ),
              ),
            var("string_sub"),
            [deferral(InAp), deferral(InAp), int(2), int(3)],
          )
        ),
      )
    ),
    test_case("Unknown arg could be any arity for deferrals", `Quick, () =>
      annotated_tree_test(
        {|(? : (? -> ?))(1, _, _)|},
        arrow(
          prod([unknown(internal()), unknown(internal())]),
          unknown(internal()),
        ),
        FIError.(
          Exp.(
            deferred_ap(
              asc(
                empty_hole(),
                Typ.(
                  TypeProvenance.(
                    arrow(
                      unknown(hole(EmptyHole)),
                      unknown(hole(EmptyHole)),
                    )
                  )
                ),
              ),
              [int(1), deferral(InAp), deferral(InAp)],
            )
          )
        ),
      )
    ),
    test_case("Deferral outside of function args", `Quick, () =>
      annotated_tree_test(
        {|1 + _|},
        int(),
        FIError.Exp.(
          bin_op(
            Int(Plus),
            int(1),
            deferral(~ann=Some(Exp(UnusedDeferral)), OutsideAp),
          )
        ),
      )
    ),
  ],
);

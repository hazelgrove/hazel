open Alcotest;
open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = (
  "Statics.Functions",
  [
    fully_consistent_typecheck(
      "Function with unknown param",
      "fun x -> 4 + 5",
      Some(Typ.(arrow(unknown(Internal), int()))),
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
        arrow(prod([tup_label(label("a"), unknown(Internal))]), int()),
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
      "diff accepts a variable as its second argument",
      "let x = 1.0 in diff(x, x)",
      Some(float()),
    ),
    fully_consistent_typecheck(
      "diff accepts a parenthesized variable as its second argument",
      "let x = 1.0 in diff(x, (x))",
      Some(float()),
    ),
    test_case("diff rejects a non-variable second argument", `Quick, () =>
      annotated_tree_test(
        "diff(1.0, 2.0)",
        float(),
        FIError.Exp.(
          ap(
            Forward,
            var("diff"),
            tuple([
              float(1.0),
              float(
                ~ann=Some(Marks([BuiltinError(DiffVariableRequired)])),
                2.0,
              ),
            ]),
          )
        ),
      )
    ),
    test_case(
      "diff rejects an expression as its second argument",
      `Quick,
      () => {
        let exp = parse_exp("diff(1.0, float_of_int(2))");
        let static_map = statics(exp);
        let actual =
          errors(static_map) |> List.map(((_, marks)) => Marks(marks));
        Alcotest.check(
          Alcotest.list(testable_issue),
          "Static Errors",
          [Marks([BuiltinError(DiffVariableRequired)])],
          actual,
        );
      },
    ),
    test_case(
      "deferred diff rejects a non-variable second argument",
      `Quick,
      () => {
        let exp = parse_exp("diff(_, 2.0)");
        let static_map = statics(exp);
        let actual =
          errors(static_map) |> List.map(((_, marks)) => Marks(marks));
        Alcotest.check(
          Alcotest.list(testable_issue),
          "Static Errors",
          [Marks([BuiltinError(DiffVariableRequired)])],
          actual,
        );
      },
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
          prod([unknown(Internal), unknown(Internal)]),
          unknown(Internal),
        ),
      ),
    ),
    test_case("Wrong number of deferrals", `Quick, () =>
      annotated_tree_test(
        {|string_sub(_, _, 2, 3)|},
        unknown(Internal),
        FIError.Exp.(
          deferred_ap(
            ~ann=
              Some(
                Marks([
                  IsBadPartialAp(
                    ArityMismatch({
                      expected: 3,
                      actual: 4,
                    }),
                  ),
                ]),
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
          prod([unknown(Internal), unknown(Internal)]),
          unknown(Internal),
        ),
        FIError.(
          Exp.(
            deferred_ap(
              asc(
                empty_hole(),
                Typ.(
                  arrow(
                    unknown(Hole(EmptyHole)),
                    unknown(Hole(EmptyHole)),
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
            deferral(~ann=Some(Marks([IsDeferral(OutsideAp)])), OutsideAp),
          )
        ),
      )
    ),
  ],
);

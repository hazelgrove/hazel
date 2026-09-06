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
    /* A body that already reports its mismatch with the expected return type
       is not repeated on the function (#2507), as for a tuple component. */
    test_case(
      "A body mismatch is reported once, on the body",
      `Quick,
      () => {
        let marks =
          statics(
            parse_exp({|let f : Int -> Bool = fun x -> x + 1 in f(1)|}),
          )
          |> errors
          |> List.concat_map(snd);
        check(Alcotest.int, "one error", 1, List.length(marks));
        check(
          Alcotest.bool,
          "on the body",
          true,
          switch (marks) {
          | [ExpectationMismatch({ana, syn})] =>
            switch (Language.Typ.term_of(ana), Language.Typ.term_of(syn)) {
            | (Atom(Bool), Atom(Int)) => true
            | _ => false
            }
          | _ => false
          },
        );
      },
    ),
    test_case(
      "A body mismatch is reported once in an argument function",
      `Quick,
      () => {
        let marks =
          statics(
            parse_exp(
              {|let g = fun (f : Int -> Bool) -> f(1) in g(fun x -> x + 1)|},
            ),
          )
          |> errors
          |> List.concat_map(snd);
        check(Alcotest.int, "one error", 1, List.length(marks));
      },
    ),
    /* A mismatch of the definition's own shape is still reported on it. */
    test_case(
      "A non-function against an arrow type is reported on itself",
      `Quick,
      () => {
        let marks =
          statics(parse_exp({|let f : Int -> Bool = 3 in f|}))
          |> errors
          |> List.concat_map(snd);
        check(Alcotest.int, "one error", 1, List.length(marks));
        check(
          Alcotest.bool,
          "on the definition",
          true,
          switch (marks) {
          | [ExpectationMismatch({ana, syn})] =>
            switch (Language.Typ.term_of(ana), Language.Typ.term_of(syn)) {
            | (Arrow(_), Atom(Int)) => true
            | _ => false
            }
          | _ => false
          },
        );
      },
    ),
  ],
);

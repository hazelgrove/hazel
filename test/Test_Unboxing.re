open Alcotest;
open Language;

module F = IdTagged.FreshGrammar;

let exp = testable(Exp.pp, DHExp.fast_equal);

let unboxed_testable = (inner_testable: testable('a)) =>
  testable(
    Fmt.using(Unboxing.show_unboxed(pp(inner_testable)), Fmt.string),
    Unboxing.equal_unboxed(equal(inner_testable)),
  );

let big_int_testable =
  testable(Fmt.using(Bigint.to_string, Fmt.string), Bigint.equal);
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), Exp.fast_equal);

let test_does_not_match = (name, type_testable, request, term) =>
  test_case(
    name,
    `Quick,
    () => {
      check(
        unboxed_testable(type_testable),
        "Should not match.",
        Unboxing.DoesNotMatch,
        Unboxing.unbox(request, term),
      );
      ();
    },
  );

let test_indet_match = (name, type_testable, request, term) =>
  test_case(
    name,
    `Quick,
    () => {
      check(
        unboxed_testable(type_testable),
        "Should indeterminately match.",
        Unboxing.IndetMatch,
        Unboxing.unbox(request, term),
      );
      ();
    },
  );

let test_matches = (name, type_testable, request, term, unboxed) =>
  test_case(
    name,
    `Quick,
    () => {
      check(
        unboxed_testable(type_testable),
        "Should match.",
        Unboxing.Matches(unboxed),
        Unboxing.unbox(request, term),
      );
      ();
    },
  );

open IdTagged.FreshGrammar;
let tests = (
  "Unboxing",
  Unboxing.(
    Exp.[
      // ListLit requests
      test_matches(
        "ListLit to ListLit",
        list(dhexp_typ),
        ListLit,
        list_lit([1, 2, 3] |> List.map(int)),
        [1, 2, 3] |> List.map(int),
      ),
      test_does_not_match(
        "ListLit to ListLitn, incorrect length",
        list(dhexp_typ),
        ListLitn(2),
        list_lit([1, 2, 3] |> List.map(int)),
      ),
      test_indet_match(
        "Cons to ListLit",
        list(dhexp_typ),
        ListLit,
        cons(1 |> int, cons(2 |> int, empty_hole())),
      ),
      test_indet_match(
        "Ascribed Hole to ListLit",
        list(dhexp_typ),
        ListLit,
        asc(empty_hole(), Typ.(list(empty_hole()))),
      ),
      // ListLitn requests
      test_matches(
        "ListLit to ListLitn, correct length",
        list(dhexp_typ),
        ListLitn(3),
        list_lit([1, 2, 3] |> List.map(int)),
        [1, 2, 3] |> List.map(int),
      ),
      test_indet_match(
        "Cons to ListLitn, length > cons: indet match",
        list(dhexp_typ),
        ListLitn(3),
        cons(1 |> int, cons(2 |> int, empty_hole())),
      ),
      test_does_not_match(
        "Cons to ListLitn, length < cons: does not match",
        list(dhexp_typ),
        ListLitn(1),
        cons(1 |> int, cons(2 |> int, empty_hole())),
      ),
      test_matches(
        "ListLitn to Cons, correct length",
        list(dhexp_typ),
        ListLitn(3),
        cons(1 |> int, list_lit([int(2), int(3)])),
        [1, 2, 3] |> List.map(int),
      ),
      test_indet_match(
        "Ascribed Hole to ListLitn",
        list(dhexp_typ),
        ListLitn(0),
        asc(empty_hole(), Typ.(list(empty_hole()))),
      ),
      // Cons requests
      test_matches(
        "ListLit to Cons: empty tail",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        list_lit([1] |> List.map(int)),
        (1 |> int, list_lit([])),
      ),
      test_matches(
        "ListLit to Cons: non-empty tail",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        list_lit([1, 2, 3] |> List.map(int)),
        (1 |> int, list_lit([2, 3] |> List.map(int))),
      ),
      test_does_not_match(
        "EmptyList to Cons",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        list_lit([]),
      ),
      test_matches(
        "Cons to Cons",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        cons(1 |> int, cons(2 |> int, empty_hole())),
        (1 |> int, cons(2 |> int, empty_hole())),
      ),
      test_indet_match(
        "Ascribed Hole to Cons",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        asc(empty_hole(), Typ.(list(empty_hole()))),
      ),
      test_case("Unboxing integer", `Quick, () => {
        check(
          unboxed_testable(big_int_testable),
          "8",
          Matches(Bigint.of_int(8)),
          unbox(Atom(Int), F.Exp.int(8)),
        )
      }),
      test_case(
        "Pivot request",
        `Quick,
        () => {
          let actual =
            unbox(
              TupleElementPivot("a"),
              F.Exp.(
                tuple([
                  tup_label(label("a"), string("aval")),
                  tup_label(label("b"), string("bval")),
                ])
              ),
            );
          let expected = (
            "aval",
            F.Exp.[tup_label(label("b"), string("bval"))],
          );
          check(
            unboxed_testable(pair(Alcotest.string, list(exp))),
            "Pivot request",
            Matches(expected),
            actual,
          );
        },
      ),
      test_case(
        "Dot projection of casted tup label",
        `Quick,
        () => {
          open F;
          let orig =
            Exp.(
              tuple([
                asc(
                  tup_label(
                    label("j"),
                    asc(int(1), Typ.unknown(Internal)),
                  ),
                  Typ.unknown(Internal),
                ),
                asc(int(3), Typ.unknown(Internal)),
              ])
            );
          check(
            unboxed_testable(exp),
            "Dot projection of casted tup label",
            Matches(orig),
            unbox(LabeledTupleProjection("j"), orig),
          );
        },
      ),
    ]
  ),
);

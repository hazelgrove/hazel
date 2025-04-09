open Alcotest;
open Haz3lcore;

let unboxed_testable = (inner_testable: testable('a)) =>
  testable(
    Fmt.using(Unboxing.show_unboxed(pp(inner_testable)), Fmt.string),
    Unboxing.equal_unboxed(equal(inner_testable)),
  );
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

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

let int_exp = i => Atom(Int(Bigint.of_int(i))) |> DHExp.fresh;

let tests = (
  "Unboxing",
  Unboxing.(
    DHExp.[
      // ListLit requests
      test_matches(
        "ListLit to ListLit",
        list(dhexp_typ),
        ListLit,
        ListLit([1, 2, 3] |> List.map(int_exp)) |> fresh,
        [1, 2, 3] |> List.map(int_exp),
      ),
      test_does_not_match(
        "ListLit to ListLitn, incorrect length",
        list(dhexp_typ),
        ListLitn(2),
        ListLit([1, 2, 3] |> List.map(int_exp)) |> fresh,
      ),
      test_indet_match(
        "Cons to ListLit",
        list(dhexp_typ),
        ListLit,
        Cons(1 |> int_exp, Cons(2 |> int_exp, hole([]) |> fresh) |> fresh)
        |> fresh,
      ),
      test_indet_match(
        "CastedHole to ListLit",
        list(dhexp_typ),
        ListLit,
        Cast(
          hole([]) |> fresh,
          Typ.(hole([]) |> fresh),
          Typ.(List(hole([]) |> fresh) |> fresh),
        )
        |> fresh,
      ),
      test_matches(
        "CastedList to ListLit",
        list(dhexp_typ),
        ListLit,
        Cast(
          ListLit([1, 2, 3] |> List.map(int_exp)) |> fresh,
          Typ.(List(hole([]) |> fresh) |> fresh),
          Typ.(List(hole([]) |> fresh) |> fresh),
        )
        |> fresh,
        [1, 2, 3] |> List.map(int_exp),
      ),
      test_matches(
        "DoubleCastedList to ListLit",
        list(dhexp_typ),
        ListLit,
        Cast(
          Cast(
            ListLit([1, 2, 3] |> List.map(int_exp)) |> fresh,
            Typ.(List(hole([]) |> fresh) |> fresh),
            Typ.(List(hole([]) |> fresh) |> fresh),
          )
          |> fresh,
          Typ.(List(hole([]) |> fresh) |> fresh),
          Typ.(List(Atom(Int) |> fresh) |> fresh),
        )
        |> fresh,
        [1, 2, 3]
        |> List.map(i =>
             Cast(
               i |> int_exp,
               Typ.(hole([]) |> fresh),
               Typ.(Atom(Int) |> fresh),
             )
             |> fresh
           ),
      ),
      // ListLitn requests
      test_matches(
        "ListLit to ListLitn, correct length",
        list(dhexp_typ),
        ListLitn(3),
        ListLit([1, 2, 3] |> List.map(int_exp)) |> fresh,
        [1, 2, 3] |> List.map(int_exp),
      ),
      test_indet_match(
        "Cons to ListLitn, length > cons: indet match",
        list(dhexp_typ),
        ListLitn(3),
        Cons(1 |> int_exp, Cons(2 |> int_exp, hole([]) |> fresh) |> fresh)
        |> fresh,
      ),
      test_does_not_match(
        "Cons to ListLitn, length < cons: does not match",
        list(dhexp_typ),
        ListLitn(1),
        Cons(1 |> int_exp, Cons(2 |> int_exp, hole([]) |> fresh) |> fresh)
        |> fresh,
      ),
      test_indet_match(
        "CastedHole to ListLitn",
        list(dhexp_typ),
        ListLitn(0),
        Cast(
          hole([]) |> fresh,
          Typ.(hole([]) |> fresh),
          Typ.(List(hole([]) |> fresh) |> fresh),
        )
        |> fresh,
      ),
      test_matches(
        "CastedList to ListLitn",
        list(dhexp_typ),
        ListLitn(3),
        Cast(
          ListLit([1, 2, 3] |> List.map(int_exp)) |> fresh,
          Typ.(List(hole([]) |> fresh) |> fresh),
          Typ.(List(hole([]) |> fresh) |> fresh),
        )
        |> fresh,
        [1, 2, 3] |> List.map(int_exp),
      ),
      test_matches(
        "DoubleCastedList to ListLitn",
        list(dhexp_typ),
        ListLitn(3),
        Cast(
          Cast(
            ListLit([1, 2, 3] |> List.map(int_exp)) |> fresh,
            Typ.(List(hole([]) |> fresh) |> fresh),
            Typ.(List(hole([]) |> fresh) |> fresh),
          )
          |> fresh,
          Typ.(List(hole([]) |> fresh) |> fresh),
          Typ.(List(Atom(Int) |> fresh) |> fresh),
        )
        |> fresh,
        [1, 2, 3]
        |> List.map(i =>
             Cast(
               i |> int_exp,
               Typ.(hole([]) |> fresh),
               Typ.(Atom(Int) |> fresh),
             )
             |> fresh
           ),
      ),
      // Cons requests
      test_matches(
        "ListLit to Cons: empty tail",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        ListLit([1] |> List.map(int_exp)) |> fresh,
        (1 |> int_exp, ListLit([]) |> fresh),
      ),
      test_matches(
        "ListLit to Cons: non-empty tail",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        ListLit([1, 2, 3] |> List.map(int_exp)) |> fresh,
        (1 |> int_exp, ListLit([2, 3] |> List.map(int_exp)) |> fresh),
      ),
      test_does_not_match(
        "EmptyList to Cons",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        ListLit([]) |> fresh,
      ),
      test_matches(
        "Cons to Cons",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        Cons(1 |> int_exp, Cons(2 |> int_exp, hole([]) |> fresh) |> fresh)
        |> fresh,
        (1 |> int_exp, Cons(2 |> int_exp, hole([]) |> fresh) |> fresh),
      ),
      test_indet_match(
        "CastedHole to Cons",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        Cast(
          hole([]) |> fresh,
          Typ.(hole([]) |> fresh),
          Typ.(List(hole([]) |> fresh) |> fresh),
        )
        |> fresh,
      ),
      test_matches(
        "CastedCons to Cons",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        Cast(
          Cons(1 |> int_exp, hole([]) |> fresh) |> fresh,
          Typ.(List(hole([]) |> fresh) |> fresh),
          Typ.(List(Atom(Int) |> fresh) |> fresh),
        )
        |> fresh,
        (
          Cast(
            1 |> int_exp,
            Typ.(hole([]) |> fresh),
            Typ.(Atom(Int) |> fresh),
          )
          |> fresh,
          Cast(
            hole([]) |> fresh,
            Typ.(List(hole([]) |> fresh) |> fresh),
            Typ.(List(Atom(Int) |> fresh) |> fresh),
          )
          |> fresh,
        ),
      ),
      test_matches(
        "DoubleCastedCons to Cons",
        pair(dhexp_typ, dhexp_typ),
        Cons,
        Cast(
          Cast(
            Cons(hole([]) |> fresh, hole([]) |> fresh) |> fresh,
            Typ.(List(hole([]) |> fresh) |> fresh),
            Typ.(List(List(hole([]) |> fresh) |> fresh) |> fresh),
          )
          |> fresh,
          Typ.(List(List(hole([]) |> fresh) |> fresh) |> fresh),
          Typ.(List(List(Atom(Int) |> fresh) |> fresh) |> fresh),
        )
        |> fresh,
        (
          Cast(
            Cast(
              hole([]) |> fresh,
              Typ.(hole([]) |> fresh),
              Typ.(List(hole([]) |> fresh) |> fresh),
            )
            |> fresh,
            Typ.(List(hole([]) |> fresh) |> fresh),
            Typ.(List(Atom(Int) |> fresh) |> fresh),
          )
          |> fresh,
          Cast(
            Cast(
              hole([]) |> fresh,
              Typ.(List(hole([]) |> fresh) |> fresh),
              Typ.(List(List(hole([]) |> fresh) |> fresh) |> fresh),
            )
            |> fresh,
            Typ.(List(List(hole([]) |> fresh) |> fresh) |> fresh),
            Typ.(List(List(Atom(Int) |> fresh) |> fresh) |> fresh),
          )
          |> fresh,
        ),
      ),
    ]
  ),
);

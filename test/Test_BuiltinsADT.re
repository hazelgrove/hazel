open Alcotest;
open Language;

let ord_eq_testable: testable(DHExp.t) =
  testable(Fmt.using(DHExp.show, Fmt.string), (a, b) =>
    DHExp.term_of(a) == DHExp.term_of(b)
  );

let invert_ord_tests = {
  let imp = BuiltinsADT.invert_ord.imp;
  [
    test_case("invert_ord name is `invert_ord`", `Quick, () =>
      check(string, "name", "invert_ord", BuiltinsADT.invert_ord.name)
    ),
    test_case("invert_ord(Lt) = Gt", `Quick, () =>
      check(
        option(ord_eq_testable),
        "Lt -> Gt",
        Some(BuiltinsADT.Ord.gt),
        imp(BuiltinsADT.Ord.lt),
      )
    ),
    test_case("invert_ord(Gt) = Lt", `Quick, () =>
      check(
        option(ord_eq_testable),
        "Gt -> Lt",
        Some(BuiltinsADT.Ord.lt),
        imp(BuiltinsADT.Ord.gt),
      )
    ),
    test_case("invert_ord(Eq) = Eq", `Quick, () =>
      check(
        option(ord_eq_testable),
        "Eq -> Eq",
        Some(BuiltinsADT.Ord.eq),
        imp(BuiltinsADT.Ord.eq),
      )
    ),
    test_case("invert_ord(non-Ord constructor) = None", `Quick, () =>
      check(
        bool,
        "None",
        true,
        Option.is_none(imp(IdTagged.FreshGrammar.Exp.int(0))),
      )
    ),
  ];
};

let of_atom_compare_tests = {
  /* of_atom_compare on an Int compare entry */
  let fn =
    BuiltinsADT.of_atom_compare((
      "int_compare",
      Atom.Cmp(Atom.Int, Bigint.compare),
    ));
  let big = i => IdTagged.FreshGrammar.Exp.big_int(Bigint.of_int(i));
  let pair = (a, b) => IdTagged.FreshGrammar.Exp.tuple([big(a), big(b)]);
  [
    test_case("of_atom_compare: preserves the name", `Quick, () =>
      check(string, "name", "int_compare", fn.name)
    ),
    test_case("of_atom_compare: 1 < 2 returns Lt", `Quick, () =>
      check(
        option(ord_eq_testable),
        "Lt",
        Some(BuiltinsADT.Ord.lt),
        fn.imp(pair(1, 2)),
      )
    ),
    test_case("of_atom_compare: 2 == 2 returns Eq", `Quick, () =>
      check(
        option(ord_eq_testable),
        "Eq",
        Some(BuiltinsADT.Ord.eq),
        fn.imp(pair(2, 2)),
      )
    ),
    test_case("of_atom_compare: 3 > 2 returns Gt", `Quick, () =>
      check(
        option(ord_eq_testable),
        "Gt",
        Some(BuiltinsADT.Ord.gt),
        fn.imp(pair(3, 2)),
      )
    ),
    test_case("of_atom_compare: non-matching input returns None", `Quick, () =>
      check(
        bool,
        "None",
        true,
        Option.is_none(fn.imp(IdTagged.FreshGrammar.Exp.int(0))),
      )
    ),
  ];
};

let tests = ("BuiltinsADT", invert_ord_tests @ of_atom_compare_tests);

open Alcotest;
open Language;

let tests = (
  "Atom",
  [
    test_case(
      "conversions_from: Int has one entry per other class",
      `Quick,
      () => {
        let fns = Atom.conversions_from(Atom.Int);
        check(int, "6 conversions", 6, List.length(fns));
        let names = List.map(fst, fns);
        List.iter(
          expected =>
            check(bool, "has " ++ expected, true, List.mem(expected, names)),
          [
            "sint_of_int",
            "nat_of_int",
            "float_of_int",
            "real_of_int",
            "bool_of_int",
            "string_of_int",
          ],
        );
      },
    ),
    test_case(
      "conversions_from: Nat has conversions to all other classes",
      `Quick,
      () => {
        let fns = Atom.conversions_from(Atom.Nat);
        check(int, "6 conversions", 6, List.length(fns));
        let names = List.map(fst, fns);
        check(
          bool,
          "no self-conversion",
          false,
          List.mem("nat_of_nat", names),
        );
      },
    ),
    test_case(
      "compare_builtin: numeric and string classes have builtin compares",
      `Quick,
      () => {
        check(
          option(string),
          "Int",
          Some("int_compare"),
          Atom.compare_builtin(Atom.Int),
        );
        check(
          option(string),
          "SInt",
          Some("sint_compare"),
          Atom.compare_builtin(Atom.SInt),
        );
        check(
          option(string),
          "Nat",
          Some("nat_compare"),
          Atom.compare_builtin(Atom.Nat),
        );
        check(
          option(string),
          "Float",
          Some("float_compare"),
          Atom.compare_builtin(Atom.Float),
        );
        check(
          option(string),
          "String",
          Some("string_compare"),
          Atom.compare_builtin(Atom.String),
        );
      },
    ),
    test_case(
      "compare_builtin: Bool and symbolic Real have no total compare",
      `Quick,
      () => {
        check(
          option(string),
          "Bool",
          None,
          Atom.compare_builtin(Atom.Bool),
        );
        check(
          option(string),
          "Real",
          None,
          Atom.compare_builtin(Atom.Real),
        );
      },
    ),
    test_case(
      "Real rationals normalize and print exactly",
      `Quick,
      () => {
        let one_third = Real.normalize(Bigint.one, Bigint.of_int(3), None);
        let two_sixths =
          Real.normalize(Bigint.of_int(2), Bigint.of_int(6), None);
        check(
          bool,
          "normalized equality",
          true,
          Real.equal(one_third, two_sixths),
        );
        check(
          string,
          "non-terminating rational",
          "1/3",
          Real.to_literal(one_third),
        );
        check(
          string,
          "terminating rational",
          "0.125",
          Real.to_literal(
            Real.normalize(Bigint.one, Bigint.of_int(8), None),
          ),
        );
      },
    ),
    test_case(
      "conversions_from: uses <target>_of_<source> scheme",
      `Quick,
      () => {
        let int_conversions = Atom.conversions_from(Atom.Int);
        check(
          string,
          "int->sint",
          "sint_of_int",
          fst(
            List.find(
              ((_, to_: Atom.cls)) => to_ == Atom.SInt,
              int_conversions,
            ),
          ),
        );
        let nat_conversions = Atom.conversions_from(Atom.Nat);
        check(
          string,
          "nat->string",
          "string_of_nat",
          fst(
            List.find(
              ((_, to_: Atom.cls)) => to_ == Atom.String,
              nat_conversions,
            ),
          ),
        );
      },
    ),
  ],
);

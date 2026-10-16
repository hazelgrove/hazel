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
        check(int, "5 conversions", 5, List.length(fns));
        let names = List.map(fst, fns);
        List.iter(
          expected =>
            check(bool, "has " ++ expected, true, List.mem(expected, names)),
          [
            "sint_of_int",
            "nat_of_int",
            "float_of_int",
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
        check(int, "5 conversions", 5, List.length(fns));
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
      "compare_module: numeric and string classes have a compare",
      `Quick,
      () => {
        check(
          option(string),
          "Int",
          Some("Int"),
          Atom.compare_module(Atom.Int),
        );
        check(
          option(string),
          "SInt",
          Some("SInt"),
          Atom.compare_module(Atom.SInt),
        );
        check(
          option(string),
          "Nat",
          Some("Nat"),
          Atom.compare_module(Atom.Nat),
        );
        check(
          option(string),
          "Float",
          Some("Float"),
          Atom.compare_module(Atom.Float),
        );
        check(
          option(string),
          "String",
          Some("String"),
          Atom.compare_module(Atom.String),
        );
      },
    ),
    test_case("compare_module: Bool has no compare", `Quick, () =>
      check(option(string), "Bool", None, Atom.compare_module(Atom.Bool))
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

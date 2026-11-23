open Alcotest;
open Language;
open Poly;

let tests = (
  "Atom",
  [
    test_case("A float prints as a decimal that reads back as it", `Quick, () =>
      List.iter(
        [0.0000172, 1.72128496811e-05, 1e20, 1.0 /. 3.0, 0.1, 100.0, (-2.5)],
        ~f=f => {
          let text = Atom.to_literal(Float(f));
          check(bool, text, true, Float.equal(float_of_string(text), f));
          check(
            bool,
            text ++ " is a decimal",
            true,
            String.contains(text, '.') && !String.contains(text, 'e'),
          );
        },
      )
    ),
    test_case(
      "conversions_from: Int has one entry per other class",
      `Quick,
      () => {
        let fns = Atom.conversions_from(Atom.Int);
        check(int, "5 conversions", 5, List.length(fns));
        let names = List.map(~f=fst, fns);
        List.iter(
          ~f=
            expected =>
              check(
                bool,
                "has " ++ expected,
                true,
                List.mem(names, expected, ~equal=Poly.equal),
              ),
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
        let names = List.map(~f=fst, fns);
        check(
          bool,
          "no self-conversion",
          false,
          List.mem(names, "nat_of_nat", ~equal=Poly.equal),
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
    test_case("compare_builtin: Bool has no builtin compare", `Quick, () =>
      check(option(string), "Bool", None, Atom.compare_builtin(Atom.Bool))
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
            List.find_exn(int_conversions, ~f=((_, to_: Atom.cls)) =>
              Atom.equal_cls(to_, Atom.SInt)
            ),
          ),
        );
        let nat_conversions = Atom.conversions_from(Atom.Nat);
        check(
          string,
          "nat->string",
          "string_of_nat",
          fst(
            List.find_exn(nat_conversions, ~f=((_, to_: Atom.cls)) =>
              to_ == Atom.String
            ),
          ),
        );
      },
    ),
  ],
);

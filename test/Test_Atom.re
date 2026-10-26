open Alcotest;
open Language;

let rat = (n: int, d: int): Atom.t =>
  switch (Util.Rational.make(Bigint.of_int(n), Bigint.of_int(d))) {
  | Some(r) => Rat(r)
  | None => Alcotest.fail("zero denominator")
  };

let atom = testable(Atom.pp, Atom.equal);

let rejects = (msg, f) =>
  check(
    bool,
    msg,
    true,
    switch (f()) {
    | _ => false
    | exception _ => true
    },
  );

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
    test_case(
      "to_literal: Rat displays as a reduced fraction",
      `Quick,
      () => {
        check(string, "1/3", "1/3", Atom.to_literal(rat(1, 3)));
        check(string, "2/-6", "-1/3", Atom.to_literal(rat(2, -6)));
        check(string, "4/2", "2/1", Atom.to_literal(rat(4, 2)));
        check(string, "0/5", "0/1", Atom.to_literal(rat(0, 5)));
      },
    ),
    test_case(
      "Rat: public builtins are the constructor and operator parity",
      `Quick,
      () => {
        check(
          int,
          "no conversions from Rat",
          0,
          List.length(Atom.conversions_from(Rat)),
        );
        List.iter(
          cls =>
            check(
              bool,
              "no conversion to Rat from " ++ Atom.show_cls(cls),
              false,
              List.exists(
                ((_, to_: Atom.cls)) => to_ == Rat,
                Atom.conversions_from(cls),
              ),
            ),
          Atom.all_of_cls,
        );
        check(
          option(string),
          "no compare",
          None,
          Atom.compare_builtin(Rat),
        );
        let names = List.map(BuiltinsUtil.name_of_builtin, Builtins.builtins);
        check(bool, "rat exists", true, List.mem("rat", names));
        /* Operator builtins exist for Rat as for Int/SInt/Nat (D7 parity);
           conversions and compare stay suppressed. */
        let mentions_rat = name =>
          String.starts_with(~prefix="rat_", name)
          || String.ends_with(~suffix="_rat", name)
          || String.ends_with(~suffix="_of_rat", name);
        check(
          list(string),
          "only operator rat_* names",
          [
            "rat_divide",
            "rat_gt",
            "rat_gte",
            "rat_lt",
            "rat_lte",
            "rat_minus",
            "rat_plus",
            "rat_power",
            "rat_times",
          ],
          List.filter(mentions_rat, names) |> List.sort(String.compare),
        );
      },
    ),
    test_case(
      "Rat: type token",
      `Quick,
      () => {
        /* 7a40 spells type tokens at each site (no BaseAtom); the parse
           side is checked in Test_RationalReadback, for both parsers */
        check(
          string,
          "token",
          "Rational",
          Typ.pretty_print(Typ.fresh_atom(Rat)),
        );
        check(
          bool,
          "reserved base type name",
          true,
          Ctx.is_base_typ("Rational"),
        );
      },
    ),
    test_case(
      "Rat: yojson round trip",
      `Quick,
      () => {
        List.iter(
          a =>
            check(
              atom,
              Atom.show(a),
              a,
              a |> Atom.yojson_of_t |> Atom.t_of_yojson,
            ),
          [rat(1, 3), rat(-1, 3), rat(0, 1), rat(7, 1)],
        );
        check(
          string,
          "encoding",
          {|["Rat",["-1","3"]]|},
          rat(2, -6) |> Atom.yojson_of_t |> Yojson.Safe.to_string,
        );
        let big =
          switch (
            Util.Rational.make(
              Bigint.of_string("36893488147419103232"),
              Bigint.of_string("6"),
            )
          ) {
          | Some(r) => Atom.Rat(r)
          | None => Alcotest.fail("zero denominator")
          };
        check(
          atom,
          "beyond 2^53",
          big,
          big |> Atom.yojson_of_t |> Atom.t_of_yojson,
        );
        check(
          string,
          "beyond 2^53 encoding",
          {|["Rat",["18446744073709551616","3"]]|},
          big |> Atom.yojson_of_t |> Yojson.Safe.to_string,
        );
        check(
          bool,
          "cls",
          true,
          Atom.equal_cls(
            Rat,
            Rat |> Atom.yojson_of_cls |> Atom.cls_of_yojson,
          ),
        );
      },
    ),
    test_case(
      "Rat: yojson decoding validates",
      `Quick,
      () => {
        let decode = (s, ()) =>
          s |> Yojson.Safe.from_string |> Atom.t_of_yojson;
        rejects("zero denominator", decode({|["Rat",["1","0"]]|}));
        rejects("not reduced", decode({|["Rat",["2","6"]]|}));
        rejects("negative denominator", decode({|["Rat",["1","-3"]]|}));
        rejects("non-canonical zero", decode({|["Rat",["0","5"]]|}));
        rejects("not a pair", decode({|["Rat",["1"]]|}));
        check(
          atom,
          "canonical",
          rat(1, 3),
          decode({|["Rat",["1","3"]]|}, ()),
        );
      },
    ),
    test_case(
      "Rat: sexp round trip and validation",
      `Quick,
      () => {
        List.iter(
          a =>
            check(
              atom,
              Atom.show(a),
              a,
              a |> Atom.sexp_of_t |> Atom.t_of_sexp,
            ),
          [rat(1, 3), rat(-1, 3), rat(0, 1)],
        );
        let decode = (s, ()) => s |> Sexplib.Sexp.of_string |> Atom.t_of_sexp;
        rejects("zero denominator", decode("(Rat (1 0))"));
        rejects("not reduced", decode("(Rat (2 6))"));
        check(atom, "canonical", rat(-1, 3), decode("(Rat (-1 3))", ()));
      },
    ),
    test_case(
      "Rat: Typ yojson round trip",
      `Quick,
      () => {
        let t = IdTagged.FreshGrammar.Typ.rational();
        let t' = t |> Typ.yojson_of_t |> Typ.t_of_yojson;
        check(bool, "Rational type", true, Typ.term_of(t') == Atom(Rat));
      },
    ),
  ],
);

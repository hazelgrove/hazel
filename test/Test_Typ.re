open Alcotest;
open Language;
let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

let typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

let join_tests = (
  "Typ.join",
  IdTagged.FreshGrammar.Typ.[
    test_case(
      "Typ join on polymorphic types",
      `Quick,
      () => {
        let t =
          Typ.join(
            Builtins.ctx_init(Some(Int)),
            Forall(Var("a") |> TPat.temp, Var("a") |> Typ.temp) |> Typ.temp,
            Forall(Var("b") |> TPat.temp, Var("b") |> Typ.temp) |> Typ.temp,
          );
        check(
          option(testable_typ),
          "Forall alpha equivalent",
          Some(
            Forall(Var("a") |> TPat.temp, Var("a") |> Typ.temp) |> Typ.temp,
          ),
          t,
        );
      },
    ),
    test_case(
      "Typ join on product projection with fully known types",
      `Quick,
      () => {
        let t =
          Typ.join(
            Builtins.ctx_init(None),
            int(),
            prod_projection(
              prod([
                tup_label(label("a"), int()),
                tup_label(label("b"), bool()),
              ]),
              label("a"),
            ),
          );
        check(option(typ), "Joined product projections", Some(int()), t);
      },
    ),
    test_case(
      "Typ join on product projection with unknown types",
      `Quick,
      () => {
        let t =
          Typ.join(
            Builtins.ctx_init(None),
            int(),
            prod_projection(unknown(Internal), label("a")),
          );
        check(
          option(typ),
          "Joined product projections with unknown",
          Some(int()),
          t,
        );
      },
    ),
    test_case(
      "Typ join on product projection with unknown label",
      `Quick,
      () => {
        let t =
          Typ.join(
            Builtins.ctx_init(None),
            int(),
            prod_projection(
              prod([
                tup_label(label("a"), int()),
                tup_label(label("b"), bool()),
              ]),
              unknown(Internal),
            ),
          );
        check(
          option(typ),
          "Joined product projections with unknown label",
          Some(int()),
          t,
        );
      },
    ),
    test_case(
      "Typ join on product extension with fully known extension types",
      `Quick,
      () => {
        let t =
          Typ.join(
            Builtins.ctx_init(None),
            prod_extension(
              prod([
                tup_label(label("a"), int()),
                bool(),
                tup_label(label("b"), float()),
              ]),
              prod([
                tup_label(label("c"), string()),
                tup_label(label("b"), bool()),
                nat(),
              ]),
            ),
            prod([
              tup_label(unknown(Internal), int()),
              unknown(Internal),
              tup_label(label("b"), unknown(Internal)),
              unknown(Internal),
              nat(),
            ]),
          );
        check(
          option(typ),
          "Joined product extensions",
          Some(
            prod([
              tup_label(label("a"), int()),
              bool(),
              tup_label(label("b"), bool()),
              tup_label(label("c"), string()),
              nat(),
            ]),
          ),
          t,
        );
      },
    ),
    test_case(
      "Typ join on two product extensions with known extension types",
      `Quick,
      () => {
        let t =
          Typ.join(
            Builtins.ctx_init(None),
            prod_extension(
              prod([
                tup_label(label("a"), int()),
                tup_label(label("b"), bool()),
              ]),
              prod([string(), tup_label(label("b"), float())]),
            ),
            prod_extension(
              prod([tup_label(label("a"), int())]),
              prod([tup_label(label("b"), float()), string()]),
            ),
          );
        check(
          option(typ),
          "Joined product extensions",
          Some(
            prod([
              tup_label(label("a"), int()),
              tup_label(label("b"), float()),
              string(),
            ]),
          ),
          t,
        );
      },
    ),
  ],
);

let fast_equal_tests = (
  "Typ.fast_equal",
  [
    test_case(
      "Equality alpha equivalent",
      `Quick,
      () => {
        check(
          bool,
          "Forall alpha equivalent",
          true,
          Typ.fast_equal(
            Forall(Var("a") |> TPat.temp, Var("a") |> Typ.temp) |> Typ.temp,
            Forall(Var("b") |> TPat.temp, Var("b") |> Typ.temp) |> Typ.temp,
          ),
        );
        check(
          bool,
          "Forall non alpha equivalent",
          false,
          Equality.syntactic.typ(
            Forall(Var("a") |> TPat.temp, Var("a") |> Typ.temp) |> Typ.temp,
            Forall(Var("b") |> TPat.temp, Var("b") |> Typ.temp) |> Typ.temp,
          ),
        );
      },
    ),
    test_case(
      "consistent_join on equivalent atomic types",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Typ;
        let t3 =
          Typ.consistent_join(
            Builtins.ctx_init(Some(Int)),
            [string(), string()],
          );
        check(
          testable_typ,
          "consistent_join on equivalent atomic types",
          string(),
          t3,
        );
      },
    ),
    test_case(
      "consistent_join on inconsistent atomic types",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Typ;
        let t3 =
          Typ.consistent_join(
            Builtins.ctx_init(Some(Int)),
            [string(), int()],
          );
        check(
          testable_typ,
          "consistent_join on inconsistent atomic types",
          unknown(Internal),
          t3,
        );
      },
    ),
    test_case(
      "consistent_join on lists of inconsistent atomic types",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Typ;
        let t3 =
          Typ.consistent_join(
            Builtins.ctx_init(Some(Int)),
            [list(string()), list(int())],
          );
        check(
          testable_typ,
          "consistent_join on equivalent function types",
          list(unknown(Internal)),
          t3,
        );
      },
    ),
    test_case(
      "consistent_join on arrow types with inconsistent parts",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Typ;
        let t3 =
          Typ.consistent_join(
            Builtins.ctx_init(Some(Int)),
            [arrow(string(), list(int())), arrow(int(), list(string()))],
          );
        check(
          testable_typ,
          "consistent_join on arrow types with inconsistent parts",
          arrow(unknown(Inconsistent), list(unknown(Inconsistent))),
          t3,
        );
      },
    ),
    test_case(
      "Consistent join collapses unknowns",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Typ;
        let t3 =
          Typ.consistent_join(
            Builtins.ctx_init(Some(Int)),
            [unknown(Hole(EmptyHole)), int()],
          );
        check(testable_typ, "Consistent join collapses unknowns", int(), t3);
      },
    ),
    test_case(
      "Consistent join does not collapse inconsistent unknowns",
      `Quick,
      () => {
        open IdTagged.FreshGrammar.Typ;
        let t3 =
          Typ.consistent_join(
            Builtins.ctx_init(Some(Int)),
            [int(), string(), float()],
          );
        check(
          testable_typ,
          "Consistent join does not collapse inconsistent unknowns",
          unknown(Inconsistent),
          t3,
        );
      },
    ),
  ],
);

let diff_tests = (
  "Typ.diff",
  [
    QCheck_alcotest.to_alcotest(
      QCheck.Test.make(
        ~name="diff identity",
        ~count=1000,
        QCheck_Util.arb_typ(~minimal_idents=true, 7),
        typ =>
        Typ.diff(typ, typ) == []
      ),
    ),
    test_case(
      "diff root different atom types",
      `Quick,
      () => {
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let float_typ = Typ.fresh(Atom(Atom.Float));
        let expected = [Typ.rep_id(float_typ)];
        check(
          testable(
            Fmt.using(
              ids => String.concat(", ", List.map(Id.show, ids)),
              Fmt.string,
            ),
            (==),
          ),
          "diff on different atom types",
          expected,
          Typ.diff(int_typ, float_typ),
        );
      },
    ),
    test_case(
      "diff arrow different codomain",
      `Quick,
      () => {
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let float_typ = Typ.fresh(Atom(Atom.Float));
        let arrow1 = Typ.fresh(Arrow(int_typ, int_typ));
        let arrow2 = Typ.fresh(Arrow(int_typ, float_typ));
        let expected = [Typ.rep_id(float_typ)];
        check(
          testable(
            Fmt.using(
              ids => String.concat(", ", List.map(Id.show, ids)),
              Fmt.string,
            ),
            (==),
          ),
          "diff on arrows with different codomains",
          expected,
          Typ.diff(arrow1, arrow2),
        );
      },
    ),
    test_case(
      "diff list different element",
      `Quick,
      () => {
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let float_typ = Typ.fresh(Atom(Atom.Float));
        let list1 = Typ.fresh(List(int_typ));
        let list2 = Typ.fresh(List(float_typ));
        let expected = [Typ.rep_id(float_typ)];
        check(
          testable(
            Fmt.using(
              ids => String.concat(", ", List.map(Id.show, ids)),
              Fmt.string,
            ),
            (==),
          ),
          "diff on lists with different elements",
          expected,
          Typ.diff(list1, list2),
        );
      },
    ),
    test_case(
      "diff arrow different domain",
      `Quick,
      () => {
        let int_typ = Typ.fresh(Atom(Atom.Int));
        let float_typ = Typ.fresh(Atom(Atom.Float));
        let string_typ = Typ.fresh(Atom(Atom.String));
        let arrow1 = Typ.fresh(Arrow(int_typ, string_typ));
        let arrow2 = Typ.fresh(Arrow(float_typ, string_typ));
        let expected = [Typ.rep_id(float_typ)];
        check(
          testable(
            Fmt.using(
              ids => String.concat(", ", List.map(Id.show, ids)),
              Fmt.string,
            ),
            (==),
          ),
          "diff on arrows with different domains",
          expected,
          Typ.diff(arrow1, arrow2),
        );
      },
    ),
    test_case(
      "diff var different names",
      `Quick,
      () => {
        let var1 = Typ.fresh(Var("x"));
        let var2 = Typ.fresh(Var("y"));
        let expected = [Typ.rep_id(var2)];
        check(
          testable(
            Fmt.using(
              ids => String.concat(", ", List.map(Id.show, ids)),
              Fmt.string,
            ),
            (==),
          ),
          "diff on vars with different names",
          expected,
          Typ.diff(var1, var2),
        );
      },
    ),
    QCheck_alcotest.to_alcotest(
      QCheck.Test.make(
        ~name="Same type has no diff",
        ~count=1000,
        QCheck_Util.arb_typ(~minimal_idents=false, 30),
        typ => {
          Typ.diff(typ, typ) == []
        }
      ),
    ),
  ],
);

let tests = [join_tests, fast_equal_tests, diff_tests];

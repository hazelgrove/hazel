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

// TODO We want this property but it's not currently passing for forall and rec types so it's not included below
let meet_precision_property =
  QCheck_alcotest.to_alcotest(
    QCheck.Test.make(
      ~name="Typ.meet is less precise than inputs",
      ~count=100000,
      QCheck.(
        QCheck_Util.(
          pair(
            arb_typ(~minimal_idents=true, 10),
            arb_typ(~minimal_idents=true, 10),
          )
        )
      ),
      ((t1, t2)) => {
        let ctx = Builtins.ctx_init(Some(Int));
        let m = Typ.meet(ctx, t1, t2);
        Typ.is_more_precise(ctx, Typ.normalize(ctx, t1), m)
        && Typ.is_more_precise(ctx, Typ.normalize(ctx, t2), m);
      },
    ),
  );

let meet_tests = (
  "Typ.meet",
  IdTagged.FreshGrammar.Typ.[
    test_case(
      "equal atomic types",
      `Quick,
      () => {
        let t = Typ.meet(Builtins.ctx_init(None), int(), int());
        check(typ, "Meet of equal atomic types", int(), t);
      },
    ),
    test_case(
      "Unknown and atomic type",
      `Quick,
      () => {
        let t = Typ.meet(Builtins.ctx_init(None), unknown(Internal), int());
        check(typ, "Meet of Unknown and atomic type", unknown(Internal), t);
      },
    ),
    test_case(
      "Sum type with same variants",
      `Quick,
      () => {
        let t =
          Typ.meet(
            Builtins.ctx_init(None),
            sum([
              Variant("A", [], Some(int())),
              Variant("B", [], Some(bool())),
            ]),
            sum([
              Variant("A", [], Some(int())),
              Variant("B", [], Some(bool())),
            ]),
          );
        check(
          typ,
          "Meet of sum types with same variants",
          sum([
            Variant("A", [], Some(int())),
            Variant("B", [], Some(bool())),
          ]),
          t,
        );
      },
    ),
    test_case(
      "Unbound variables",
      `Quick,
      () => {
        let t = Typ.meet(Builtins.ctx_init(None), var("a"), var("b"));
        check(typ, "Meet of unbound variables", unknown(Internal), t);
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

let tests = [join_tests, meet_tests, fast_equal_tests];

open Alcotest;
open Language;
let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

let tests = (
  "Typ",
  [
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
      "Equality alpha equivalent",
      `Quick,
      () => {
        check(
          bool,
          "Forall alpha equivalent",
          true,
          Typ.fast_equal(
            ~alpha_equivalence=true,
            Forall(Var("a") |> TPat.temp, Var("a") |> Typ.temp) |> Typ.temp,
            Forall(Var("b") |> TPat.temp, Var("b") |> Typ.temp) |> Typ.temp,
          ),
        );
        check(
          bool,
          "Forall non alpha equivalent",
          false,
          Typ.fast_equal(
            ~alpha_equivalence=false,
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

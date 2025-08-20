open Alcotest;
open Language;

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
            Forall(Var("a") |> TPat.temp, Var("a") |> Typ.temp_empty)
            |> Typ.temp_empty,
            Forall(Var("b") |> TPat.temp, Var("b") |> Typ.temp_empty)
            |> Typ.temp_empty,
          );
        check(
          option(testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal)),
          "Forall alpha equivalent",
          Some(
            Forall(Var("a") |> TPat.temp, Var("a") |> Typ.temp_empty)
            |> Typ.temp_empty,
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
            Forall(Var("a") |> TPat.temp, Var("a") |> Typ.temp_empty)
            |> Typ.temp_empty,
            Forall(Var("b") |> TPat.temp, Var("b") |> Typ.temp_empty)
            |> Typ.temp_empty,
          ),
        );
        check(
          bool,
          "Forall non alpha equivalent",
          false,
          Typ.fast_equal(
            ~alpha_equivalence=false,
            Forall(Var("a") |> TPat.temp, Var("a") |> Typ.temp_empty)
            |> Typ.temp_empty,
            Forall(Var("b") |> TPat.temp, Var("b") |> Typ.temp_empty)
            |> Typ.temp_empty,
          ),
        );
      },
    ),
  ],
);

/**
 * This file contains tests to validate the `MakeTerm` module's ability to convert
 * zippers into expressions.
 */
open Alcotest;
open Haz3lcore;

let exp_typ = testable(Fmt.using(Exp.show, Fmt.string), Exp.fast_equal);

let parse_exp = (s: string) =>
  MakeTerm.from_zip_for_sem(Option.get(Printer.zipper_of_string(s))).term;
let exp_check = (expected, actual) =>
  check(exp_typ, actual, expected, parse_exp(actual));

let tests = (
  "MakeTerm",
  [
    test_case("Integer Literal", `Quick, () => {
      exp_check(Int(0) |> Exp.fresh, "0")
    }),
    test_case("Empty Hole", `Quick, () => {
      exp_check(EmptyHole |> Exp.fresh, "?")
    }),
    test_case("Free Variable", `Quick, () => {
      exp_check(Var("x") |> Exp.fresh, "x")
    }),
    test_case("Parenthesized Expression", `Quick, () => {
      exp_check(Parens(Int(0) |> Exp.fresh) |> Exp.fresh, "(0)")
    }),
    test_case("Let Expression", `Quick, () => {
      exp_check(
        Let(
          Var("x") |> Pat.fresh,
          Int(1) |> Exp.fresh,
          Var("x") |> Exp.fresh,
        )
        |> Exp.fresh,
        "let x = 1 in x",
      )
    }),
    test_case("Function Application", `Quick, () => {
      exp_check(
        Ap(Forward, Var("f") |> Exp.fresh, Var("x") |> Exp.fresh)
        |> Exp.fresh,
        "f(x)",
      )
    }),
    test_case("Named Function Definition", `Quick, () => {
      exp_check(
        Let(
          Var("f") |> Pat.fresh,
          Fun(Var("x") |> Pat.fresh, Var("x") |> Exp.fresh, None)  // It seems as though the function naming happens during elaboration and not during parsing
          |> Exp.fresh,
          Int(1) |> Exp.fresh,
        )
        |> Exp.fresh,
        "let f = fun x -> x in 1",
      )
    }),
    test_case("Incomplete Function Definition", `Quick, () => {
      exp_check(
        Let(
          EmptyHole |> Pat.fresh,
          Fun(Var("x") |> Pat.fresh, EmptyHole |> Exp.fresh, None)
          |> Exp.fresh,
          EmptyHole |> Exp.fresh,
        )
        |> Exp.fresh,
        "let    = fun x ->   in  ",
      )
    }),
    test_case("Constructor", `Quick, () => {
      exp_check(
        Constructor("A", Unknown(Internal) |> Typ.fresh) |> Exp.fresh,
        "A",
      )
    }),
    test_case("Type Alias", `Quick, () => {
      exp_check(
        TyAlias(
          Var("x") |> TPat.fresh,
          Int |> Typ.fresh,
          Int(1) |> Exp.fresh,
        )
        |> Exp.fresh,
        "type x = Int in 1",
      )
    }),
  ],
);

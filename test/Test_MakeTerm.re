/**
 * This file contains tests to validate the `MakeTerm` module's ability to convert
 * zippers into expressions.
 */
open Alcotest;
open Haz3lcore;

let exp_typ = testable(Fmt.using(Exp.show, Fmt.string), Exp.fast_equal);

// TODO Assertion if it doesn't parse
let parse_exp = (s: string) =>
  MakeTerm.from_zip_for_sem(Option.get(Printer.zipper_of_string(s))).term;
let exp_check = (expected, actual) =>
  check(exp_typ, actual, expected, parse_exp(actual));

let tests = [
  test_case("Singleton Labled Tuple ascription in let", `Quick, () => {
    exp_check(
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Parens(
            Prod([
              TupLabel(Label("l") |> Typ.fresh, String |> Typ.fresh)
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Parens(String("a") |> Exp.fresh) |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
      "let x : (l=String) = (\"a\") in x",
    )
  }),
  test_case("Assigning labeled tuple to variable", `Quick, () => {
    exp_check(
      Let(
        Var("x") |> Pat.fresh,
        Parens(
          Tuple([
            TupLabel(Label("l") |> Exp.fresh, Int(32) |> Exp.fresh)
            |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Let(
          Cast(
            Var("y") |> Pat.fresh,
            Parens(
              Prod([
                TupLabel(Label("l") |> Typ.fresh, Int |> Typ.fresh)
                |> Typ.fresh,
              ])
              |> Typ.fresh,
            )
            |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Pat.fresh,
          Var("x") |> Exp.fresh,
          Var("y") |> Exp.fresh,
        )
        |> Exp.fresh,
      )
      |> Exp.fresh,
      "let x = (l=32) in
       let y : (l=Int) = x in y",
    )
  }),
  test_case("Multiple labels in tuple", `Quick, () =>
    exp_check(
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Parens(
            Prod([
              TupLabel(Label("l") |> Typ.fresh, Int |> Typ.fresh)
              |> Typ.fresh,
              TupLabel(Label("l2") |> Typ.fresh, String |> Typ.fresh)
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Parens(
          Tuple([
            TupLabel(Label("l") |> Exp.fresh, Int(32) |> Exp.fresh)
            |> Exp.fresh,
            TupLabel(Label("l2") |> Exp.fresh, String("") |> Exp.fresh)
            |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
      {|let x : (l=Int, l2=String) = (l=32, l2="") in x|},
    )
  ),
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
      Ap(Forward, Var("f") |> Exp.fresh, Var("x") |> Exp.fresh) |> Exp.fresh,
      "f(x)",
    )
  }),
  test_case("Named Function Definition", `Quick, () => {
    exp_check(
      Let(
        Var("f") |> Pat.fresh,
        Fun(Var("x") |> Pat.fresh, Var("x") |> Exp.fresh, None, None)  // It seems as though the function naming happens during elaboration and not during parsing
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
        Fun(Var("x") |> Pat.fresh, EmptyHole |> Exp.fresh, None, None)
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
      TyAlias(Var("x") |> TPat.fresh, Int |> Typ.fresh, Int(1) |> Exp.fresh)
      |> Exp.fresh,
      "type x = Int in 1",
    )
  }),
];

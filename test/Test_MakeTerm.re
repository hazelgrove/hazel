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
];

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
            TupLabel(Label("l") |> Typ.fresh, String |> Typ.fresh)  // TODO Do we want to wrap the singleton case in a product
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Parens(String("a") |> Exp.fresh) |> Exp.fresh, // TODO Should we require parens around singleton tables to ascribe labels
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
      "let x : (l=String) = \"a\" in x",
    )
  }),
  test_case("", `Quick, () => {
    exp_check(
      Int(7) |> Exp.fresh,
      "let x = (l=32) in
let y : (l=Int) = x in
 ",
    )
  }),
];

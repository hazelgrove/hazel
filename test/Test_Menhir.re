open Haz3lmenhir;
open Alcotest;

let testable_ast = testable(Fmt.using(AST.show_exp, Fmt.string), (==));

let parser_test = (a, expected, actual, ()) =>
  Alcotest.check(
    testable_ast,
    a,
    expected,
    Haz3lmenhir.Interface.parse_program(actual),
  );

let fun_exp = AST.Fun(VarPat("x"), Var("x"), None);

let fun_str = "fun x -> x";

let tests = [
  test_case(
    "Integer Literal",
    `Quick,
    parser_test("Same Integer", Int(8), "8"),
  ),
  test_case("Fun", `Quick, parser_test("Fun", fun_exp, fun_str)),
  test_case(
    "String Literal",
    `Quick,
    parser_test("Same String", String("Hello World"), "\"Hello World\""),
  ),
];

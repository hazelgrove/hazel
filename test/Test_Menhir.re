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

let identity =
  QCheck.Test.make(
    ~name="Identity",
    ~count=10,
    AST.arb_exp_sized(1),
    s => {
      print_endline(AST.show_exp(s));
      s == s;
    },
  );

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
  QCheck_alcotest.to_alcotest(identity),
];

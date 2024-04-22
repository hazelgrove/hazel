open Hazel_menhir;
// open Haz3lcore.DHExp;
open Alcotest;

let test_file = "./test/test.hazel";

let read_whole_file = (filename): string => {
  let ch = open_in_bin(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s;
};

let file_contents = read_whole_file(test_file);

// print_endline(AST.show_exp(Hazel_menhir.Interface.parse_program(file_contents)));

// let prog: AST.exp = Hazel_menhir.Interface.parse_program(file_contents);

// let dhexp = of_menhir_ast(prog);
// print_endline(show(dhexp));

// let pp_string ppf x = Fmt.pf ppf "%S" x in
//   testable pp_string ( = )

let testable_ast = testable(Fmt.using(AST.show_exp, Fmt.string), (==));

let parser_test = (a, expected, actual, ()) =>
  Alcotest.check(
    testable_ast,
    a,
    expected,
    Hazel_menhir.Interface.parse_program(actual),
  );

// let bigger_expression =
//   AST.Let(
//     TypeAnn(
//       VarPat("intsOneElseAdd"),
//       ArrowType(TupleType([TupleType([IntType, IntType])]), IntType),
//     ),
//     Fun(
//       TuplePat([VarPat("x"), VarPat("y")]),
//       CaseExp(
//         TupleExp([Var("x"), Var("y")]),
//         [
//           (TuplePat([IntPat(1), IntPat(1)]), Int(1)),
//           (VarPat("_"), BinExp(Var("x"), IntOp(Plus), Var("y"))),
//         ],
//       ),
//     ),
//     Let(
//       TypeAnn(
//         VarPat("ap_and_inc"),
//         ArrowType(
//           TupleType([
//             TupleType([ArrowType(TupleType([IntType, IntType]), IntType)]),
//             IntType,
//           ]),
//           IntType,
//         ),
//       ),
//       Fun(
//         TuplePat([VarPat("f"), VarPat("x")]),
//         BinExp(
//           ApExp(Var("f"), TupleExp([Var("x"), Int(2)])),
//           IntOp(Plus),
//           Int(1),
//         ),
//       ),
//       ApExp(Var("ap_and_inc"), TupleExp([Var("intsOneElseAdd"), Int(1)])),
//     ),
// );

let fun_exp = AST.Fun(IntType, VarPat("x"), Var("x"), None);

let fun_str = "fun: Int x -> x";

let tests = [
  test_case(
    "Integer Literal",
    `Quick,
    parser_test("Same Integer", Int(8), "8"),
  ),
  test_case("Fun", `Quick, parser_test("Fun", fun_exp, fun_str)),
  // test_case(
  //   "String Literal",
  //   `Quick,
  //   parser_test("Same String", String("Hello World"), "\"Hello World\""),
  // ),
  // test_case(
  //   "Bigger expression",
  //   `Quick,
  //   parser_test("test.hazel contents", bigger_expression, file_contents),
  // ),
];

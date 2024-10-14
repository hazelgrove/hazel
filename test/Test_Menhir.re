open Haz3lmenhir;
open Alcotest;
open Haz3lcore;

let testable_ast = testable(Fmt.using(AST.show_exp, Fmt.string), (==));

let exp_typ =
  testable(
    Fmt.using(Haz3lcore.Exp.show, Fmt.string),
    Haz3lcore.DHExp.fast_equal,
  );

let alco_check = exp_typ |> Alcotest.check;

let mk_map = Statics.mk(CoreSettings.on, Builtins.ctx_init);
let dhexp_of_uexp = u => Elaborator.elaborate(mk_map(u), u) |> fst;

let parser_test = (name: string, exp: Term.Exp.t, menhir: string, ()) =>
  alco_check(
    name,
    exp,
    dhexp_of_uexp(
      Haz3lmenhir.Conversion.Exp.of_menhir_ast(
        Haz3lmenhir.Interface.parse_program(menhir),
      ),
    ),
  );

let fun_exp: Exp.t =
  Fun(Var("x") |> Pat.fresh, Var("x") |> Exp.fresh, None, None) |> Exp.fresh;
let fun_str = "fun x -> x";

let tests = [
  test_case(
    "Integer Literal",
    `Quick,
    parser_test("Same Integer", Int(8) |> Exp.fresh, "8"),
  ),
  test_case("Fun", `Quick, parser_test("Fun", fun_exp, fun_str)),
  test_case(
    "String Literal",
    `Quick,
    parser_test(
      "Same String",
      String("Hello World") |> Exp.fresh,
      "\"Hello World\"",
    ),
  ),
  //NOTE: leaving out deferrals for now due to bug
  // test_case(
  //   "Deferred Ap",
  //   `Quick,
  //   parser_test("Deferred Ap", , "x(_)"),
  // ),
];

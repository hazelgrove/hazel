open Haz3lmenhir;
open Alcotest;
open Haz3lcore;

let testable_ast = testable(Fmt.using(AST.show_exp, Fmt.string), (==));

let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);

let exp_typ =
  testable(
    Fmt.using(Haz3lcore.Exp.show, Fmt.string),
    Haz3lcore.DHExp.fast_equal,
  );

let alco_check = exp_typ |> Alcotest.check;

let mk_map = Statics.mk(CoreSettings.on, Builtins.ctx_init);
let dhexp_of_uexp = u => Elaborator.elaborate(mk_map(u), u) |> fst;

//exp = expected, menhir = test
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

//Test for a let function
let let_fun_uexp: Exp.t =
  Let(
    Var("f") |> Pat.fresh,
    Fun(Var("x") |> Pat.fresh, Int(1) |> Exp.fresh, None, None) |> Exp.fresh,
    Int(55) |> Exp.fresh,
  )
  |> Exp.fresh;

let let_fun_str = "
let f =
    fun x ->
        1
    f
    in
55";

let let_fun_test = () =>
  parser_test(
    "Let expression for a function which is not recursive (menhir)",
    let_fun_uexp,
    let_fun_str,
  );

//Test for an empty hole
let empty_hole_str = "?";
let empty_hole_uexp: Exp.t = {
  ids: [id_at(0)],
  term: EmptyHole,
  copied: false,
};
let empty_hole_test = () =>
  parser_test("Empty hole (menhir)", empty_hole_uexp, empty_hole_str);

//Test for a free variable
let free_var_uexp: Exp.t = {
  ids: [id_at(0)],
  term: Parens({ids: [id_at(1)], term: Var("y"), copied: false}),
  copied: false,
};
let free_var_test = () =>
  parser_test(
    "Nonempty hole with free variable (menhir)",
    dhexp_of_uexp(free_var_uexp),
    "y",
  );

//Menhir test for a binary operation
let bin_op_uexp: Exp.t =
  BinOp(Int(Plus), Int(1) |> Exp.fresh, Int(2) |> Exp.fresh) |> Exp.fresh;

let bin_op_str = "1 + 2";

let bin_op_test = () =>
  parser_test(
    "binary integer operation (plus)",
    dhexp_of_uexp(bin_op_uexp),
    bin_op_str,
  );

//Inconsistent branches menhir test
let case_menhir_str = "
    case 4 == 3
    | true => 24
    | false => 32
end
";
let case_uexp: Exp.t =
  Match(
    BinOp(Int(Equals), Int(4) |> Exp.fresh, Int(3) |> Exp.fresh)
    |> Exp.fresh,
    [
      (Bool(true) |> Pat.fresh, Int(24) |> Exp.fresh),
      (Bool(false) |> Pat.fresh, Int(32) |> Exp.fresh),
    ],
  )
  |> Exp.fresh;
let inconsistent_case_test = () =>
  parser_test("Case test", case_uexp, case_menhir_str);

//Function free var application menhir test
let ap_fun_uexp: Exp.t =
  Ap(
    Forward,
    Fun(
      Var("x") |> Pat.fresh,
      BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh)
      |> Exp.fresh,
      None,
      None,
    )
    |> Exp.fresh,
    Var("y") |> Exp.fresh,
  )
  |> Exp.fresh;
let ap_fun_str = "
    (fun x -> 4 + 5)(y)
";
let ap_fun_test = () =>
  parser_test(
    "Application of a function (menhir)",
    dhexp_of_uexp(ap_fun_uexp),
    ap_fun_str,
  );

//Consistent if statement menhir test
let consistent_if_uexp: Exp.t =
  If(Bool(false) |> Exp.fresh, Int(8) |> Exp.fresh, Int(6) |> Exp.fresh)
  |> Exp.fresh;

let consistent_if_str = "
    if false then 8 else 6
";
let consistent_if_menhir = () =>
  parser_test(
    "Consistent case with rules (BoolLit(true), IntLit(8)) and (BoolLit(false), IntLit(6))",
    dhexp_of_uexp(consistent_if_uexp),
    consistent_if_str,
  );

//Single integer menhir test
let single_int_str = "8";
let single_int_uexp: Exp.t = {
  ids: [id_at(0)],
  term: Int(8),
  copied: false,
};
let single_integer_menhir = () =>
  parser_test(
    "Single integer test (menhir)",
    single_int_uexp,
    single_int_str,
  );

// //Menhir let expression test
// let let_exp_str = "let (a, b) = (4, 6) in a - b";
// let let_exp_uexp: Exp.t =
//   Let(
//     Tuple([Var("a") |> Pat.fresh, Var("b") |> Pat.fresh]) |> Pat.fresh,
//     Tuple([Int(4) |> Exp.fresh, Int(6) |> Exp.fresh]) |> Exp.fresh,
//     BinOp(Int(Minus), Var("a") |> Exp.fresh, Var("b") |> Exp.fresh)
//     |> Exp.fresh,
//   )
//   |> Exp.fresh;
// let let_exp_menhir = () =>
//   alco_check_menhir(
//     "Let expression for tuple (a, b) (menhir)",
//     let_exp_str,
//     let_exp_uexp,
//   );
//
// let typ_ap_str = "(typfun x -> 4) @ <Int>";
// let typ_ap_uexp: Exp.t =
//   TypAp(
//     TypFun(Var("x") |> TPat.fresh, Int(4) |> Exp.fresh, None) |> Exp.fresh,
//     Int |> Typ.fresh,
//   )
//   |> Exp.fresh;
// let typ_ap_menhir = () =>
//   alco_check_menhir("Type ap test (menhir)", typ_ap_str, typ_ap_uexp);
//
// let failed_cast_str = "1 ?<Int => String>";
// let failed_cast_uexp: Exp.t =
//   FailedCast(Int(1) |> Exp.fresh, Int |> Typ.fresh, String |> Typ.fresh)
//   |> Exp.fresh;
// let failed_cast_menhir = () =>
//   alco_check_menhir(
//     "Failed cast test (menhir)",
//     failed_cast_str,
//     failed_cast_uexp,
//   );

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
  test_case("Ap Fun", `Quick, ap_fun_test()),
  test_case("Single integer", `Quick, single_integer_menhir()),
];

open Alcotest;
open Haz3lcore;

/*Create a testable type for dhexp which requires
  an equal function (dhexp_eq) and a print function (dhexp_print) */
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);
let mk_map = CoreSettings.on |> Interface.Statics.mk_map;
let dhexp_of_uexp = u => Elaborator.elaborate(mk_map(u), u) |> fst;
let alco_check = dhexp_typ |> Alcotest.check;

let u1: Exp.t = {ids: [id_at(0)], term: Int(8), copied: false};
let single_integer = () =>
  alco_check("Integer literal 8", u1, dhexp_of_uexp(u1));

let u2: Exp.t = {ids: [id_at(0)], term: EmptyHole, copied: false};
let empty_hole = () => alco_check("Empty hole", u2, dhexp_of_uexp(u2));

let u3: Exp.t = {
  ids: [id_at(0)],
  term: Parens({ids: [id_at(1)], term: Var("y"), copied: false}),
  copied: false,
};

let free_var = () => alco_check("free variable", u3, dhexp_of_uexp(u3));

let u4: Exp.t =
  Let(
    Tuple([Var("a") |> Pat.fresh, Var("b") |> Pat.fresh]) |> Pat.fresh,
    Tuple([Int(4) |> Exp.fresh, Int(6) |> Exp.fresh]) |> Exp.fresh,
    BinOp(Int(Minus), Var("a") |> Exp.fresh, Var("b") |> Exp.fresh)
    |> Exp.fresh,
  )
  |> Exp.fresh;

let let_exp = () =>
  alco_check("Let expression for tuple (a, b)", u4, dhexp_of_uexp(u4));

let u5 =
  BinOp(Int(Plus), Bool(false) |> Exp.fresh, Var("y") |> Exp.fresh)
  |> Exp.fresh;

let d5 =
  BinOp(
    Int(Plus),
    FailedCast(Bool(false) |> Exp.fresh, Bool |> Typ.fresh, Int |> Typ.fresh)
    |> Exp.fresh,
    Cast(
      Var("y") |> Exp.fresh,
      Unknown(Internal) |> Typ.fresh,
      Int |> Typ.fresh,
    )
    |> Exp.fresh,
  )
  |> Exp.fresh;

let bin_op = () =>
  alco_check(
    "Inconsistent binary integer operation (plus)",
    d5,
    dhexp_of_uexp(u5),
  );

let u6: Exp.t =
  If(Bool(false) |> Exp.fresh, Int(8) |> Exp.fresh, Int(6) |> Exp.fresh)
  |> Exp.fresh;

let consistent_if = () =>
  alco_check(
    "Consistent case with rules (BoolLit(true), IntLit(8)) and (BoolLit(false), IntLit(6))",
    u6,
    dhexp_of_uexp(u6),
  );

let u7: Exp.t =
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

let ap_fun = () =>
  alco_check("Application of a function", u7, dhexp_of_uexp(u7));

let u8: Exp.t =
  Match(
    BinOp(Int(Equals), Int(4) |> Exp.fresh, Int(3) |> Exp.fresh)
    |> Exp.fresh,
    [
      (Bool(true) |> Pat.fresh, Int(24) |> Exp.fresh),
      (Bool(false) |> Pat.fresh, Bool(false) |> Exp.fresh),
    ],
  )
  |> Exp.fresh;

let d8: Exp.t =
  Match(
    BinOp(Int(Equals), Int(4) |> Exp.fresh, Int(3) |> Exp.fresh)
    |> Exp.fresh,
    [
      (
        Bool(true) |> Pat.fresh,
        Cast(
          Int(24) |> Exp.fresh,
          Int |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Exp.fresh,
      ),
      (
        Bool(false) |> Pat.fresh,
        Cast(
          Bool(false) |> Exp.fresh,
          Bool |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Exp.fresh,
      ),
    ],
  )
  |> Exp.fresh;

let inconsistent_case = () =>
  alco_check(
    "Inconsistent branches where the first branch is an integer and second branch is a boolean",
    d8,
    dhexp_of_uexp(u8),
  );

let u9: Exp.t =
  Let(
    Cast(
      Var("f") |> Pat.fresh,
      Arrow(Int |> Typ.fresh, Int |> Typ.fresh) |> Typ.fresh,
      Unknown(Internal) |> Typ.fresh,
    )
    |> Pat.fresh,
    Fun(
      Var("x") |> Pat.fresh,
      BinOp(Int(Plus), Int(1) |> Exp.fresh, Var("x") |> Exp.fresh)
      |> Exp.fresh,
      None,
      None,
    )
    |> Exp.fresh,
    Int(55) |> Exp.fresh,
  )
  |> Exp.fresh;

let d9: Exp.t =
  Let(
    Var("f") |> Pat.fresh,
    Fun(
      Var("x") |> Pat.fresh,
      BinOp(Int(Plus), Int(1) |> Exp.fresh, Var("x") |> Exp.fresh)
      |> Exp.fresh,
      None,
      Some("f"),
    )
    |> Exp.fresh,
    Int(55) |> Exp.fresh,
  )
  |> Exp.fresh;

let let_fun = () =>
  alco_check(
    "Let expression for function which is not recursive",
    d9,
    dhexp_of_uexp(u9),
  );

let elaboration_tests = [
  test_case("Single integer", `Quick, single_integer),
  test_case("Empty hole", `Quick, empty_hole),
  test_case("Free variable", `Quick, free_var),
  test_case("Let expression", `Quick, let_exp),
  test_case("Inconsistent binary operation", `Quick, bin_op),
  test_case("Consistent if statement", `Quick, consistent_if),
  test_case("Application of function on free variable", `Quick, ap_fun),
  test_case("Inconsistent case statement", `Quick, inconsistent_case),
  test_case("Let expression for a function", `Quick, let_fun),
];

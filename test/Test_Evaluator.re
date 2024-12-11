open Alcotest;
open Haz3lcore;
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let evaluation_test = (msg, expected, unevaluated) =>
  check(
    dhexp_typ,
    msg,
    expected,
    ProgramResult.Result.unbox(
      snd(Evaluator.evaluate'(Builtins.env_init, {d: unevaluated})),
    ),
  );

let test_int = () =>
  evaluation_test("8", Int(8) |> Exp.fresh, Int(8) |> Exp.fresh);

let test_sum = () =>
  evaluation_test(
    "4 + 5",
    Int(9) |> Exp.fresh,
    BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh) |> Exp.fresh,
  );

let test_function_application = () =>
  evaluation_test(
    "float_of_int(1)",
    Float(1.0) |> Exp.fresh,
    Ap(Forward, Var("float_of_int") |> Exp.fresh, Int(1) |> Exp.fresh)
    |> Exp.fresh,
  );

let test_function_deferral = () =>
  evaluation_test(
    "string_sub(\"hello\", 1, _)(2)",
    String("el") |> Exp.fresh,
    Ap(
      Forward,
      DeferredAp(
        Var("string_sub") |> Exp.fresh,
        [
          String("hello") |> Exp.fresh,
          Int(1) |> Exp.fresh,
          Deferral(InAp) |> Exp.fresh,
        ],
      )
      |> Exp.fresh,
      Int(2) |> Exp.fresh,
    )
    |> Exp.fresh,
  );

let tet_ap_of_hole_deferral = () =>
  evaluation_test(
    "?(_, _, 3)(1., true)",
    Ap(
      Forward,
      Cast(
        Cast(
          EmptyHole |> Exp.fresh,
          Unknown(Internal) |> Typ.fresh,
          Arrow(
            Unknown(Internal) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Typ.fresh,
        )
        |> Exp.fresh,
        Arrow(
          Unknown(Internal) |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Typ.fresh,
        Arrow(
          Prod([
            Unknown(Internal) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          ])
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Typ.fresh,
      )
      |> Exp.fresh,
      Tuple([
        Cast(
          Float(1.) |> Exp.fresh,
          Float |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Exp.fresh,
        Cast(
          Bool(true) |> Exp.fresh,
          Bool |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Exp.fresh,
        Cast(
          Int(3) |> Exp.fresh,
          Int |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Exp.fresh,
      ])
      |> Exp.fresh,
    )
    |> Exp.fresh,
    Ap(
      Forward,
      DeferredAp(
        Cast(
          Cast(
            EmptyHole |> Exp.fresh,
            Unknown(Internal) |> Typ.fresh,
            Arrow(
              Unknown(Internal) |> Typ.fresh,
              Unknown(Internal) |> Typ.fresh,
            )
            |> Typ.fresh,
          )
          |> Exp.fresh,
          Arrow(
            Unknown(Internal) |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Typ.fresh,
          Arrow(
            Prod([
              Unknown(Internal) |> Typ.fresh,
              Unknown(Internal) |> Typ.fresh,
              Unknown(Internal) |> Typ.fresh,
            ])
            |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Typ.fresh,
        )
        |> Exp.fresh,
        [
          Deferral(InAp) |> Exp.fresh,
          Deferral(InAp) |> Exp.fresh,
          Cast(
            Int(3) |> Exp.fresh,
            Int |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Exp.fresh,
        ],
      )
      |> Exp.fresh,
      Tuple([
        Cast(
          Float(1.) |> Exp.fresh,
          Float |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Exp.fresh,
        Cast(
          Bool(true) |> Exp.fresh,
          Bool |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Exp.fresh,
      ])
      |> Exp.fresh,
    )
    |> Exp.fresh,
  );

let tests = [
  test_case("Integer literal", `Quick, test_int),
  test_case("Integer sum", `Quick, test_sum),
  test_case("Function application", `Quick, test_function_application),
  test_case("Function deferral", `Quick, test_function_deferral),
  test_case("Deferral applied to hole", `Quick, tet_ap_of_hole_deferral),
];

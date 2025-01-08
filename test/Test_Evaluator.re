open Alcotest;
open Haz3lcore;
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let evaluation_test = (msg, expected, unevaluated) =>
  check(
    dhexp_typ,
    msg,
    expected,
    unevaluated
    |> Evaluator.evaluate'(Builtins.env_init)
    |> snd
    |> ProgramResult.Result.unbox
    |> Exp.substitute_closures(Builtins.env_init),
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

let test_ap_of_hole_deferral = () =>
  evaluation_test(
    "?(_, _, 3)(1., true)",
    Ap(
      Forward,
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
      Cast(
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
        Prod([
          Unknown(Internal) |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        ])
        |> Typ.fresh,
        Unknown(Internal) |> Typ.fresh,
      )
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

let test_multi_arg_builtin_cast = () =>
  evaluation_test(
    "string_compare((\"Hello\", \"World\"):(?, ?))",
    Int(-1) |> Exp.fresh,
    Ap(
      Forward,
      BuiltinFun("string_compare") |> Exp.fresh,
      Cast(
        Tuple([
          Cast(
            String("Hello") |> Exp.fresh,
            String |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Exp.fresh,
          Cast(
            String("World") |> Exp.fresh,
            String |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Exp.fresh,
        ])
        |> Exp.fresh,
        Prod([
          Unknown(Internal) |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        ])
        |> Typ.fresh,
        Prod([String |> Typ.fresh, String |> Typ.fresh]) |> Typ.fresh,
      )
      |> Exp.fresh,
    )
    |> Exp.fresh,
  );

let test_variable_capture = () =>
  evaluation_test(
    {|let u = 5 in let f = fun () -> u in let u = 3 in f()|},
    Int(5) |> Exp.fresh,
    Let(
      Var("u") |> Pat.fresh,
      Int(5) |> Exp.fresh,
      Let(
        Var("f") |> Pat.fresh,
        Fun(Tuple([]) |> Pat.fresh, Var("u") |> Exp.fresh, None)
        |> Exp.fresh,
        Let(
          Var("u") |> Pat.fresh,
          Int(3) |> Exp.fresh,
          Ap(Forward, Var("f") |> Exp.fresh, Tuple([]) |> Exp.fresh)
          |> Exp.fresh,
        )
        |> Exp.fresh,
      )
      |> Exp.fresh,
    )
    |> Exp.fresh,
  );

let test_unbound_lookup = () =>
  evaluation_test(
    "(fun x -> x)(x)",
    Var("x") |> Exp.fresh,
    Ap(
      Forward,
      Fun(Var("x") |> Pat.fresh, Var("x") |> Exp.fresh, None) |> Exp.fresh,
      Var("x") |> Exp.fresh,
    )
    |> Exp.fresh,
  );

let test_unevaluated_if = () =>
  evaluation_test(
    "let x = 5 in if ? then x else x",
    If(EmptyHole |> Exp.fresh, Int(5) |> Exp.fresh, Int(5) |> Exp.fresh)
    |> Exp.fresh,
    Let(
      Var("x") |> Pat.fresh,
      Int(5) |> Exp.fresh,
      If(
        EmptyHole |> Exp.fresh,
        Var("x") |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
    )
    |> Exp.fresh,
  );

let tests = (
  "Evaluator",
  [
    test_case("Integer literal", `Quick, test_int),
    test_case("Integer sum", `Quick, test_sum),
    test_case("Function application", `Quick, test_function_application),
    test_case("Function deferral", `Quick, test_function_deferral),
    test_case("Deferral applied to hole", `Quick, test_ap_of_hole_deferral),
    test_case(
      "Multi-arg builtin with cast",
      `Quick,
      test_multi_arg_builtin_cast,
    ),
    test_case("Variable capture", `Quick, test_variable_capture),
    test_case("Unbound lookup", `Quick, test_unbound_lookup),
    test_case("Unevaluated if closure", `Quick, test_unevaluated_if),
  ],
);

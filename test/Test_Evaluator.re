open Alcotest;
open Haz3lcore;
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let evaluation_test = (msg, expected, unevaluated) =>
  check(
    dhexp_typ,
    msg,
    expected,
    ProgramResult.Result.unbox(
      snd(Evaluator.evaluate'(Builtins.env_init, unevaluated)),
    ),
  );
let parse_exp = (s: string) => {
  switch (MakeTerm.parse_exp(s)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};
let elaborate = u =>
  Elaborator.elaborate(Statics.mk(CoreSettings.on, Builtins.ctx_init, u), u)
  |> fst;
let parse_and_evaluate = (s: string) =>
  ProgramResult.Result.unbox(
    snd(Evaluator.evaluate'(Builtins.env_init, elaborate(parse_exp(s)))),
  );

let parse_and_evaluate_test =
    (~msg: option(string)=?, expected: string, actual: string) =>
  evaluation_test(
    Option.value(~default=expected ++ " == " ++ actual, msg),
    parse_exp(expected),
    elaborate(parse_exp(actual)),
  );

let test_int = () =>
  evaluation_test("8", Int(8) |> Exp.fresh, Int(8) |> Exp.fresh);

let test_sum = () =>
  evaluation_test(
    "4 + 5",
    Int(9) |> Exp.fresh,
    BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh) |> Exp.fresh,
  );

let test_labeled_tuple_projection = () =>
  evaluation_test(
    "(a=1, b=2, c=?).a",
    Int(1) |> Exp.fresh,
    Dot(
      Tuple([
        TupLabel(Label("a") |> Exp.fresh, Int(1) |> Exp.fresh) |> Exp.fresh,
        TupLabel(Label("b") |> Exp.fresh, Int(2) |> Exp.fresh) |> Exp.fresh,
        TupLabel(Label("c") |> Exp.fresh, EmptyHole |> Exp.fresh)
        |> Exp.fresh,
      ])
      |> Exp.fresh,
      Label("a") |> Exp.fresh // This is a var now for parsing reasons
    )
    |> Exp.fresh,
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

let tests = (
  "Evaluator",
  [
    test_case("Integer literal", `Quick, test_int),
    test_case("Integer sum", `Quick, test_sum),
    test_case("Function application", `Quick, test_function_application),
    test_case("Function deferral", `Quick, test_function_deferral),
    test_case("Deferral applied to hole", `Quick, tet_ap_of_hole_deferral),
    test_case("Elaborated Pattern for labeled tuple", `Quick, () =>
      parse_and_evaluate_test(
        "2",
        {|let x : (a=Int) -> Int = fun a -> a in x(2)|},
      )
    ),
    test_case("Labeled tuple field access", `Quick, () =>
      parse_and_evaluate_test("1", {|(a=1,b=2).a|})
    ),
    test_case("Anonymous function with explicit label", `Quick, () => {
      parse_and_evaluate_test(
        "5",
        {|let fn : (a=String) -> Int =
  fun (a=a : String) -> string_length(a)
in fn("hello")|},
      )
    }),
    test_case("Anonymous function without explicit label", `Quick, () => {
      parse_and_evaluate_test(
        "5",
        {|let fn : (a=String) -> Int =
            fun (a : String) -> string_length(a)
          in fn("hello")|},
      )
    }),
    test_case("Dot operation for missing label", `Quick, () =>
      parse_and_evaluate_test("(a=1,b=2).c", "(a=1,b=2).c")
    ),
  ],
);

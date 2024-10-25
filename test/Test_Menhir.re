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

// Existing recovering parser
let make_term_parse = (s: string) =>
  MakeTerm.from_zip_for_sem(Option.get(Printer.zipper_of_string(s))).term;

let menhir_matches = (name: string, exp: Term.Exp.t, actual: string) =>
  alco_check(
    name ++ " matches expected type",
    exp,
    Haz3lmenhir.Conversion.Exp.of_menhir_ast(
      Haz3lmenhir.Interface.parse_program(actual),
    ),
  );

let menhir_only_test = (name: string, exp: Term.Exp.t, actual: string) =>
  test_case(name, `Quick, () => {menhir_matches(name, exp, actual)});

// TODO Assert against result instead of exception for parse failure for better error messages
let parser_test = (name: string, exp: Term.Exp.t, actual: string) =>
  test_case(
    name,
    `Quick,
    () => {
      alco_check("Does not match MakeTerm", exp, make_term_parse(actual));

      menhir_matches(name, exp, actual);
    },
  );

let fun_exp: Exp.t =
  Fun(Var("x") |> Pat.fresh, Var("x") |> Exp.fresh, None, None) |> Exp.fresh;

let tests = [
  parser_test("Integer Literal", Int(8) |> Exp.fresh, "8"),
  parser_test("Fun", fun_exp, "fun x -> x"),
  parser_test(
    "String Literal",
    String("Hello World") |> Exp.fresh,
    "\"Hello World\"",
  ),
  parser_test("Bool Literal", Bool(true) |> Exp.fresh, "true"),
  parser_test("Empty Hole", EmptyHole |> Exp.fresh, "?"),
  parser_test("Var", Var("x") |> Exp.fresh, "x"),
  parser_test("Parens", Parens(Var("y") |> Exp.fresh) |> Exp.fresh, "(y)"),
  parser_test(
    "BinOp",
    BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh) |> Exp.fresh,
    "4 + 5",
  ),
  parser_test(
    "Let",
    Let(Var("x") |> Pat.fresh, Int(5) |> Exp.fresh, Var("x") |> Exp.fresh)
    |> Exp.fresh,
    "let x = 5 in x",
  ),
  parser_test(
    "Tuple",
    Tuple([Int(4) |> Exp.fresh, Int(5) |> Exp.fresh]) |> Exp.fresh,
    "(4, 5)" // TODO Verify with maketerm. Should this be parens or not
  ),
  parser_test(
    "Match",
    Match(
      Int(4) |> Exp.fresh,
      [
        (Int(1) |> Pat.fresh, String("hello") |> Exp.fresh),
        (Wild |> Pat.fresh, String("world") |> Exp.fresh),
      ],
    )
    |> Exp.fresh,
    {|case 4
       | 1 => "hello"
       | _ => "world"
      end|},
  ),
  parser_test(
    "If",
    If(Bool(true) |> Exp.fresh, Int(8) |> Exp.fresh, Int(6) |> Exp.fresh)
    |> Exp.fresh,
    "if true then 8 else 6",
  ),
  parser_test(
    "Deferred Ap",
    DeferredAp(Var("x") |> Exp.fresh, [Deferral(InAp) |> Exp.fresh])
    |> Exp.fresh,
    "x(_)",
  ),
  parser_test(
    "Cons",
    Cons(Int(1) |> Exp.fresh, ListLit([]) |> Exp.fresh) |> Exp.fresh,
    "1 :: []",
  ),
  parser_test(
    "ListLit",
    ListLit([
      Int(1) |> Exp.fresh,
      Int(2) |> Exp.fresh,
      Int(3) |> Exp.fresh,
    ])
    |> Exp.fresh,
    "[1, 2, 3]",
  ),
  menhir_only_test(
    "Constructor",
    Constructor("A", Unknown(Internal) |> Typ.fresh) |> Exp.fresh,
    "A:Unknown Internal" // This is source incompatible with make_term which does not find the type. We also don't allow expression casts.
  ),
  parser_test(
    "Type Alias",
    TyAlias(Var("x") |> TPat.fresh, Int |> Typ.fresh, Int(1) |> Exp.fresh)
    |> Exp.fresh,
    "type x = Int in 1",
  ),
  parser_test(
    "Test",
    Test(
      BinOp(Int(Equals), Int(3) |> Exp.fresh, Int(3) |> Exp.fresh)
      |> Exp.fresh,
    )
    |> Exp.fresh,
    "test 3 == 3 end",
  ),
  parser_test(
    "Filter",
    Filter(
      Filter({act: (Eval, One), pat: Int(3) |> Exp.fresh}),
      Int(3) |> Exp.fresh,
    )
    |> Exp.fresh,
    "eval 3 in 3" // TODO Use other filter commands
  ),
  parser_test(
    "List Concat",
    ListConcat(
      ListLit([Int(1) |> Exp.fresh, Int(2) |> Exp.fresh]) |> Exp.fresh,
      ListLit([Int(3) |> Exp.fresh, Int(4) |> Exp.fresh]) |> Exp.fresh,
    )
    |> Exp.fresh,
    "[1, 2] @ [3, 4]",
  ),
  parser_test(
    "Integer Ops",
    BinOp(
      Int(LessThan),
      BinOp(
        Int(Minus),
        BinOp(
          Int(Plus),
          UnOp(Int(Minus), Int(1) |> Exp.fresh) |> Exp.fresh,
          Int(2) |> Exp.fresh,
        )
        |> Exp.fresh,
        BinOp(
          Int(Divide),
          Int(3) |> Exp.fresh,
          BinOp(
            Int(Times),
            Int(4) |> Exp.fresh,
            BinOp(Int(Power), Int(5) |> Exp.fresh, Int(6) |> Exp.fresh)
            |> Exp.fresh,
          )
          |> Exp.fresh,
        )
        |> Exp.fresh,
      )
      |> Exp.fresh,
      Int(8) |> Exp.fresh,
    )
    |> Exp.fresh,
    "-1 + 2 - 3 / 4 * 5 ** 6 < 8" // TODO Add the remaining operators
  ),
  parser_test(
    "Let binding with type ascription",
    Let(
      Cast(
        Var("x") |> Pat.fresh,
        Int |> Typ.fresh,
        Unknown(Internal) |> Typ.fresh,
      )
      |> Pat.fresh,
      Int(5) |> Exp.fresh,
      Var("x") |> Exp.fresh,
    )
    |> Exp.fresh,
    "let (x : Int) = 5 in x",
  ),
  test_case("named_function", `Quick, () => {
    alco_check(
      "named_function matches expected type",
      Fun(
        Pat.Var("x") |> Pat.fresh,
        BinOp(Int(Plus), Var("x") |> Exp.fresh, Int(5) |> Exp.fresh)
        |> Exp.fresh,
        None,
        Some("f"),
      )
      |> Exp.fresh,
      Haz3lmenhir.Conversion.Exp.of_menhir_ast(
        Haz3lmenhir.Interface.parse_program("named_fun f x -> x + 5"),
      ),
    )
  }),
];

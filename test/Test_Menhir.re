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

// Existing recovering parser
let make_term_parse = (s: string) =>
  MakeTerm.from_zip_for_sem(Option.get(Printer.zipper_of_string(s))).term;

let menhir_matches = (exp: Term.Exp.t, actual: string) =>
  alco_check(
    "menhir matches expected parse",
    exp,
    Haz3lmenhir.Conversion.Exp.of_menhir_ast(
      Haz3lmenhir.Interface.parse_program(actual),
    ),
  );

let menhir_only_test = (name: string, exp: Term.Exp.t, actual: string) =>
  test_case(name, `Quick, () => {menhir_matches(exp, actual)});

// TODO Remove these before merge. Using it to mark skipped tests until we fix them.
let skip_parser_test = (name: string, _exp: Term.Exp.t, _actual: string) =>
  test_case(name, `Quick, () => {Alcotest.skip()});
let skip_menhir_only_test = (name: string, _exp: Term.Exp.t, _actual: string) =>
  test_case(name, `Quick, () => {Alcotest.skip()});

// TODO Assert against result instead of exception for parse failure for better error messages
let parser_test = (name: string, exp: Term.Exp.t, actual: string) =>
  test_case(
    name,
    `Quick,
    () => {
      alco_check(
        "expected parse matches MakeTerm parse",
        exp,
        make_term_parse(actual),
      );
      menhir_matches(exp, actual);
    },
  );

let menhir_maketerm_equivalent_test = (name: string, actual: string) =>
  test_case(name, `Quick, () => {
    alco_check(
      "Menhir parse matches MakeTerm parse",
      make_term_parse(actual),
      Haz3lmenhir.Conversion.Exp.of_menhir_ast(
        Haz3lmenhir.Interface.parse_program(actual),
      ),
    )
  });

let fun_exp: Exp.t =
  Fun(Var("x") |> Pat.fresh, Var("x") |> Exp.fresh, None, None) |> Exp.fresh;

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
    free_var_uexp,
    "y",
  );

//Menhir test for a binary operation
let bin_op_uexp: Exp.t =
  BinOp(Int(Plus), Int(1) |> Exp.fresh, Int(2) |> Exp.fresh) |> Exp.fresh;

let bin_op_str = "1 + 2";

let bin_op_test = () =>
  parser_test("binary integer operation (plus)", bin_op_uexp, bin_op_str);

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
  parser_test("Application of a function (menhir)", ap_fun_uexp, ap_fun_str);

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
    consistent_if_uexp,
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

let tests =
  [
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
    parser_test(
      "Parens",
      Parens(Var("y") |> Exp.fresh) |> Exp.fresh,
      "(y)",
    ),
    parser_test(
      "BinOp",
      BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh)
      |> Exp.fresh,
      "4 + 5",
    ),
    parser_test(
      "Let",
      Let(
        Var("x") |> Pat.fresh,
        Int(5) |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
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
      "A",
    ),
    menhir_only_test(
      "Constructor cast",
      Cast(
        Constructor("A", Unknown(Internal) |> Typ.fresh) |> Exp.fresh,
        Unknown(Internal) |> Typ.fresh,
        Int |> Typ.fresh,
      )
      |> Exp.fresh,
      "A : Int",
    ),
    menhir_only_test(
      "Constructor of specific sum type",
      Constructor("A", Int |> Typ.fresh) |> Exp.fresh,
      "A ~ Int",
    ),
    // TODO Fix for the tests below
    menhir_only_test(
      "Constructor with Type Variable",
      Constructor("A", Var("T") |> Typ.fresh) |> Exp.fresh,
      "A ~ T",
    ),
    parser_test(
      "Type Variable",
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Var("T") |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        EmptyHole |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
      "let x : T = ? in x",
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
        Filter({act: (Eval, All), pat: Int(3) |> Exp.fresh}),
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
      "times and divide precendence",
      BinOp(
        Int(Times),
        Int(1) |> Exp.fresh,
        BinOp(Int(Divide), Int(2) |> Exp.fresh, Int(3) |> Exp.fresh)
        |> Exp.fresh,
      )
      |> Exp.fresh,
      "1 * 2 / 3",
    ),
    parser_test(
      "plus and minus precendence",
      BinOp(
        Int(Plus),
        BinOp(Int(Minus), Int(1) |> Exp.fresh, Int(2) |> Exp.fresh)
        |> Exp.fresh,
        Int(3) |> Exp.fresh,
      )
      |> Exp.fresh,
      "1 - 2 + 3",
    ),
    parser_test(
      "Integer Ops",
      BinOp(
        Int(GreaterThanOrEqual),
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
      "-1 + 2 - 3 / 4 * 5 ** 6 >= 8",
    ),
    parser_test("Float", Float(1.) |> Exp.fresh, "1."),
    parser_test(
      "Float Ops",
      BinOp(
        Float(LessThan),
        BinOp(
          Float(Minus),
          Float(2.) |> Exp.fresh,
          BinOp(
            Float(Divide),
            Float(3.) |> Exp.fresh,
            BinOp(
              Float(Times),
              Float(4.) |> Exp.fresh,
              BinOp(
                Float(Power),
                Float(5.) |> Exp.fresh,
                Float(6.) |> Exp.fresh,
              )
              |> Exp.fresh,
            )
            |> Exp.fresh,
          )
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Float(8.) |> Exp.fresh,
      )
      |> Exp.fresh,
      "2. -. 3. /. 4. *. 5. **. 6. <. 8.",
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
      "let (x: Int) = 5 in x",
    ),
    menhir_only_test(
      "named_function",
      Fun(
        Pat.Var("x") |> Pat.fresh,
        BinOp(Int(Plus), Var("x") |> Exp.fresh, Int(5) |> Exp.fresh)
        |> Exp.fresh,
        None,
        Some("f"),
      )
      |> Exp.fresh,
      "named_fun f x -> x + 5",
    ),
    parser_test(
      "basic sum type",
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Sum([
            Variant("A", [], None),
            Variant("B", [], None),
            Variant("C", [], Some(Int |> Typ.fresh)),
          ])
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Ap(
          Forward,
          Constructor("C", Unknown(Internal) |> Typ.fresh) |> Exp.fresh,
          Int(7) |> Exp.fresh,
        )
        |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
      "let x : +A +B +C(Int) = C(7) in x",
    ),
    menhir_maketerm_equivalent_test("Empty Type Hole", "let g: ? = 7 in g"),
    menhir_maketerm_equivalent_test(
      "Pattern with type ascription",
      "fun (b : Bool) -> b",
    ),
    menhir_only_test(
      "Type Hole in arrow cast",
      Fun(
        Cast(
          Var("b") |> Pat.fresh,
          Parens(
            Arrow(
              Unknown(Hole(EmptyHole)) |> Typ.fresh,
              Unknown(Hole(EmptyHole)) |> Typ.fresh,
            )
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        EmptyHole |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
      "fun (b : ? -> ?) -> ?",
    ),
    menhir_only_test(
      "multiargument function",
      Ap(
        Forward,
        Var("f") |> Exp.fresh,
        Tuple([Int(1) |> Exp.fresh, Int(2) |> Exp.fresh]) |> Exp.fresh,
      )
      |> Exp.fresh,
      "f(1, 2)",
    ),
    menhir_only_test(
      "Sum type definition without leading plus",
      TyAlias(
        Var("GoodSum") |> TPat.fresh,
        Sum([
          Variant("A", [], None),
          Variant("B", [], None),
          Variant("C", [], Some(Int |> Typ.fresh)),
        ])
        |> Typ.fresh,
        Int(1) |> Exp.fresh,
      )
      |> Exp.fresh,
      "type GoodSum = A + B + C(Int) in 1",
    ),
    menhir_maketerm_equivalent_test(
      "partial sum type",
      "type Partial = Ok(?) + ? in ?",
    ),
  ]
  @ {
    let strip_comments = str => {
      let re = Str.regexp("#[^#]*#");
      Str.global_replace(re, "", str);
    };
    let replace_holes = str => {
      // List of lines in doc buffers that are not correctly formed
      let failing_parse_strings = [
        "type ? = badTypeToken in",
        "type NotASum = NotInSum(Bool) in",
        "+ notvalid",
        "type Bool = ? in",
        "+ Int(Int)",
        "+ Int(Int)",
        "+ (?)(Int)",
        "+ A(Bool)(Int)",
        "type (?, ?) = ? in",
        "+ Bool",
      ];
      let remove_failing_parse = str => {
        List.fold_left(
          (acc, s) => Str.global_replace(Str.regexp_string(s), "", acc),
          str,
          failing_parse_strings,
        );
      };

      str
      |> Str.global_replace(Str.regexp("=   in"), "= ? in", _)
      |> remove_failing_parse
      |> Str.global_replace(Str.regexp("^ *\n"), "", _);
    };
    let (_, slides: list((string, PersistentZipper.t)), _) =
      Haz3lweb.Init.startup.documentation;
    List.map(
      ((name, slide): (string, PersistentZipper.t)) => {
        test_case(
          "Documentation buffer: " ++ name,
          `Quick,
          () => {
            let cleaned_source =
              replace_holes(strip_comments(slide.backup_text));
            print_endline(cleaned_source);
            let _menhir_parsed =
              Haz3lmenhir.Conversion.Exp.of_menhir_ast(
                Haz3lmenhir.Interface.parse_program(cleaned_source),
              );
            ();
            // alco_check(
            //   "Menhir parse does not match MakeTerm",
            //   make_term_parse(slide.backup_text),
            //   menhir_parsed,
            // );
          },
        )
      },
      slides,
    );
  };

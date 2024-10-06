open Alcotest;
open Haz3lcore;

/*Create a testable type for dhexp which requires
  an equal function (dhexp_eq) and a print function (dhexp_print) */
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);

let mk_map = Statics.mk(CoreSettings.on, Builtins.ctx_init);
let dhexp_of_uexp = u => Elaborator.elaborate(mk_map(u), u) |> fst;
let alco_check = dhexp_typ |> Alcotest.check;

module MenhirElaborationTests = {
  let alco_check_menhir = (name: string, dhexp: string, uexp: Term.Exp.t) =>
    alco_check(
      name,
      Haz3lmenhir.Conversion.Exp.of_menhir_ast(
        Haz3lmenhir.Interface.parse_program(dhexp),
      ),
      dhexp_of_uexp(uexp),
    );

  //Test for a let function
  let let_fun_uexp: Exp.t =
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

  let let_fun_str = "
let f =
    fun x ->
        1 + x
    f
    in
55";

  let let_fun_menhir = () =>
    alco_check_menhir(
      "Let expression for a function which is not recursive (menhir)",
      let_fun_str,
      let_fun_uexp,
    );

  //Test for an empty hole
  let empty_hole_str = "?";
  let empty_hole_uexp: Exp.t = {
    ids: [id_at(0)],
    term: EmptyHole,
    copied: false,
  };
  let empty_hole_menhir = () =>
    alco_check_menhir("Empty hole (menhir)", empty_hole_str, empty_hole_uexp);

  //Test for a free variable
  let free_var_uexp: Exp.t = {
    ids: [id_at(0)],
    term: Parens({ids: [id_at(1)], term: Var("y"), copied: false}),
    copied: false,
  };
  let free_var_menhir = () =>
    alco_check(
      "Nonempty hole with free variable (menhir)",
      Haz3lmenhir.Conversion.Exp.of_menhir_ast(
        Haz3lmenhir.Interface.parse_program("y"),
      ),
      dhexp_of_uexp(free_var_uexp),
    );

  //Menhir test for a binary operation
  let bin_op_uexp: Exp.t =
    BinOp(Int(Plus), Bool(false) |> Exp.fresh, Var("y") |> Exp.fresh)
    |> Exp.fresh;

  let bin_op_str = "false?<Bool => Int> + y<Unknown Internal => Int>";

  let bin_op_menhir = () =>
    alco_check(
      "Inconsistent binary integer operation (plus)",
      Haz3lmenhir.Conversion.Exp.of_menhir_ast(
        Haz3lmenhir.Interface.parse_program(bin_op_str),
      ),
      dhexp_of_uexp(bin_op_uexp),
    );

  //Inconsistent branches menhir test
  let inconsistent_case_menhir_str = "
    case 4 == 3
    | true => 24<Int => Unknown Internal>
    | false => false<Bool => Unknown Internal>
    end
";
  let inconsistent_case_uexp: Exp.t =
    Match(
      BinOp(Int(Equals), Int(4) |> Exp.fresh, Int(3) |> Exp.fresh)
      |> Exp.fresh,
      [
        (Bool(true) |> Pat.fresh, Int(24) |> Exp.fresh),
        (Bool(false) |> Pat.fresh, Bool(false) |> Exp.fresh),
      ],
    )
    |> Exp.fresh;
  let inconsistent_case_menhir = () =>
    alco_check_menhir(
      "Inconsistent branches where the first branch is an integer and second branch is a boolean (menhir)",
      inconsistent_case_menhir_str,
      inconsistent_case_uexp,
    );

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
  let ap_fun_menhir = () =>
    alco_check(
      "Application of a function (menhir)",
      Haz3lmenhir.Conversion.Exp.of_menhir_ast(
        Haz3lmenhir.Interface.parse_program(ap_fun_str),
      ),
      dhexp_of_uexp(ap_fun_uexp),
    );

  //Consistent if statement menhir test
  let consistent_if_uexp: Exp.t =
    If(Bool(false) |> Exp.fresh, Int(8) |> Exp.fresh, Int(6) |> Exp.fresh)
    |> Exp.fresh;

  let consistent_if_str = "
    if false then 8 else 6
";
  let consistent_if_menhir = () =>
    alco_check(
      "Consistent case with rules (BoolLit(true), IntLit(8)) and (BoolLit(false), IntLit(6))",
      Haz3lmenhir.Conversion.Exp.of_menhir_ast(
        Haz3lmenhir.Interface.parse_program(consistent_if_str),
      ),
      dhexp_of_uexp(consistent_if_uexp),
    );

  //Single integer menhir test
  let single_int_str = "8";
  let single_int_uexp: Exp.t = {
    ids: [id_at(0)],
    term: Int(8),
    copied: false,
  };
  let single_integer_menhir = () =>
    alco_check_menhir(
      "Single integer test (menhir)",
      single_int_str,
      single_int_uexp,
    );

  //Menhir let expression test
  let let_exp_str = "let (a, b) = (4, 6) in a - b";
  let let_exp_uexp: Exp.t =
    Let(
      Tuple([Var("a") |> Pat.fresh, Var("b") |> Pat.fresh]) |> Pat.fresh,
      Tuple([Int(4) |> Exp.fresh, Int(6) |> Exp.fresh]) |> Exp.fresh,
      BinOp(Int(Minus), Var("a") |> Exp.fresh, Var("b") |> Exp.fresh)
      |> Exp.fresh,
    )
    |> Exp.fresh;
  let let_exp_menhir = () =>
    alco_check_menhir(
      "Let expression for tuple (a, b) (menhir)",
      let_exp_str,
      let_exp_uexp,
    );

  let typ_ap_str = "(typfun x -> 4) @ <Int>";
  let typ_ap_uexp: Exp.t =
    TypAp(
      TypFun(Var("x") |> TPat.fresh, Int(4) |> Exp.fresh, None) |> Exp.fresh,
      Int |> Typ.fresh,
    )
    |> Exp.fresh;
  let typ_ap_menhir = () =>
    alco_check_menhir("Type ap test (menhir)", typ_ap_str, typ_ap_uexp);

  let failed_cast_str = "1 ?<Int => String>";
  let failed_cast_uexp: Exp.t =
    FailedCast(Int(1) |> Exp.fresh, Int |> Typ.fresh, String |> Typ.fresh)
    |> Exp.fresh;
  let failed_cast_menhir = () =>
    alco_check_menhir(
      "Failed cast test (menhir)",
      failed_cast_str,
      failed_cast_uexp,
    );

  let constructor_str = "X: Unknown Internal";
  let constructor_uexp: Exp.t =
    Constructor("X", Unknown(Internal) |> Typ.fresh) |> Exp.fresh;
  let constructor_menhir = () =>
    alco_check_menhir(
      "Constructor test (menhir)",
      constructor_str,
      constructor_uexp,
    );

  /*
   <<1 / 2 ? `a`>>
       */
  let dynamic_error_hole_str = "<<(1/0) ? `DivideByZero`>> <Unknown Internal => Int>";
  let dynamic_error_hole_uexp: Exp.t = {
    ids: [id_at(0)],
    term:
      DynamicErrorHole(
        BinOp(Int(Divide), Int(1) |> Exp.fresh, Int(0) |> Exp.fresh)
        |> Exp.fresh,
        InvalidOperationError.DivideByZero,
      ),
    copied: false,
  };
  let dynamic_error_hole_menhir = () =>
    alco_check_menhir(
      "Dynamic error hole (menhir)",
      dynamic_error_hole_str,
      dynamic_error_hole_uexp,
    );

  let builtin_fun_str = "infinity";
  let builtin_fun_uexp: Exp.t = {
    ids: [id_at(0)],
    term: BuiltinFun("infinity"),
    copied: false,
  };
  let builtin_fun_menhir = () =>
    alco_check_menhir(
      "Builtin function test (menhir)",
      builtin_fun_str,
      builtin_fun_uexp,
    );

  let undef_str = "undef";
  let undef_uexp: Exp.t = {ids: [id_at(0)], term: Undefined, copied: false};
  let undef_menhir = () =>
    alco_check_menhir("Undef test (menhir)", undef_str, undef_uexp);

  let test_str = "test 1 ?<Int => Bool> end";
  let test_uexp: Exp.t = {
    ids: [id_at(0)],
    term: Test(Int(1) |> Exp.fresh),
    copied: false,
  };
  let test_menhir = () =>
    alco_check_menhir("Test failed (menhir)", test_str, test_uexp);

  let filter_str = "eval 1, 0";
  let stepper_filter_kind =
    TermBase.StepperFilterKind.Filter({
      pat: Int(1) |> Exp.fresh,
      act: (FilterAction.Eval, FilterAction.All),
    });
  let filter_uexp: Exp.t = {
    ids: [id_at(0)],
    term: Filter(stepper_filter_kind, Int(0) |> Exp.fresh),
    copied: false,
  };
  let filter_menhir = () =>
    alco_check_menhir("Filter test (menhir)", filter_str, filter_uexp);

  //NOTE: left out until deferral elaborations are fixed
  // let deferred_str = "
  // (fun x -> 4 + 5)(_)
  // ";
  // let deferred_uexp: Exp.t =
  //   DeferredAp(
  //     Fun(
  //       Var("x") |> Pat.fresh,
  //       BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh)
  //       |> Exp.fresh,
  //       None,
  //       None,
  //     )
  //     |> Exp.fresh,
  //     [Deferral(InAp) |> Exp.fresh],
  //   )
  //   |> Exp.fresh;
  // let deferred_ap_menhir = () =>
  //   alco_check_menhir("Deferred Ap Test (menhir)", deferred_str, deferred_uexp);

  let undefined_str = "
undef
";
  let undefined_uexp: Exp.t = Undefined |> Exp.fresh;
  let undefined_menhir = () =>
    alco_check_menhir(
      "Undefined test (menhir)",
      undefined_str,
      undefined_uexp,
    );

  let list_exp_str = "[1, 2, 3]";
  let list_exp_uexp: Exp.t = {
    ids: [id_at(0)],
    term:
      ListLit([
        Int(1) |> Exp.fresh,
        Int(2) |> Exp.fresh,
        Int(3) |> Exp.fresh,
      ]),
    copied: false,
  };
  let list_exp_menhir = () =>
    alco_check_menhir("List exp (menhir)", list_exp_str, list_exp_uexp);

  let invalid_str = "
?\"x\"
";
  let invalid_uexp: Exp.t = Invalid("x") |> Exp.fresh;
  let invalid_menhir = () =>
    alco_check_menhir("Invalid test (menhir)", invalid_str, invalid_uexp);

  let ty_alias_str = "
x
";
  let ty_alias_uexp: Exp.t = {
    ids: [id_at(0)],
    term:
      TyAlias(
        Var("x") |> TPat.fresh,
        Int |> Typ.fresh,
        Var("x") |> Exp.fresh,
      ),
    copied: false,
  };
  let ty_alias_menhir = () =>
    alco_check_menhir(
      "Type alias test (menhir)",
      ty_alias_str,
      ty_alias_uexp,
    );

  let list_concat_str = "[1, 2] @ [3, 4]";
  let list_concat_uexp: Exp.t = {
    ids: [id_at(0)],
    term:
      ListConcat(
        ListLit([Int(1) |> Exp.fresh, Int(2) |> Exp.fresh]) |> Exp.fresh,
        ListLit([Int(3) |> Exp.fresh, Int(4) |> Exp.fresh]) |> Exp.fresh,
      ),
    copied: false,
  };
  let list_concat_menhir = () =>
    alco_check_menhir(
      "List concat test (menhir)",
      list_concat_str,
      list_concat_uexp,
    );

  let unop_str = "-1";
  let unop_uexp: Exp.t = {
    ids: [id_at(0)],
    term: UnOp(Int(Minus), Int(1) |> Exp.fresh),
    copied: false,
  };
  let unop_menhir = () =>
    alco_check_menhir("Unary operation test (menhir)", unop_str, unop_uexp);

  let seq_str = "1; 2";
  let seq_uexp: Exp.t = {
    ids: [id_at(0)],
    term: Seq(Int(1) |> Exp.fresh, Int(2) |> Exp.fresh),
    copied: false,
  };
  let seq_menhir = () =>
    alco_check_menhir("Sequence test (menhir)", seq_str, seq_uexp);

  let fixf_str = "fix x -> 1<Int => Unknown Internal>";
  let fixf_uexp: Exp.t = {
    ids: [id_at(0)],
    term: FixF(Var("x") |> Pat.fresh, Int(1) |> Exp.fresh, None),
    copied: false,
  };
  let fixf_menhir = () =>
    alco_check_menhir("FixF test (menhir)", fixf_str, fixf_uexp);

  let tests = [
    test_case("Filter test (menhir)", `Quick, filter_menhir),
    test_case("Test failed (menhir)", `Quick, test_menhir),
    test_case("Built-in function (menhir)", `Quick, builtin_fun_menhir),
    test_case(
      "Dynamic error hole (menhir)",
      `Quick,
      dynamic_error_hole_menhir,
    ),
    test_case("Constructor test (menhir)", `Quick, constructor_menhir),
    test_case("Failed cast test (menhir)", `Quick, failed_cast_menhir),
    test_case("Type ap test (menhir)", `Quick, typ_ap_menhir),
    test_case("Let expression for a tuple (menhir)", `Quick, let_exp_menhir),
    test_case("Single integer (menhir)", `Quick, single_integer_menhir),
    test_case(
      "Let expression for a function (menhir)",
      `Quick,
      let_fun_menhir,
    ),
    test_case("Empty hole (menhir)", `Quick, empty_hole_menhir),
    test_case("Free var (menhir)", `Quick, free_var_menhir),
    test_case("Bin op (menhir)", `Quick, bin_op_menhir),
    test_case("Inconsistent case (menhir)", `Quick, inconsistent_case_menhir),
    test_case("ap fun (menhir)", `Quick, ap_fun_menhir),
    test_case("Consistent if (menhir)", `Quick, consistent_if_menhir),
    //NOTE: left out until deferral elaborations are fixed
    // test_case("Deffered Ap Test (menhir)", `Quick, deferred_ap_menhir),
    test_case("Undefined test (menhir)", `Quick, undefined_menhir),
    test_case("List exp (menhir)", `Quick, list_exp_menhir),
    test_case("Invalid test (menhir)", `Quick, invalid_menhir),
    test_case("Type alias test (menhir)", `Quick, ty_alias_menhir),
    test_case("List concat test (menhir)", `Quick, list_concat_menhir),
    test_case("Unary operation test (menhir)", `Quick, unop_menhir),
    test_case("Sequence test (menhir)", `Quick, seq_menhir),
    test_case("FixF test (menhir)", `Quick, fixf_menhir),
  ];
};

let tests = [("Menhir elaboration tests", MenhirElaborationTests.tests)];

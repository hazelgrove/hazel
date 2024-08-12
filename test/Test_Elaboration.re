open Alcotest;
open Haz3lcore;

/*Create a testable type for dhexp which requires
  an equal function (dhexp_eq) and a print function (dhexp_print) */
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);

//Starts at 0
let get_id_menhir_closure = (id_index: int) => {
  let id_index = ref(id_index - 1);
  (inc: bool) => {
    print_endline(
      "get id inc: "
      ++ string_of_bool(inc)
      ++ " "
      ++ string_of_int(id_index^),
    );
    if (inc) {
      id_index := id_index^ + 1;
    };
    id_at(id_index^);
  };
};

let mk_map = CoreSettings.on |> Interface.Statics.mk_map;
let dhexp_of_uexp = u => Elaborator.elaborate(mk_map(u), u) |> fst;
let alco_check = dhexp_typ |> Alcotest.check;

let alco_check_menhir = (name: string, dhexp: string, uexp: Term.Exp.t) =>
  alco_check(
    name,
    Haz3lcore.TermBase.Exp.of_menhir_ast(
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
    Haz3lcore.TermBase.Exp.of_menhir_ast(
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
    Haz3lcore.DHExp.of_menhir_ast(
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
    Haz3lcore.DHExp.of_menhir_ast(
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
    Haz3lcore.DHExp.of_menhir_ast(
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
print_endline("SEXP seiralization:");
InvalidOperationError.DivideByZero
|> InvalidOperationError.sexp_of_t
|> Sexplib.Sexp.to_string
|> print_endline;

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

let elaboration_tests = [
  test_case("Dynamic error hole (menhir)", `Quick, dynamic_error_hole_menhir),
  test_case("Constructor test (menhir)", `Quick, constructor_menhir),
  test_case("Failed cast test (menhir)", `Quick, failed_cast_menhir),
  test_case("Type ap test (menhir)", `Quick, typ_ap_menhir),
  test_case("Let expression for a tuple (menhir)", `Quick, let_exp_menhir),
  test_case("Single integer (menhir)", `Quick, single_integer_menhir),
  test_case("Let expression for a function (menhir)", `Quick, let_fun_menhir),
  test_case("Empty hole (menhir)", `Quick, empty_hole_menhir),
  test_case("Free var (menhir)", `Quick, free_var_menhir),
  test_case("Bin op (menhir)", `Quick, bin_op_menhir),
  test_case("Inconsistent case (menhir)", `Quick, inconsistent_case_menhir),
  test_case("ap fun (menhir)", `Quick, ap_fun_menhir),
  test_case("Consistent if (menhir)", `Quick, consistent_if_menhir),
];

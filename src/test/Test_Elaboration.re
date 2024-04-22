open Alcotest;
open Haz3lcore;
// open Hazel_menhir;

let dhexp_eq = (d1: option(DHExp.t), d2: option(DHExp.t)): bool =>
  switch (d1, d2) {
  | (Some(d1), Some(d2)) => DHExp.fast_equal(d1, d2)
  | _ => false
  };

let dhexp_print = (d: option(DHExp.t)): string =>
  switch (d) {
  | None => "None"
  | Some(d) => DHExp.show(d)
  };

/*Create a testable type for dhexp which requires
  an equal function (dhexp_eq) and a print function (dhexp_print) */
let dhexp_typ = testable(Fmt.using(dhexp_print, Fmt.string), dhexp_eq);

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
let dhexp_of_uexp = u => Elaborator.dhexp_of_uexp(mk_map(u), u, false);
let alco_check = dhexp_typ |> Alcotest.check;

let alco_check_menhir = (name: string, dhexp: string, uexp: Term.UExp.t) =>
  alco_check(
    name,
    Some(
      Haz3lcore.DHExp.of_menhir_ast(
        Hazel_menhir.Interface.parse_program(dhexp),
        get_id_menhir_closure(0),
      ),
    ),
    dhexp_of_uexp(uexp),
  );

//Test for a let function
let let_fun_uexp: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Let(
      {
        ids: [id_at(1)],
        term:
          TypeAnn(
            {ids: [id_at(2)], term: Var("f")},
            {
              ids: [id_at(3)],
              term:
                Arrow(
                  {ids: [id_at(4)], term: Int},
                  {ids: [id_at(5)], term: Int},
                ),
            },
          ),
      },
      {
        ids: [id_at(6)],
        term:
          Fun(
            {ids: [id_at(7)], term: Var("x")},
            {
              ids: [id_at(8)],
              term:
                BinOp(
                  Int(Plus),
                  {ids: [id_at(9)], term: Int(1)},
                  {ids: [id_at(10)], term: Var("x")},
                ),
            },
          ),
      },
      {ids: [id_at(11)], term: Int(55)},
    ),
};
let let_fun_str = "
let f : Int -> Int =
    _FIX f Int -> Int fun: Int x ->
        1 + x
    f
    in
55";
let let_fun_menhir = () =>
  alco_check_menhir(
    "Let expression for function (str elaborated using the menhir parser)",
    let_fun_str,
    let_fun_uexp,
  );

//Test for an empty hole
let empty_hole_str = "()";
let empty_hole_uexp: Term.UExp.t = {ids: [id_at(0)], term: EmptyHole};
let empty_hole_menhir = () =>
  alco_check_menhir(
    "Empty hole (str elaborated using the menhir parser)",
    empty_hole_str,
    empty_hole_uexp,
  );

//Test for a free variable
let free_var_uexp: Term.UExp.t = {
  ids: [id_at(0)],
  term: Parens({ids: [id_at(1)], term: Var("y")}),
};
let free_var_menhir = () =>
  alco_check(
    "Nonempty hole with free variable (menhir)",
    Some(
      Haz3lcore.DHExp.of_menhir_ast(
        Hazel_menhir.Interface.parse_program("(_HOLE _FREE y)"),
        get_id_menhir_closure(1),
      ),
    ),
    dhexp_of_uexp(free_var_uexp),
  );

//Menhir test for a binary operation
let bin_op_uexp: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    BinOp(
      Int(Plus),
      {ids: [id_at(1)], term: Bool(false)},
      {ids: [id_at(2)], term: Var("y")},
    ),
};
let bin_op_menhir = () =>
  alco_check(
    "Inconsistent binary integer operation (plus)",
    Some(
      Haz3lcore.DHExp.of_menhir_ast(
        Hazel_menhir.Interface.parse_program(
          "(_HOLE false) + (_HOLE _FREE y)",
        ),
        get_id_menhir_closure(1),
      ),
    ),
    dhexp_of_uexp(bin_op_uexp),
  );

//Inconsistent branches menhir test
let inconsistent_case_menhir_str = "
    (_HOLE (case 4 == 3
    | true => 24
    | false => false
    end) )
";
let inconsistent_case_uexp: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Match(
      {
        ids: [id_at(1)],
        term:
          BinOp(
            Int(Equals),
            {ids: [id_at(2)], term: Int(4)},
            {ids: [id_at(3)], term: Int(3)},
          ),
      },
      [
        (
          {ids: [id_at(6)], term: Bool(true)},
          {ids: [id_at(4)], term: Int(24)},
        ),
        (
          {ids: [id_at(7)], term: Bool(false)},
          {ids: [id_at(5)], term: Bool(false)},
        ),
      ],
    ),
};
let inconsistent_case_menhir = () =>
  alco_check_menhir(
    "Inconsistent branches where the first branch is an integer and second branch is a boolean (menhir)",
    inconsistent_case_menhir_str,
    inconsistent_case_uexp,
  );

//Function free var application menhir test
let ap_fun_uexp: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Ap(
      {
        ids: [id_at(1)],
        term:
          Fun(
            {ids: [id_at(2)], term: Var("x")},
            {
              ids: [id_at(3)],
              term:
                BinOp(
                  Int(Plus),
                  {ids: [id_at(4)], term: Int(4)},
                  {ids: [id_at(5)], term: Var("x")},
                ),
            },
          ),
      },
      {ids: [id_at(6)], term: Var("y")},
    ),
};
let ap_fun_str = "
    (fun: Unknown Internal x -> 4 + x <Unknown Internal => Int>)((_HOLE _FREE y))
";
let ap_fun_menhir = () =>
  alco_check(
    "Application of a function of a free variable wrapped inside a nonempty hole constructor (menhir)",
    Some(
      Haz3lcore.DHExp.of_menhir_ast(
        Hazel_menhir.Interface.parse_program(ap_fun_str),
        get_id_menhir_closure(6),
      ),
    ),
    dhexp_of_uexp(ap_fun_uexp),
  );

//Consistent if statement menhir test
let consistent_if_uexp: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    If(
      {ids: [id_at(1)], term: Bool(false)},
      {ids: [id_at(2)], term: Int(8)},
      {ids: [id_at(3)], term: Int(6)},
    ),
};
let consistent_if_str = "
    if false then 8 else 6
";
let consistent_if_menhir = () =>
  alco_check(
    "Consistent case with rules (BoolLit(true), IntLit(8)) and (BoolLit(false), IntLit(6))",
    Some(
      Haz3lcore.DHExp.of_menhir_ast(
        Hazel_menhir.Interface.parse_program(consistent_if_str),
        get_id_menhir_closure(6),
      ),
    ),
    dhexp_of_uexp(consistent_if_uexp),
  );

//Single integer menhir test
let single_int_str = "8";
let single_int_uexp: Term.UExp.t = {ids: [id_at(0)], term: Int(8)};
let single_integer_menhir = () =>
  alco_check_menhir(
    "Single integer test (menhir)",
    single_int_str,
    single_int_uexp,
  );

//Menhir let expression test
let let_exp_str = "let (a, b) = (4, 6) in a - b";
let let_exp_uexp: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Let(
      {
        ids: [id_at(1)],
        term:
          Tuple([
            {ids: [id_at(2)], term: Var("a")},
            {ids: [id_at(3)], term: Var("b")},
          ]),
      },
      {
        ids: [id_at(4)],
        term:
          Tuple([
            {ids: [id_at(5)], term: Int(4)},
            {ids: [id_at(6)], term: Int(6)},
          ]),
      },
      {
        ids: [id_at(7)],
        term:
          BinOp(
            Int(Minus),
            {ids: [id_at(8)], term: Var("a")},
            {ids: [id_at(9)], term: Var("b")},
          ),
      },
    ),
};
let let_exp_menhir = () =>
  alco_check_menhir(
    "Let expression for tuple (a, b) (menhir)",
    let_exp_str,
    let_exp_uexp,
  );

let elaboration_tests = [
  test_case("Let expression (menhir)", `Quick, let_exp_menhir),
  test_case("Single integer (menhir)", `Quick, single_integer_menhir),
  test_case("Let expression for a function (menhir)", `Quick, let_fun_menhir),
  test_case("Empty hole (menhir)", `Quick, empty_hole_menhir),
  test_case("Free var (menhir)", `Quick, free_var_menhir),
  test_case("Bin op (menhir)", `Quick, bin_op_menhir),
  test_case("Inconsistent case (menhir)", `Quick, inconsistent_case_menhir),
  test_case("ap fun (menhir)", `Quick, ap_fun_menhir),
  test_case("Consistent if (menhir)", `Quick, consistent_if_menhir),
];

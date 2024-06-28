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

// let alco_check_menhir = (name: string, dhexp: string, uexp: Term.Exp.t) =>
//   alco_check(
//     name,
//     Some(
//       Haz3lcore.TermBase.Exp.of_menhir_ast(
//         Haz3lmenhir.Interface.parse_program(dhexp),
//         get_id_menhir_closure(0),
//       ),
//     ),
//     dhexp_of_uexp(uexp),
//   );
//
// //Test for a let function
// let let_fun_uexp: Term.UExp.t = {
//   ids: [id_at(0)],
//   term:
//     Let(
//       {
//         ids: [id_at(1)],
//         term:
//           TypeAnn(
//             {ids: [id_at(2)], term: Var("f")},
//             {
//               ids: [id_at(3)],
//               term:
//                 Arrow(
//                   {ids: [id_at(4)], term: Int},
//                   {ids: [id_at(5)], term: Int},
//                 ),
//             },
//           ),
//       },
//       {
//         ids: [id_at(6)],
//         term:
//           Fun(
//             {ids: [id_at(7)], term: Var("x")},
//             {
//               ids: [id_at(8)],
//               term:
//                 BinOp(
//                   Int(Plus),
//                   {ids: [id_at(9)], term: Int(1)},
//                   {ids: [id_at(10)], term: Var("x")},
//                 ),
//             },
//           ),
//       },
//       {ids: [id_at(11)], term: Int(55)},
//     ),
// };
// let let_fun_str = "
// let f : Int -> Int =
//     fix (f: Int -> Int) -> fun: Int x ->
//         1 + x
//     f
//     in
// 55";
// let let_fun_menhir = () =>
//   alco_check_menhir(
//     "Let expression for a function which wraps a fix point constructor around the function",
//     let_fun_str,
//     let_fun_uexp,
//   );
//
// //Test for an empty hole
// let empty_hole_str = "?";
// let empty_hole_uexp: Term.UExp.t = {ids: [id_at(0)], term: EmptyHole};
// let empty_hole_menhir = () =>
//   alco_check_menhir(
//     "Empty hole (str elaborated using the menhir parser)",
//     empty_hole_str,
//     empty_hole_uexp,
//   );
//
// //Test for a free variable
// let free_var_uexp: Term.UExp.t = {
//   ids: [id_at(0)],
//   term: Parens({ids: [id_at(1)], term: Var("y")}),
// };
// let free_var_menhir = () =>
//   alco_check(
//     "Nonempty hole with free variable (menhir)",
//     Some(
//       Haz3lcore.DHExp.of_menhir_ast(
//         Haz3lmenhir.Interface.parse_program("{{?y}}"),
//         get_id_menhir_closure(1),
//       ),
//     ),
//     dhexp_of_uexp(free_var_uexp),
//   );
//
// //Menhir test for a binary operation
// let bin_op_uexp: Term.UExp.t = {
//   ids: [id_at(0)],
//   term:
//     BinOp(
//       Int(Plus),
//       {ids: [id_at(1)], term: Bool(false)},
//       {ids: [id_at(2)], term: Var("y")},
//     ),
// };
// let bin_op_menhir = () =>
//   alco_check(
//     "Inconsistent binary integer operation (plus)",
//     Some(
//       Haz3lcore.DHExp.of_menhir_ast(
//         Haz3lmenhir.Interface.parse_program("{{false}} + {{? y}}"),
//         get_id_menhir_closure(1),
//       ),
//     ),
//     dhexp_of_uexp(bin_op_uexp),
//   );
//
// //Inconsistent branches menhir test
// let inconsistent_case_menhir_str = "
//     {{?case 4 == 3
//     | true => 24
//     | false => false
//     end}}
// ";
// let inconsistent_case_uexp: Term.UExp.t = {
//   ids: [id_at(0)],
//   term:
//     Match(
//       {
//         ids: [id_at(1)],
//         term:
//           BinOp(
//             Int(Equals),
//             {ids: [id_at(2)], term: Int(4)},
//             {ids: [id_at(3)], term: Int(3)},
//           ),
//       },
//       [
//         (
//           {ids: [id_at(6)], term: Bool(true)},
//           {ids: [id_at(4)], term: Int(24)},
//         ),
//         (
//           {ids: [id_at(7)], term: Bool(false)},
//           {ids: [id_at(5)], term: Bool(false)},
//         ),
//       ],
//     ),
// };
// let inconsistent_case_menhir = () =>
//   alco_check_menhir(
//     "Inconsistent branches where the first branch is an integer and second branch is a boolean (menhir)",
//     inconsistent_case_menhir_str,
//     inconsistent_case_uexp,
//   );
//
// //Function free var application menhir test
// let ap_fun_uexp: Term.UExp.t = {
//   ids: [id_at(0)],
//   term:
//     Ap(
//       {
//         ids: [id_at(1)],
//         term:
//           Fun(
//             {ids: [id_at(2)], term: Var("x")},
//             {
//               ids: [id_at(3)],
//               term:
//                 BinOp(
//                   Int(Plus),
//                   {ids: [id_at(4)], term: Int(4)},
//                   {ids: [id_at(5)], term: Var("x")},
//                 ),
//             },
//           ),
//       },
//       {ids: [id_at(6)], term: Var("y")},
//     ),
// };
// let ap_fun_str = "
//     (fun: Unknown Internal x -> 4 + x <Unknown Internal => Int>)({{? y}})
// ";
// let ap_fun_menhir = () =>
//   alco_check(
//     "Application of a function of a free variable wrapped inside a nonempty hole constructor (menhir)",
//     Some(
//       Haz3lcore.DHExp.of_menhir_ast(
//         Haz3lmenhir.Interface.parse_program(ap_fun_str),
//         get_id_menhir_closure(6),
//       ),
//     ),
//     dhexp_of_uexp(ap_fun_uexp),
//   );
//
// //Consistent if statement menhir test
// let consistent_if_uexp: Term.UExp.t = {
//   ids: [id_at(0)],
//   term:
//     If(
//       {ids: [id_at(1)], term: Bool(false)},
//       {ids: [id_at(2)], term: Int(8)},
//       {ids: [id_at(3)], term: Int(6)},
//     ),
// };
// let consistent_if_str = "
//     if false then 8 else 6
// ";
// let consistent_if_menhir = () =>
//   alco_check(
//     "Consistent case with rules (BoolLit(true), IntLit(8)) and (BoolLit(false), IntLit(6))",
//     Some(
//       Haz3lcore.DHExp.of_menhir_ast(
//         Haz3lmenhir.Interface.parse_program(consistent_if_str),
//         get_id_menhir_closure(6),
//       ),
//     ),
//     dhexp_of_uexp(consistent_if_uexp),
//   );
//
// //Single integer menhir test
// let single_int_str = "8";
// let single_int_uexp: Term.UExp.t = {ids: [id_at(0)], term: Int(8)};
// let single_integer_menhir = () =>
//   alco_check_menhir(
//     "Single integer test (menhir)",
//     single_int_str,
//     single_int_uexp,
//   );
//
// //Menhir let expression test
// let let_exp_str = "let (a, b) = (4, 6) in a - b";
// let let_exp_uexp: Term.UExp.t = {
//   ids: [id_at(0)],
//   term:
//     Let(
//       {
//         ids: [id_at(1)],
//         term:
//           Tuple([
//             {ids: [id_at(2)], term: Var("a")},
//             {ids: [id_at(3)], term: Var("b")},
//           ]),
//       },
//       {
//         ids: [id_at(4)],
//         term:
//           Tuple([
//             {ids: [id_at(5)], term: Int(4)},
//             {ids: [id_at(6)], term: Int(6)},
//           ]),
//       },
//       {
//         ids: [id_at(7)],
//         term:
//           BinOp(
//             Int(Minus),
//             {ids: [id_at(8)], term: Var("a")},
//             {ids: [id_at(9)], term: Var("b")},
//           ),
//       },
//     ),
// };
// let let_exp_menhir = () =>
//   alco_check_menhir(
//     "Let expression for tuple (a, b) (menhir)",
//     let_exp_str,
//     let_exp_uexp,
//   );
//
// let elaboration_tests = [
//   test_case("Let expression (menhir)", `Quick, let_exp_menhir),
//   test_case("Single integer (menhir)", `Quick, single_integer_menhir),
//   test_case("Let expression for a function (menhir)", `Quick, let_fun_menhir),
//   test_case("Empty hole (menhir)", `Quick, empty_hole_menhir),
//   test_case("Free var (menhir)", `Quick, free_var_menhir),
//   test_case("Bin op (menhir)", `Quick, bin_op_menhir),
//   test_case("Inconsistent case (menhir)", `Quick, inconsistent_case_menhir),
//   test_case("ap fun (menhir)", `Quick, ap_fun_menhir),
//   test_case("Consistent if (menhir)", `Quick, consistent_if_menhir),
// ];
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

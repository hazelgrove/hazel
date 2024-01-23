open Alcotest;
open Js_of_ocaml;
open Haz3lcore;

let dhexp_eq = (d1: option(DHExp.t), d2: option(DHExp.t)): bool =>
  switch (d1, d2) {
  | (Some(d1), Some(d2)) => DHExp.fast_equal(d1, d2)
  | _ => false
  };

let dhexp_typ = testable(Fmt.nop, dhexp_eq);

let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);
let mk_map = CoreSettings.on |> Interface.Statics.mk_map;
let dhexp_of_uexp = Elaborator.dhexp_of_uexp;
let alco_check = dhexp_typ |> Alcotest.check;

let u1: Term.UExp.t = {ids: [id_at(0)], term: Int(8)};
let single_integer = () =>
  alco_check(
    "Integer literal 8",
    Some(IntLit(8)),
    dhexp_of_uexp(mk_map(u1), u1),
  );

let u2: Term.UExp.t = {ids: [id_at(0)], term: EmptyHole};
let empty_hole = () =>
  alco_check(
    "Empty hole",
    Some(EmptyHole(id_at(0), 0)),
    dhexp_of_uexp(mk_map(u2), u2),
  );

let u3: Term.UExp.t = {
  ids: [id_at(0)],
  term: Parens({ids: [id_at(1)], term: Var("y")}),
};
let d3: DHExp.t =
  NonEmptyHole(TypeInconsistent, id_at(1), 0, FreeVar(id_at(1), 0, "y"));
let free_var = () =>
  alco_check(
    "Nonempty hole with free variable",
    Some(d3),
    dhexp_of_uexp(mk_map(u3), u3),
  );

let u4: Term.UExp.t = {
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
let d4: DHExp.t =
  Let(
    Tuple([Var("a"), Var("b")]),
    Tuple([IntLit(4), IntLit(6)]),
    BinIntOp(Minus, BoundVar("a"), BoundVar("b")),
  );
let let_exp = () =>
  alco_check(
    "Let expression for tuple (a, b)",
    Some(d4),
    dhexp_of_uexp(mk_map(u4), u4),
  );

let main = (_s: string) => {
  run(
    "Dynamics",
    [
      (
        "Elaboration",
        [
          test_case("Single integer", `Quick, single_integer),
          test_case("Empty hole", `Quick, empty_hole),
          test_case("Free variable", `Quick, free_var),
          test_case("Let expression", `Quick, let_exp),
        ],
      ),
    ],
  );
};

Js.Unsafe.js_expr("require('process')");
Js.Unsafe.js_expr("process.argv[2]") |> Js.to_string |> main;

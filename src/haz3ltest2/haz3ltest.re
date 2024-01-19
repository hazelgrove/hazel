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

let u1: Term.UExp.t = {ids: [id_at(0)], term: Int(8)};
let single_integer = () =>
  Alcotest.check(
    dhexp_typ,
    "idk",
    Some(IntLit(8)),
    Elaborator.dhexp_of_uexp(
      Interface.Statics.mk_map(CoreSettings.on, u1),
      u1,
    ),
  );

let main = (_s: string) => {
  run(
    "Elaboration",
    [("single integer", [test_case("yabo", `Quick, single_integer)])],
  );
};

Js.Unsafe.js_expr("require('process')");
Js.Unsafe.js_expr("process.argv[2]") |> Js.to_string |> main;

open Js_of_ocaml;
open Util;
open OptUtil.Syntax;
open Haz3lcore;

let mk_map = CoreSettings.on |> Interface.Statics.mk_map;
let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);
let dhexp_of_uexp = u => Elaborator.dhexp_of_uexp(mk_map(u), u, false);

/* UExp of 1 + 1 */
let u: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    BinOp(
      Int(Plus),
      {ids: [id_at(1)], term: Int(1)},
      {ids: [id_at(2)], term: Int(1)},
    ),
};

/* Measures time from UExp to final DExp */
let measure_time_uexp = (u: Term.UExp.t) => {
  print_endline(
    "Measuring time to evaluate UExp \n\n\t" ++ Term.UExp.show(u) ++ "\n",
  );
  let env = Builtins.env_init;
  let start_time = Js.Unsafe.global##.performance##now();
  let* d = dhexp_of_uexp(u);
  let (_, result) = Evaluator.evaluate(env, d);
  let end_time = Js.Unsafe.global##.performance##now();

  print_endline(
    "Final DHExp: \n\n\t" ++ EvaluatorResult.show(result) ++ "\n",
  );

  Some(end_time -. start_time); /* in milliseconds */
};

let print_time = t =>
  print_endline("Elapsed Time: " ++ string_of_float(t) ++ " ms");

let time_str = measure_time_uexp(u);
switch (time_str) {
| Some(t) => print_time(t)
| None => print_endline("Cannot get elapsed time.")
};

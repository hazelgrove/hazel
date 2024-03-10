open Js_of_ocaml;
open Haz3lcore;
open Util;
open OptUtil.Syntax;

let mk_map = CoreSettings.on |> Interface.Statics.mk_map;
let dhexp_of_uexp = (u: Term.UExp.t) =>
  Elaborator.dhexp_of_uexp(mk_map(u), u, false);
let uexp_of_string = (s: string) => {
  let* z = Printer.zipper_of_string(s);
  let u = Zipper.zip(z) |> MakeTerm.go |> fst;
  Some(u);
};

let measure_uexp_to_dhexp = (s: string) => {
  print_endline("Measuring time to evaluate UExp of \n\n\t" ++ s ++ "\n");
  let env = Builtins.env_init;
  /* Create UExp before benchmarking time */
  let* u = uexp_of_string(s);
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

let measure_time = (s: string) => {
  switch (measure_uexp_to_dhexp(s)) {
  | Some(t) => print_time(t)
  | None => print_endline("Cannot get elapsed time.")
  };
};

/* With CLI */
/* Js.Unsafe.js_expr("require('process')");
   Js.Unsafe.js_expr("process.argv[2]") |> Js.to_string |> measure_time; */

/* Without CLI */
let sample_program = "1 + 1";
measure_time(sample_program);

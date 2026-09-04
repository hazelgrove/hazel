/* SPIKE (wasm-eval-bench): evaluator-only benchmark, js_of_ocaml vs
   wasm_of_ocaml.

   Mirrors the timing structure of `hazel bench-eval` (see src/CLI/Cli.re),
   but reaches the evaluator without touching haz3lcore, so that the same
   source builds under both backends. Parse and statics run once and are not
   timed; only evaluation is.

   Output is the JSON shape bench/compare.js already consumes:
     [{"name": "<file>/<scenario>/eval", "time_ns": <float>}, ...] */

open Language;
open Util;

/* Fixed-precision workloads only. Hazel's Int and Nat are Bigint-backed,
   and bignum is exactly what cannot be compiled to Wasm here, so these use
   SInt (machine int) and Float. The three have deliberately different
   profiles: recursion, allocation-light arithmetic, allocation-heavy list
   work -- the last matters because wasm_of_ocaml hands the OCaml heap to
   the browser's collector. */
let workloads = [
  ("sint-fib", [%blob "fixed-sint-fib.hz"]),
  ("float-numeric", [%blob "fixed-float-numeric.hz"]),
  ("sint-list", [%blob "fixed-sint-list.hz"]),
];

let now_ms = (): float =>
  Js_of_ocaml.Js.Unsafe.global##.performance##now()##valueOf
  |> Js_of_ocaml.Js.float_of_number;

let parse = (text: string): Exp.t =>
  Grammar.map_exp_annotation(
    _ => IdTagged.IdTag.fresh(),
    MenhirParser.Conversion.Exp.of_menhir_ast(
      MenhirParser.Interface.parse_program(text),
    ),
  );

let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
  Statics.mk(
    CoreSettings.on,
    Builtins.ctx_init(Some(Operators.default_mode)),
    exp,
  );

let evaluate_elab = (elab: Exp.t): Exp.t =>
  fst(Evaluator.evaluate(~env=Builtins.env_init, elab));

let evaluate_elab_incr = (~eval_info: EvalInfo.t, elab: Exp.t): Exp.t =>
  fst(Evaluator.evaluate(~eval_info, ~env=Builtins.env_init, elab));

/* Time [f] over [iterations] runs after one warm-up, in ns per iteration. */
let time = (~iterations: int, f: unit => unit): float => {
  f();
  let t0 = now_ms();
  for (_ in 1 to iterations) {
    f();
  };
  let t1 = now_ms();
  (t1 -. t0) /. float_of_int(iterations) *. 1_000_000.;
};

let () = {
  let iterations =
    switch (Sys.argv) {
    | [|_, n|] => int_of_string(n)
    | _ => 20
    };

  let results =
    List.concat_map(
      ((name, source)) => {
        let exp = parse(source);
        let (info_map, elab) = statics_and_elab(exp);
        let eval_info =
          EvalInfo.of_info_map(
            ~probe_all=CoreSettings.on.probe_all,
            ~targets=Id.Map.empty,
            info_map,
          );
        /* Correctness guard: a backend that silently got stuck (say, on a
           Bigint stub) would look fast and mean nothing. Emit the evaluated
           result so the two backends can be diffed, not just timed. */
        let result = Exp.show(evaluate_elab(elab));
        let plain = time(~iterations, () => ignore(evaluate_elab(elab)));
        let incr =
          time(~iterations, () =>
            ignore(evaluate_elab_incr(~eval_info, elab))
          );
        [
          Printf.sprintf(
            {|{"name":"%s/plain/eval","time_ns":%.0f}|},
            name,
            plain,
          ),
          Printf.sprintf(
            {|{"name":"%s/incr/eval","time_ns":%.0f}|},
            name,
            incr,
          ),
          Printf.sprintf(
            {|{"name":"%s/result","sha":"%08x"}|},
            name,
            Hashtbl.hash(result) land 0xFFFFFFFF,
          ),
        ];
      },
      workloads,
    );

  print_endline("[" ++ String.concat(",", results) ++ "]");
};

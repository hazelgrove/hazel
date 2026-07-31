open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Coarse evaluator benchmarks for the observation-trace work
 * (plans/observation-trace.md §8). Compare the SAME case across commits
 * (this file uses only the evaluate API, so it compiles on pre-trace
 * commits too). Three regimes: probes off, a probed recursion, and
 * probe-all (every expression targeted — worst case).
 *
 * NOT comparable across cases: each regime runs a different fib size and
 * rep count, chosen so wall times land in the same few-hundred-ms range.
 * Per-CALL cost rises steeply with observation (approx fib call counts:
 * fib(16)=3193, fib(14)=1219, fib(12)=465): probes-off ≈ 42 µs/call,
 * one-probe ≈ 71 µs/call, probe-all ≈ 140 µs/call (2026-07-31 numbers).
 * The per-eval figure printed below is the cross-commit metric. */

let fib = n =>
  Printf.sprintf(
    {|let fib = fun n -> if n < 2 then n else fib(n - 1) + fib(n - 2)
in fib(%d)|},
    n,
  );

let fib_probed = n =>
  Printf.sprintf(
    {|let fib = fun n -> if n < 2 then n else ^^probe(fib(n - 1)) + fib(n - 2)
in fib(%d)|},
    n,
  );

let all_exp_targets = (info_map: Statics.Map.t): Sample.targets =>
  Id.Map.filter_map(
    (_id, info) =>
      switch (info) {
      | Info.InfoExp(_) => Some(Sample.empty_capture_spec)
      | _ => None
      },
    info_map,
  );

let eval_n = (~probe_all=false, ~reps: int, code: string): unit => {
  let (_term, elaborated, info_map, targets) = parse_with_probes(code);
  let targets = probe_all ? all_exp_targets(info_map) : targets;
  for (_ in 1 to reps) {
    ignore(
      Evaluator.evaluate(
        ~eval_info=EvalInfo.of_targets(targets),
        ~env=Builtins.env_init,
        elaborated,
      ),
    );
  };
};

let timed = (~reps: int, label: string, f: unit => unit): unit => {
  let t0 = Sys.time();
  f();
  let ms = (Sys.time() -. t0) *. 1000.;
  Printf.printf(
    "BENCH %s: %.1f ms (%.1f ms/eval; cases are NOT cross-comparable)\n",
    label,
    ms,
    ms /. float_of_int(reps),
  );
};

let tests = (
  "ObsBench",
  [
    test_case("bench: fib(16) probes-off x10", `Quick, () =>
      timed(~reps=10, "fib16-off-x10", () => eval_n(~reps=10, fib(16)))
    ),
    test_case("bench: fib(14) one-probe x10", `Quick, () =>
      timed(~reps=10, "fib14-probed-x10", () =>
        eval_n(~reps=10, fib_probed(14))
      )
    ),
    test_case("bench: fib(12) probe-all x3", `Quick, () =>
      timed(~reps=3, "fib12-all-x3", () =>
        eval_n(~probe_all=true, ~reps=3, fib(12))
      )
    ),
  ],
);

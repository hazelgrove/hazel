/* Standardness completion, item 4: the cost of the closed-evaluation
   discharge channel on `!!(1 / 0)` (recD: ~40.65 s for this one goal).
   TEST-LOCAL instrumentation only: the evaluator's injected step function
   (Evaluator.re, `check_theorem_proof`) is rebuilt here verbatim around a
   counter. Batch b1 (before std-c2) measured `!!(1 / 0)` and `1 / 0 == 1`
   at 1000 steps / ~28 s each, none structurally equal to its input: each
   re-entry nests one more dynamic-error hole around the same payload.
   std-c2 stops at the first dynamic error. Each case here runs the OLD
   loop (a test-local replica) and the production loop on the same goal
   and requires the SAME discharge verdict; the error cases also require
   the production loop to stop early. `error_then_discarded_arg` is the
   discriminating control: its expected `false` under the OLD loop is a
   prediction from the nesting mechanism, measured here. */
open Alcotest;
open Language;

module RE = Test_ResetExistentials;

let elab = (src: string): Exp.t => {
  let (_, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      Test_Evaluator_Prelude.parse_exp(src),
    );
  elab;
};

/* Verbatim copy of the evaluator's step callback. */
let real_step: ProofCheck.step_fn =
  (~env, e) => {
    let advance = (~env, e) =>
      switch (Evaluator.advance_hidden(~env, e)) {
      | Some(advanced) => advanced
      | None => ([], e)
      };
    let (leading, e) = advance(~env, e);
    switch (EvaluatorStep.get_status(~settings=CoreSettings.on, e, env)) {
    | AvailableSteps([s, ..._]) =>
      switch (EvaluatorStep.take_step(s)) {
      | Some(next) =>
        let (trailing, outgoing) = advance(~env, next);
        Some(
          ProofCheck.{
            auto_incoming: List.map(((_, j, o)) => (j, o), leading),
            auto_outgoing: List.map(((i, j, _)) => (i, j), trailing),
            outgoing,
          },
        );
      | None => None
      }
    | AutoStep(_)
    | AvailableSteps([]) => None
    };
  };

type trace = {
  steps: int,
  no_progress: int,
  cpu: float,
  result: string,
};

/* TEST-LOCAL replica of the pre-std-c2 loop (no early stop), so the
   production loop's verdict can be compared with the old one on the same
   goal and the same real step function. */
let rec old_loop = (~step: ProofCheck.step_fn, ~env, ~fuel, e: Exp.t): Exp.t =>
  if (fuel <= 0) {
    e;
  } else {
    switch (step(~env, e)) {
    | Some({outgoing, _}) => old_loop(~step, ~env, ~fuel=fuel - 1, outgoing)
    | None => e
    };
  };

let measure = (~old: bool, goal: Exp.t): trace => {
  let steps = ref(0);
  let no_progress = ref(0);
  let counting: ProofCheck.step_fn =
    (~env, e) => {
      let r = real_step(~env, e);
      incr(steps);
      switch (r) {
      | Some({outgoing, _}) when Exp.fast_equal(outgoing, e) =>
        incr(no_progress)
      | _ => ()
      };
      r;
    };
  let t0 = Sys.time();
  let run = old ? old_loop : ProofCheck.eval_via_step;
  let result =
    run(
      ~step=counting,
      ~env=Builtins.env_init,
      ~fuel=ProofCheck.closed_eval_fuel,
      goal,
    );
  {
    steps: steps^,
    no_progress: no_progress^,
    cpu: Sys.time() -. t0,
    result: RE.print_exp(result),
  };
};

let line = (label, which, src, t) =>
  print_endline(
    Printf.sprintf(
      "EVALCOST %s | %s | %s | steps %d | no-progress %d | cpu %.3fs | result %s",
      label,
      which,
      src,
      t.steps,
      t.no_progress,
      t.cpu,
      String.length(t.result) > 200 ? String.sub(t.result, 0, 200) ++ "…" : t.result,
    ),
  );

/* Old loop vs production loop on one goal: the discharge verdict (literal
   `true` or not) must be identical; `~bounded` additionally requires the
   production loop to stop within a few steps. */
let compare = (~bounded=false, label, src, expected_true) =>
  test_case(
    label ++ ": old and production loops agree" ++ (bounded ? "; production bounded" : ""),
    `Quick,
    () => {
      let goal = elab(src);
      let o = measure(~old=true, goal);
      line(label, "old", src, o);
      let p = measure(~old=false, goal);
      line(label, "production", src, p);
      check(bool, label ++ ": old verdict", expected_true, o.result == "true");
      check(bool, label ++ ": production verdict", expected_true, p.result == "true");
      if (bounded) {
        check(bool, label ++ ": production stops within 5 steps", true, p.steps <= 5);
      };
    },
  );

let tests = (
  "ResetClosedEvalCost",
  [
    compare(~bounded=true, "partial", "!!(1 / 0)", false),
    compare(~bounded=true, "partial_eq", "1 / 0 == 1", false),
    compare(~bounded=true, "error_then_discarded_arg", "(fun (x : Int) -> true)(1 / 0)", false),
    compare(~bounded=true, "builtin_domain_error", "int_of_string(\"abc\") == 1", false),
    compare("untaken_partial_branch", "if false then 1 / 0 == 1 else true", true),
    compare("std_literal", "!!3", true),
    compare("std_tuple", "!!(1, [2])", true),
    compare("closed_arith", "2 * 3 == 6", true),
    compare("closed_div", "6 / 2 == 3", true),
    compare("closed_false", "2 * 3 == 7", false),
  ],
);

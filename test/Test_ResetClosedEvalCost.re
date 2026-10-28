/* Standardness completion, item 4: the cost of the closed-evaluation
   discharge channel on `!!(1 / 0)` (recD: ~40.65 s for this one goal).
   TEST-LOCAL instrumentation only: the evaluator's injected step function
   (Evaluator.re, `check_theorem_proof`) is rebuilt here verbatim around a
   counter, and `ProofCheck.eval_via_step` is run with the production fuel.
   Receipts per goal: steps taken, steps whose output is structurally equal
   to their input (no progress), the first such step, CPU, printed size of
   the first/last terms, and the final result. Positive closed evaluation
   is the control. Nothing here changes production code. */
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
  first_no_progress: option(int),
  cpu: float,
  first_size: int,
  last_size: int,
  result: string,
};

let measure = (goal: Exp.t): trace => {
  let steps = ref(0);
  let no_progress = ref(0);
  let first_np = ref(None);
  let last = ref(goal);
  let counting: ProofCheck.step_fn =
    (~env, e) => {
      let r = real_step(~env, e);
      incr(steps);
      switch (r) {
      | Some({outgoing, _}) =>
        if (Exp.fast_equal(outgoing, e)) {
          incr(no_progress);
          if (first_np^ == None) {
            first_np := Some(steps^);
          };
        };
        last := outgoing;
      | None => ()
      };
      r;
    };
  let t0 = Sys.time();
  let result =
    ProofCheck.eval_via_step(
      ~step=counting,
      ~env=Builtins.env_init,
      ~fuel=ProofCheck.closed_eval_fuel,
      goal,
    );
  let cpu = Sys.time() -. t0;
  {
    steps: steps^,
    no_progress: no_progress^,
    first_no_progress: first_np^,
    cpu,
    first_size: String.length(RE.print_exp(goal)),
    last_size: String.length(RE.print_exp(last^)),
    result: RE.print_exp(result),
  };
};

let receipt = (label, src) => {
  let goal = elab(src);
  let t = measure(goal);
  print_endline(
    Printf.sprintf(
      "EVALCOST %s | %s | steps %d | no-progress %d (first at %s) | cpu %.3fs | size %d -> %d | result %s",
      label,
      src,
      t.steps,
      t.no_progress,
      t.first_no_progress |> Option.map(string_of_int) |> Option.value(~default="-"),
      t.cpu,
      t.first_size,
      t.last_size,
      String.length(t.result) > 200 ? String.sub(t.result, 0, 200) ++ "…" : t.result,
    ),
  );
  t;
};

let tests = (
  "ResetClosedEvalCost",
  [
    test_case("measure !!(1 / 0): never evaluates to true", `Quick, () => {
      let t = receipt("partial", "!!(1 / 0)");
      check(bool, "not discharged", true, t.result != "true");
    }),
    test_case("measure 1 / 0 == 1 (closed, partial)", `Quick, () => {
      let t = receipt("partial_eq", "1 / 0 == 1");
      check(bool, "not discharged", true, t.result != "true");
    }),
    test_case("control !!3 evaluates to true", `Quick, () => {
      let t = receipt("std_literal", "!!3");
      check(string, "true", "true", t.result);
    }),
    test_case("control !!(1, [2]) evaluates to true", `Quick, () => {
      let t = receipt("std_tuple", "!!(1, [2])");
      check(string, "true", "true", t.result);
    }),
    test_case("control 2 * 3 == 6 evaluates to true", `Quick, () => {
      let t = receipt("closed_arith", "2 * 3 == 6");
      check(string, "true", "true", t.result);
    }),
    test_case("control 6 / 2 == 3 evaluates to true", `Quick, () => {
      let t = receipt("closed_div", "6 / 2 == 3");
      check(string, "true", "true", t.result);
    }),
  ],
);

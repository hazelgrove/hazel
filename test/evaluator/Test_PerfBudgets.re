open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Performance budgets: the work an evaluation does, counted and held to a
   budget, so that CI fails on a performance regression.

   The work is the steps the evaluator's trampoline actually takes
   (Evaluator.evaluate_counting_work). A count, not a time: it is the same
   on every machine and every run, so a budget can fail CI without false
   alarms. Wall time on GitHub's shared runners varies by 10-40% from run
   to run, too much to fail a build on. What a count cannot see is a step
   that got slower; timings are left to reports.

   Each budget is a ratchet. Over it is a regression: find it, or raise the
   budget with the reason in the commit. Well under it (by more than a
   quarter) means the budget is stale: lower it in the same change, which
   records the improvement. */

let statics_and_elab = Test_Evaluator_Incremental.statics_and_elab;
let replace_int_lit = Test_Evaluator_Incremental.replace_int_lit;

/* Evaluate as the eval worker does, with the incremental cache of a
   previous run if there is one: the result, the cache, and the work. */
let work =
    (~prev: EvaluatorState.incr_eval=IncrEval.empty, exp: Exp.t)
    : (Exp.t, EvaluatorState.incr_eval, int) => {
  let (info_map, elab) = statics_and_elab(exp);
  let eval_info =
    EvalInfo.of_info_map(
      ~probe_all=CoreSettings.on.probe_all,
      ~targets=Id.Map.empty,
      info_map,
    );
  let ((result, state), steps) =
    Evaluator.evaluate_counting_work(
      ~prev,
      ~eval_info,
      ~env=Builtins.env_init,
      elab,
    );
  (result, state.incr_eval, steps);
};

let within_budget = (name: string, ~budget: int, steps: int): unit => {
  /* in the test's log, so a run shows where each count stands */
  Printf.printf("PERF %s: %d steps (budget %d)\n", name, steps, budget);
  if (steps > budget) {
    failf(
      "%s: %d steps, over its budget of %d. A performance regression: find it, or raise the budget, with the reason in the commit.",
      name,
      steps,
      budget,
    );
  } else if (steps * 4 < budget * 3) {
    failf(
      "%s: %d steps, more than a quarter under its budget of %d. An improvement: lower the budget to about %d.",
      name,
      steps,
      budget,
      steps * 11 / 10,
    );
  };
};

/* Recursion, list traversals and case matches: bench/eval-bench.hz, smaller
   (it is timed by `hazel bench-eval`; this counts it). `k` is the literal
   an edit changes, after all the work. */
let program = (~rounds: int, ~fib: int) =>
  Printf.sprintf(
    {|let fib : Int -> Int =
  fun n -> if n < 2 then n else fib(n - 1) + fib(n - 2)
in
let range : Int -> [Int] =
  fun n -> if n == 0 then [] else n::range(n - 1)
in
let map : ((Int -> Int), [Int]) -> [Int] =
  fun (f, xs) ->
    case xs
    | [] => []
    | hd::tl => f(hd)::map(f, tl)
    end
in
let sum : [Int] -> Int =
  fun xs ->
    case xs
    | [] => 0
    | hd::tl => hd + sum(tl)
    end
in
let step : Int -> Int =
  fun acc ->
    let ns = range(40) in
    acc + sum(map(fun x -> x * 2, ns)) + fib(%d)
in
let go : (Int, Int) -> Int =
  fun (i, acc) -> if i == 0 then acc else go(i - 1, step(acc))
in
let total = go(%d, 0) in
let k = 1000 in
total + k|},
    fib,
    rounds,
  );

let cold_test = () => {
  let (result, _, steps) = work(parse_exp(program(~rounds=3, ~fib=10)));
  check(dhexp_typ, "the program's value", parse_exp("6085"), result);
  /* 206,834 steps on 2026-10-12 */
  within_budget("cold run", ~budget=227_000, steps);
};

/* An edit after all the work, to a literal nothing else reads: the
   evaluator should reuse `total` and do almost nothing. It is the failure
   issues 004 and 005 kept finding by hand: a change re-evaluating
   everything (the woods' 610,670 steps on every change), invisible to
   tests that check only the answer. */
let edit_test = () => {
  let exp = parse_exp(program(~rounds=3, ~fib=10));
  let (_, prev, cold) = work(exp);
  let edited = replace_int_lit(~from=1000, ~to_=2000, exp);
  let (result, _, steps) = work(~prev, edited);
  check(dhexp_typ, "the edited program's value", parse_exp("7085"), result);
  /* 378 steps on 2026-10-12: `total` reused */
  within_budget("re-run after an edit", ~budget=420, steps);
  /* whatever the budgets, an edit like this costs a small part of a run */
  check(
    bool,
    Printf.sprintf(
      "the edit's %d steps are under 5%% of a run's %d",
      steps,
      cold,
    ),
    true,
    steps * 20 < cold,
  );
};

/* A library module with recursive members, as every Livelit Playground
   slide's Drawing library has, and a picture built from it, then a knob
   only the last line reads. An edit to the knob should reuse the library
   and the picture. Until #2675 the recursive members' own names counted
   as free around them, with no provenance in any reuse map, so the
   module's binding was never reused: here that re-run took 252 steps, not
   132, and fails this budget (its items were still reused one by one); on
   the livelits branch, where much more reads such a library, every move of
   a figure in the woods redid far more (performance series, reports 004
   and 006). */
let library_program = {|let lib = {
  let range : Int -> [Int] =
    fun n -> if n == 0 then [] else n :: range(n - 1);
  let sum : [Int] -> Int =
    fun xs -> case xs | [] => 0 | hd :: tl => hd + sum(tl) end;
  let picture = sum(range(300))
} in
let knob = 7 in
lib.picture + knob|};

let library_edit_test = () => {
  let exp = parse_exp(library_program);
  let (_, prev, cold) = work(exp);
  let edited = replace_int_lit(~from=7, ~to_=8, exp);
  let (result, _, steps) = work(~prev, edited);
  check(dhexp_typ, "the edited program's value", parse_exp("45158"), result);
  /* 132 steps on 2026-10-15 (a cold run is 127,394): the library reused */
  within_budget("re-run beside a recursive library", ~budget=145, steps);
  check(
    bool,
    Printf.sprintf(
      "the edit's %d steps are under 5%% of a run's %d",
      steps,
      cold,
    ),
    true,
    steps * 20 < cold,
  );
};

let tests = (
  "PerfBudgets",
  [
    test_case("Cold run: steps within budget", `Quick, cold_test),
    test_case("Re-run after an edit: steps within budget", `Quick, edit_test),
    test_case(
      "Re-run beside a library with recursive members: steps within budget",
      `Quick,
      library_edit_test,
    ),
  ],
);

open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Performance budgets: the work an evaluation does, counted and held to a
   budget, so that CI fails on a performance regression.

   The work is the steps the evaluator's trampoline actually takes
   (Evaluator.evaluate_counting_work), and the nodes it builds (each term
   made fresh, copied or rewrapped, IdTagged.built): a step that copies a
   whole value is one step but many nodes. Counts, not times: it is the same
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
   previous run if there is one: the result, the cache, and the work (its
   steps, and the nodes it built). */
let work_and_nodes =
    (~prev: EvaluatorState.incr_eval=IncrEval.empty, exp: Exp.t)
    : (Exp.t, EvaluatorState.incr_eval, int, int) => {
  let (info_map, elab) = statics_and_elab(exp);
  let eval_info =
    EvalInfo.of_info_map(
      ~probe_all=CoreSettings.on.probe_all,
      ~targets=Id.Map.empty,
      info_map,
    );
  let before = IdTagged.built^;
  let ((result, state), steps) =
    Evaluator.evaluate_counting_work(
      ~prev,
      ~eval_info,
      ~env=Builtins.env_init,
      elab,
    );
  (result, state.incr_eval, steps, IdTagged.built^ - before);
};
let work = (~prev=?, exp: Exp.t): (Exp.t, EvaluatorState.incr_eval, int) => {
  let (result, cache, steps, _) = work_and_nodes(~prev?, exp);
  (result, cache, steps);
};

let within_budget =
    (name: string, ~what="steps", ~budget: int, count: int): unit => {
  /* in the test's log, so a run shows where each count stands */
  Printf.printf("PERF %s: %d %s (budget %d)\n", name, count, what, budget);
  if (count > budget) {
    failf(
      "%s: %d %s, over its budget of %d. A performance regression: find it, or raise the budget, with the reason in the commit.",
      name,
      count,
      what,
      budget,
    );
  } else if (count * 4 < budget * 3) {
    failf(
      "%s: %d %s, more than a quarter under its budget of %d. An improvement: lower the budget to about %d.",
      name,
      count,
      what,
      budget,
      count * 11 / 10,
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

/* The nodes a cold run and a re-run build: what the step budgets above
   count, counted as nodes */
let nodes_test = () => {
  let exp = parse_exp(program(~rounds=3, ~fib=10));
  let (_, prev, _, cold) = work_and_nodes(exp);
  /* 17,464 nodes on 2026-10-16 */
  within_budget("cold run", ~what="nodes", ~budget=19_200, cold);
  let edited = replace_int_lit(~from=1000, ~to_=2000, exp);
  let (_, _, _, rerun) = work_and_nodes(~prev, edited);
  /* 90 nodes on 2026-10-16 */
  within_budget("re-run after an edit", ~what="nodes", ~budget=100, rerun);
};

/* A list of labeled tuples passed again and again through a typed
   parameter, the tail at each call too: data that already has the type
   should cross as itself (#2679). Before #2679 each crossing copied the
   list, cell by cell and field by field: one step, so the step budgets
   cannot see it. With dev's Ascriptions (2026-10-16) this program takes
   the same 195,319 steps but builds 27,777 nodes, not 21,155, and fails
   its budget. */
let typed_rows = (~n: int, ~passes: int) =>
  Printf.sprintf(
    {|let rows : [(a=Int, b=Int)] = [%s] in
let count : [(a=Int, b=Int)] -> Int =
  fun xs ->
    case xs
    | [] => 0
    | _::tl => 1 + count(tl)
    end
in
let go : Int -> Int =
  fun i -> if i == 0 then 0 else count(rows) + go(i - 1)
in
go(%d)|},
    String.concat(
      ", ",
      List.init(n, i => Printf.sprintf("(a=%d, b=%d)", i, i * 2)),
    ),
    passes,
  );

let typed_test = () => {
  let (result, _, steps, nodes) =
    work_and_nodes(parse_exp(typed_rows(~n=100, ~passes=10)));
  check(dhexp_typ, "the program's value", parse_exp("1000"), result);
  Printf.printf("PERF typed rows: %d steps\n", steps);
  /* 21,155 nodes on 2026-10-16, with #2679 */
  within_budget("typed rows", ~what="nodes", ~budget=23_300, nodes);
};

let tests = (
  "PerfBudgets",
  [
    test_case("Cold run: steps within budget", `Quick, cold_test),
    test_case("Re-run after an edit: steps within budget", `Quick, edit_test),
    test_case("Cold run and re-run: nodes within budget", `Quick, nodes_test),
    test_case(
      "Typed rows crossing a typed parameter: nodes within budget",
      `Quick,
      typed_test,
    ),
  ],
);

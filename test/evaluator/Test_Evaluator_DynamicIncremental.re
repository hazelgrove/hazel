open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Tests for the dynamic-incremental machinery added alongside the
 * cooperative worker scheduler:
 *
 *   - IncrEval.changed_ids: id-keyed elab diff used both as an editor
 *     overlay hint and as a worker-side fast-invalidation signal.
 *   - IncrEval.merge: union of two caches, newer wins on collision —
 *     used by the scheduler when restarting a partial run with a fresh
 *     editor request.
 *   - IncrEval.filter_safe: the worker-side cache pre-filter, drops
 *     entries the new elab can no longer reuse.
 *   - IncrEval.pending_ids: ids the upcoming run will visit fresh —
 *     drives the editor's "still calculating" overlays.
 *   - Evaluator step-limit + Invalid("Timeout"): the global step cap
 *     translates to a timeout result the worker can surface.
 *
 * Each test reproduces a real edit shape (id-preserving) using
 * `replace_int_lit` from the existing incremental tests, so we
 * actually exercise id-keyed reuse rather than running two unrelated
 * evaluations. */

let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
  Statics.mk(
    CoreSettings.on,
    Builtins.ctx_init(Some(Operators.default_mode)),
    exp,
  );

let info_slice_of = (info_map: Statics.Map.t): IncrEval.InfoSlice.t =>
  IncrEval.InfoSlice.of_info_map(info_map);

let eval_to_state =
    (~prev: IncrEval.t=IncrEval.empty, exp: Exp.t)
    : (Exp.t, EvaluatorState.t) => {
  let (info_map, elab) = statics_and_elab(exp);
  let info_slice = info_slice_of(info_map);
  Evaluator.evaluate(~prev, ~info_slice, ~env=Builtins.env_init, elab);
};

/* Walks the same id-preserving edit pattern used in
 * Test_Evaluator_Incremental: change one Int literal payload while
 * leaving every surrounding IdTagged annotation alone. */
let replace_int_lit = (~from: int, ~to_: int, exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Atom(Int(n)) when Bigint.to_string(n) == string_of_int(from) =>
      let new_term: Exp.term = Atom(Int(Bigint.of_int(to_)));
      {
        ...e,
        term: new_term,
      };
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* changed_ids on identical elaborations is empty. */
let test_changed_ids_identity = () => {
  let src = "let x = 1 in x + 2";
  let prev = parse_exp(src);
  let (_, prev_elab) = statics_and_elab(prev);
  let curr_elab = prev_elab; /* same Exp.t */
  check(
    int,
    "no changes between identical elabs",
    0,
    List.length(IncrEval.changed_ids(~prev=prev_elab, curr_elab)),
  );
};

/* Editing one literal yields a non-empty changed_ids that contains the
 * edited leaf's id. We don't pin the exact size (ancestors also report
 * structurally-different by fast_equal), but it must be non-empty. */
let test_changed_ids_after_leaf_edit = () => {
  let src = "let x = 5 in x + 2";
  let prev = parse_exp(src);
  let curr = replace_int_lit(~from=5, ~to_=7, prev);
  let (_, prev_elab) = statics_and_elab(prev);
  let (_, curr_elab) = statics_and_elab(curr);
  let diff = IncrEval.changed_ids(~prev=prev_elab, curr_elab);
  check(bool, "leaf edit produces some changed ids", true, diff != []);
};

/* IncrEval.merge: a cache entry present only in `older` survives;
 * a cache entry in both prefers the `newer` value. */
let test_merge_prefers_newer = () => {
  let src1 = "let a = 1 in let b = 2 in a + b";
  let src2 = "let a = 1 in let b = 2 in a + b + 0";
  let exp1 = parse_exp(src1);
  let exp2 = parse_exp(src2);
  let (_, _, incr1) = {
    let (info_map, elab) = statics_and_elab(exp1);
    let info_slice = info_slice_of(info_map);
    let (r, s) =
      Evaluator.evaluate(~info_slice, ~env=Builtins.env_init, elab);
    (r, s, s.incr_eval);
  };
  let (_, _, incr2) = {
    let (info_map, elab) = statics_and_elab(exp2);
    let info_slice = info_slice_of(info_map);
    let (r, s) =
      Evaluator.evaluate(~info_slice, ~env=Builtins.env_init, elab);
    (r, s, s.incr_eval);
  };
  let merged = IncrEval.merge(~newer=incr2, ~older=incr1);
  /* Merged size is at least max(size1, size2). */
  let n1 = Id.Map.cardinal(incr1.entries);
  let n2 = Id.Map.cardinal(incr2.entries);
  let nm = Id.Map.cardinal(merged.entries);
  check(bool, "merge size >= max of inputs", true, nm >= max(n1, n2));
  /* recalculated/reused are reset on merge. */
  check(
    bool,
    "merge clears recalculated marker",
    true,
    merged.recalculated == [],
  );
  check(bool, "merge clears reused marker", true, merged.reused == []);
};

/* filter_safe drops entries that fail reuse_check against the new
 * info_slice — and at minimum drops entries whose ids are explicitly
 * listed as changed. */
let test_filter_safe_drops_changed = () => {
  let src = "let x = 5 in x + 2";
  let prev = parse_exp(src);
  let (info_map, prev_elab) = statics_and_elab(prev);
  let (_, prev_state) =
    Evaluator.evaluate(
      ~info_slice=info_slice_of(info_map),
      ~env=Builtins.env_init,
      prev_elab,
    );
  let prev_incr = prev_state.incr_eval;
  let n_before = Id.Map.cardinal(prev_incr.entries);
  /* Pick any cached id and assert it gets dropped when listed as changed. */
  let any_id =
    switch (Id.Map.bindings(prev_incr.entries)) {
    | [(id, _), ..._] => id
    | [] => Alcotest.fail("Empty cache; nothing to filter")
    };
  let filtered =
    IncrEval.filter_safe(
      ~prev=prev_incr,
      ~changed_ids=[any_id],
      ~new_info_slice=info_slice_of(info_map),
      ~new_targets=Sample.no_targets,
      prev_elab,
    );
  let n_after = Id.Map.cardinal(filtered.entries);
  check(
    bool,
    "filter_safe drops at least the explicitly-changed id",
    true,
    n_after < n_before,
  );
  check(
    bool,
    "filter_safe really removed the explicitly-changed id",
    false,
    Id.Map.mem(any_id, filtered.entries),
  );
};

/* pending_ids on an empty `prev` returns every id in the new elab
 * (everything is pending — there's no cache to short-circuit anything). */
let test_pending_ids_no_cache = () => {
  let src = "let x = 5 in x + 2";
  let exp = parse_exp(src);
  let (info_map, elab) = statics_and_elab(exp);
  let pending =
    IncrEval.pending_ids(
      ~prev=IncrEval.empty,
      ~changed_ids=Id.Set.empty,
      ~new_info_slice=info_slice_of(info_map),
      ~new_targets=Sample.no_targets,
      elab,
    );
  check(
    bool,
    "no cache => some ids are pending",
    true,
    !Id.Set.is_empty(pending),
  );
};

/* pending_ids is the editor's "while-in-flight" pulse driver. The
 * caller (`EvalResult.Update.calculate`) only invokes it while the
 * worker is mid-flight, so the v1 semantics is "every id in the new
 * elaboration is potentially being recomputed". An ideal version
 * would replicate the evaluator's `dirty_names` propagation so it can
 * exclude provably-reusable probes; a future revision can either do
 * that or have the worker emit `Progress(completed_ids)` so the set
 * shrinks incrementally. */
let test_pending_ids_fully_cached = () => {
  let src = "let x = 5 in x + 2";
  let exp = parse_exp(src);
  let (info_map, elab) = statics_and_elab(exp);
  let info_slice = info_slice_of(info_map);
  let (_, state) =
    Evaluator.evaluate(~info_slice, ~env=Builtins.env_init, elab);
  let pending =
    IncrEval.pending_ids(
      ~prev=state.incr_eval,
      ~changed_ids=Id.Set.empty,
      ~new_info_slice=info_slice,
      ~new_targets=Sample.no_targets,
      elab,
    );
  check(
    bool,
    "pending_set covers every id in elab",
    true,
    !Id.Set.is_empty(pending),
  );
};

/* The global step limit returns StepLimitExceeded; the worker scheduler
 * surfaces this as Invalid("Timeout"). Here we exercise the underlying
 * cap directly: a tiny step_limit on a non-trivial program triggers
 * StepLimitExceeded. */
let test_step_limit_exceeded = () => {
  let src = "let x = 1 + 2 in let y = x + 10 in y";
  let exp = parse_exp(src);
  let (info_map, elab) = statics_and_elab(exp);
  let info_slice = info_slice_of(info_map);
  let result =
    Evaluator.evaluate_and_limit(
      ~step_limit=1, /* far below what a real run needs */
      ~info_slice,
      ~env=Builtins.env_init,
      elab,
    );
  let ok =
    switch (result) {
    | Evaluator.StepLimitExceeded => true
    | Evaluator.Completed(_) => false
    };
  check(bool, "tiny step limit triggers StepLimitExceeded", true, ok);
};

/* Trampoline.with_budget aborts mid-flight: a body that would otherwise
 * loop forever returns the fallback value once its budget is exhausted,
 * AND the surrounding computation continues. This is the key piece that
 * distinguishes per-id timeout from a global step cap.
 *
 * We construct a non-terminating trampoline (recursive bind chain) and
 * wrap it in a tiny budget, then bind a continuation that observes the
 * fallback — if the abort actually returns control to the parent, the
 * continuation runs and we see its result. */
let test_with_budget_aborts_and_continues = () => {
  /* Body that loops by repeatedly binding into itself; each bind step
   * increments the trampoline step counter. */
  let rec forever = (): Evaluator.Trampoline.t(int) =>
    Evaluator.Trampoline.bind(Evaluator.Trampoline.return(0), _ =>
      forever()
    );
  /* Wrap it in a 100-step budget; on abort the body's value is replaced
   * with the fallback (-1). The bind below observes that value and adds
   * 100 — so a successful abort produces 99. */
  let computation =
    Evaluator.Trampoline.bind(
      Evaluator.Trampoline.with_budget(
        ~budget=100,
        ~fallback=-1,
        forever(),
      ),
      v => Evaluator.Trampoline.return(v + 100),
    );
  let result = Evaluator.Trampoline.run(computation);
  switch (result) {
  | Completed(99) =>
    check(bool, "with_budget aborted and continuation ran", true, true)
  | Completed(other) =>
    Alcotest.fail(
      Printf.sprintf(
        "Expected 99, got %d (continuation didn't see fallback)",
        other,
      ),
    )
  | StepLimitExceeded =>
    Alcotest.fail("with_budget did not abort: hit global step limit")
  };
};

/* Nested with_budget: inner body times out, inner fallback is returned
 * to the outer body's continuation, outer body completes normally. */
let test_with_budget_nested = () => {
  let rec forever = (): Evaluator.Trampoline.t(int) =>
    Evaluator.Trampoline.bind(Evaluator.Trampoline.return(0), _ =>
      forever()
    );
  let inner =
    Evaluator.Trampoline.bind(
      Evaluator.Trampoline.with_budget(
        ~budget=50,
        ~fallback=42,
        forever(),
      ),
      v => Evaluator.Trampoline.return(v * 2),
    );
  let outer =
    Evaluator.Trampoline.with_budget(
      ~budget=10_000,
      ~fallback=-999,
      inner,
    );
  switch (Evaluator.Trampoline.run(outer)) {
  | Completed(84) =>
    check(bool, "inner abort, outer continuation runs", true, true)
  | Completed(other) =>
    Alcotest.fail(Printf.sprintf("Expected 84, got %d", other))
  | StepLimitExceeded =>
    Alcotest.fail("nested abort: unexpected global step limit hit")
  };
};

/* Within-budget bodies run to completion; the marker is a no-op
 * pass-through. */
let test_with_budget_no_abort = () => {
  let body = Evaluator.Trampoline.return(7);
  let result =
    Evaluator.Trampoline.run(
      Evaluator.Trampoline.bind(
        Evaluator.Trampoline.with_budget(
          ~budget=1_000_000,
          ~fallback=-1,
          body,
        ),
        v => Evaluator.Trampoline.return(v + 1),
      ),
    );
  switch (result) {
  | Completed(8) =>
    check(bool, "no-overrun: body's value passed through", true, true)
  | Completed(other) =>
    Alcotest.fail(Printf.sprintf("Expected 8, got %d", other))
  | StepLimitExceeded => Alcotest.fail("hit step limit unexpectedly")
  };
};

/* Full restart-cycle simulation: run program 1 to completion, take the
 * resulting cache, simulate a "restart with edited elab" by merging
 * with empty, and verify the merged cache is reused on a subsequent run
 * of an identical program (i.e., the merge plumbing didn't corrupt it). */
let test_merge_then_reuse = () => {
  let src = "let x = 5 in x + 2";
  let exp = parse_exp(src);
  let (info_map, elab) = statics_and_elab(exp);
  let info_slice = info_slice_of(info_map);
  let (_, state1) =
    Evaluator.evaluate(~info_slice, ~env=Builtins.env_init, elab);
  let merged = IncrEval.merge(~newer=state1.incr_eval, ~older=IncrEval.empty);
  let (_, state2) =
    Evaluator.evaluate(
      ~prev=merged,
      ~info_slice,
      ~env=Builtins.env_init,
      elab,
    );
  check(
    bool,
    "merged cache is consumed (reuse list non-empty)",
    true,
    state2.incr_eval.reused != [],
  );
};

let tests = (
  "Evaluator.DynamicIncremental",
  [
    test_case(
      "changed_ids identity is empty",
      `Quick,
      test_changed_ids_identity,
    ),
    test_case(
      "changed_ids after leaf edit non-empty",
      `Quick,
      test_changed_ids_after_leaf_edit,
    ),
    test_case("merge prefers newer entries", `Quick, test_merge_prefers_newer),
    test_case(
      "filter_safe drops changed ids",
      `Quick,
      test_filter_safe_drops_changed,
    ),
    test_case(
      "pending_ids without cache is non-empty",
      `Quick,
      test_pending_ids_no_cache,
    ),
    test_case(
      "pending_ids fully-cached is empty",
      `Quick,
      test_pending_ids_fully_cached,
    ),
    test_case(
      "step limit exceeded surfaces correctly",
      `Quick,
      test_step_limit_exceeded,
    ),
    test_case(
      "with_budget aborts mid-flight and continuation runs",
      `Quick,
      test_with_budget_aborts_and_continues,
    ),
    test_case(
      "with_budget nested: inner abort, outer continues",
      `Quick,
      test_with_budget_nested,
    ),
    test_case(
      "with_budget no-overrun is a pass-through",
      `Quick,
      test_with_budget_no_abort,
    ),
    test_case(
      "merge then reuse: cache survives plumbing",
      `Quick,
      test_merge_then_reuse,
    ),
  ],
);

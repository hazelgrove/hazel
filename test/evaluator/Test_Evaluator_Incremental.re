open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Tests for the incremental evaluator. Exercises the three key mechanisms:
 * reuse when the elaboration is unchanged, invalidation when a co-ctx
 * dependency is dirtied, and replay of captured probe samples.
 *
 * CRITICAL (why these tests don't just call parse_exp twice):
 *
 *   parse_exp mints fresh Id.t values for every token on every call. So
 *   parsing "let b = 2" and then "let b = 5" gives two Exp.t trees whose
 *   id spaces are disjoint, and `reuse_check` — which is keyed by id —
 *   never finds an entry in the prev map. That means tests that parse
 *   their "edited" variants are NOT actually exercising incremental
 *   reuse or dirty propagation; they just run two unrelated evaluations
 *   and check the second's answer, which succeeds even if the incremental
 *   machinery is completely broken.
 *
 *   The real UI preserves ids across edits via the Zipper: a text edit
 *   changes only the ids of the touched tokens, leaving all surrounding
 *   tile/token ids intact. We simulate that here with `replace_int_lit`,
 *   which walks a parsed Exp.t and replaces an Atom(Int(n)) payload
 *   in-place while keeping the surrounding IdTagged annotations untouched.
 *
 *   Each test that claims to test reuse / dirtying ALSO asserts that
 *   `incr.reused` is non-empty on the second run, so we can't silently
 *   regress into the "disjoint id spaces" failure mode again. */

/* Statics.mk now returns the info_map AND the elaborated expression
 * together (Elaborator.re was merged into statics on dev), so we always
 * grab both at once. */
let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
  Statics.mk(
    CoreSettings.on,
    Builtins.ctx_init(Some(Operators.default_mode)),
    exp,
  );

let statics_of = (exp: Exp.t): Statics.Map.t => fst(statics_and_elab(exp));

/* Run the incremental evaluator end-to-end, returning the evaluated Exp.t,
 * final EvaluatorState, and resulting IncrEval.t (for test-readability we
 * surface the incr map separately even though it also lives in state). */
let eval_incr =
    (~prev: IncrEval.t=IncrEval.empty, exp: Exp.t)
    : (Exp.t, EvaluatorState.t, IncrEval.t) => {
  let (info_map, elab) = statics_and_elab(exp);
  let info_map =
    EvalInfoMap.of_info_map(~probe_all=CoreSettings.on.probe_all, info_map);
  let (result, state) =
    Evaluator.evaluate(~prev, ~info_map, ~env=Builtins.env_init, elab);
  (result, state, state.incr_eval);
};

/* Replace Atom(Int(from)) with Atom(Int(to_)) everywhere in `exp`,
 * preserving the IdTagged annotation on every node (including on the
 * edited leaf itself — only the payload integer changes, the id stays).
 * This simulates a single-token text edit as the Zipper would produce it,
 * which is the only way to exercise true incremental reuse in tests. */
let replace_int_lit = (~from: int, ~to_: int, ~to_id=?, exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Atom(Int(n)) when Bigint.to_string(n) == string_of_int(from) =>
      let new_term: Exp.term = Atom(Int(Bigint.of_int(to_)));
      {
        annotation: Option.value(~default=e.annotation, to_id),
        term: new_term,
      };
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* Simulates a structural edit that removes a let-wrapper while preserving
 * the ids of the body. Walks `exp` and replaces every
 *   Let(_, def, body)   where def is the int literal `rhs_val`
 * with `body` itself. The body's IdTagged annotation is untouched, so its
 * subtree retains the same ids it had inside the original Let.
 *
 * Pairs with replace_int_lit to give us TWO kinds of id-preserving edits:
 * - leaf edit (replace_int_lit): a single token mutates in place
 * - structural edit (strip_let_with_int_rhs): a wrapper is added/removed
 *   around an unchanged body
 *
 * The "added" direction is just: parse the WITH-wrapper version, derive the
 * WITHOUT-wrapper version by stripping, then run prev=without, curr=with. */
let strip_let_with_int_rhs = (~rhs_val: int, exp: Exp.t): Exp.t => {
  let rec go = (e: Exp.t): Exp.t => {
    let f_exp = (continue, e: Exp.t): Exp.t =>
      switch (e.term) {
      | Let(_, def, body)
          when
            switch (def.term) {
            | Atom(Int(n)) => Bigint.to_string(n) == string_of_int(rhs_val)
            | _ => false
            } =>
        go(body)
      | _ => continue(e)
      };
    TermBase.Exp.map_term(~f_exp, e);
  };
  go(exp);
};

/* Walk an Exp.t and collect ids of every Ap(_, _, _) node. Used by the
 * sibling-module test to assert that a specific function-application
 * subexpression's cache entry survives an edit to an unrelated binding. */
let collect_ap_ids = (exp: Exp.t): list(Id.t) => {
  let ids = ref([]);
  let f_exp = (continue, e: Exp.t): Exp.t => {
    switch (e.term) {
    | Ap(_, _, _) => ids := [Exp.rep_id(e), ...ids^]
    | _ => ()
    };
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, exp);
  ids^;
};

/* A non-empty incremental map after a run of a non-trivial program. */
let test_populates_entries = () => {
  let src = "let x = 1 + 2 in let y = x + 10 in y";
  let exp = parse_exp(src);
  let (_, _, incr) = eval_incr(exp);
  check(
    bool,
    "Incremental map is non-empty after a fresh run",
    true,
    !IncrEval.is_empty(incr),
  );
};

/* Running twice with the SAME Exp.t (so ids are identical): the second run
 * should reuse lots of entries. Without replace_int_lit / id preservation
 * this works for the wrong reason (parse_exp twice on the same string
 * produces disjoint ids) — but feeding the SAME exp twice avoids that,
 * and the `reused` assertion below pins reuse actually firing. */
let test_reuse_same_program = () => {
  let src = "let x = 1 + 2 in let y = x + 10 in y";
  let exp = parse_exp(src);
  let (r1, _, incr1) = eval_incr(exp);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp);
  check(dhexp_typ, "Reuse preserves the result value", r1, r2);
  check(
    bool,
    "Second run actually reused entries (reused list non-empty)",
    true,
    incr2.reused != [],
  );
};

/* Non-deferred subtrees below the outer let should ALSO get cache entries.
 * Bug this pins: treating every Closure wrapper as a deferred boundary
 * causes let-bodies to be excluded from caching, leaving only the
 * outermost id in the map. */
let test_nested_lets_populate_entries = () => {
  let src = "let x = 1 + 2 in let y = x + 10 in let z = y + 100 in z";
  let exp = parse_exp(src);
  let (_, _, incr) = eval_incr(exp);
  let entry_count = Id.Map.cardinal(incr.entries);
  check(
    bool,
    "At least 4 entries recorded for nested lets",
    true,
    entry_count >= 4,
  );
};

/* Editing only the innermost let: the edit in-place preserves all ids
 * except the changed literal's, so reuse should fire heavily on the
 * second run. Result: x=3, y=13, z=213. */
let test_partial_reuse_after_edit = () => {
  let src = "let x = 1 + 2 in let y = x + 10 in let z = y + 100 in z";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=100, ~to_=200, exp1);
  /* Sanity: the helper must actually change the expression. If
   * replace_int_lit is a no-op (wrong traversal, wrong pattern, etc.) the
   * rest of this test is vacuous — exp1 == exp2 means Run 2 reuses
   * everything trivially, and we'd be back to not testing anything. */
  check(
    bool,
    "replace_int_lit actually changed the expression",
    true,
    !Exp.fast_equal(exp1, exp2),
  );
  let (_, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(
    dhexp_typ,
    "Edit to z's rhs produces updated result",
    parse_exp("213"),
    r2,
  );
  /* Unchanged `x` and `y` RHSes must reuse. If reuse never fires we're
   * just re-running the whole program and proving nothing about
   * incrementality. */
  check(
    bool,
    "Unchanged subtrees reused from prev map",
    true,
    incr2.reused != [],
  );
};

/* Changing the rhs of a let whose bound var is USED downstream must
 * invalidate that downstream entry via name-based dirty propagation.
 * Regression test for: editing `b` to a different value returning the
 * stale cached `sum` because co_ctx ids pointed at variable use-sites
 * (never in the id-dirty set), so the `sum` cache was reused even
 * though `b`'s value had shifted.
 *
 * Run 1: a=5, b=5, sum=a+b = 10.
 * Run 2: a=5, b=2 (edit only that literal, preserving ids), sum=a+b = 7.
 *   `sum`'s elab (the Exp.t `a + b`) is unchanged — same ids, same shape
 *   — so without dirty-name tracking the cached `sum = 10` would be
 *   reused. The dirty rule must mark `b` dirty after its rhs produces
 *   a new value, and `sum`'s co_ctx referring to `b` must invalidate. */
let test_dirty_propagates_to_downstream_sum = () => {
  /* Craft the base so the edited literal is unique: `b = 77`, not `b = 5`
   * (both `a` and `b` can't share a value or the targeted replacement
   * helper would hit both). */
  let src = "let a = 5 in let b = 77 in let sum = a + b in sum";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=77, ~to_=2, exp1);
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "First run sum = 5 + 77 = 82", parse_exp("82"), r1);
  check(
    dhexp_typ,
    "After editing b, sum reflects new b (not stale cache)",
    parse_exp("7"),
    r2,
  );
  /* Sanity: the second run should still reuse SOMETHING (`a`'s rhs
   * literal 5, the var uses, etc.) — if reused is empty we've degraded
   * into a non-incremental run and the test is vacuous. */
  check(
    bool,
    "Second run reuses at least some entries",
    true,
    incr2.reused != [],
  );
};

/* Regression test: the dirty-propagation timing bug.
 *
 * Scenario (matches a user-reported repro):
 *   Run 1: initial program.
 *   Run 2: edit something AFTER `in` of let-X (in its body).
 *   Run 3: edit something BEFORE `in` of the SAME let-X (its RHS).
 *
 * Symptom (before fix): in run 3, L_X's RHS is recalculated and yields a
 * new value, but its body's elab is unchanged. The dirty-name marker for
 * the pattern used to fire inside L_X's outer Bind continuation — i.e.
 * AFTER the body had already been evaluated — so the body reused its
 * stale cached entry and the final result was wrong.
 *
 * Fix: mark the pattern dirty eagerly, right after the transition returns
 * the RHS's final value and before the body is recursively evaluated.
 *
 * This test uses in-place literal edits so the id space is preserved
 * across runs (a parse-every-time version would exercise nothing). */
let test_rhs_edit_after_body_edit_invalidates_body = () => {
  /* `let a = 5 in let b = 77 in a + b + 88`
   * Edits: 88 -> 99 (body of let-b), then 77 -> 2 (rhs of let-b). */
  let src = "let a = 5 in let b = 77 in a + b + 88";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=88, ~to_=99, exp1);
  let exp3 = replace_int_lit(~from=77, ~to_=2, exp2);
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  let (r3, _, incr3) = eval_incr(~prev=incr2, exp3);
  check(dhexp_typ, "Run 1: 5 + 77 + 88 = 170", parse_exp("170"), r1);
  check(dhexp_typ, "Run 2: 5 + 77 + 99 = 181", parse_exp("181"), r2);
  check(
    dhexp_typ,
    "Run 3: 5 + 2 + 99 = 106 (NOT the stale 181)",
    parse_exp("106"),
    r3,
  );
  /* The "disjoint id spaces" failure mode would produce the correct
   * answer trivially because nothing is reused. Assert reuse actually
   * fires on runs 2 and 3 so the test verifies incremental behavior. */
  check(
    bool,
    "Run 2 reused some entries (not a from-scratch eval)",
    true,
    incr2.reused != [],
  );
  check(
    bool,
    "Run 3 reused some entries (not a from-scratch eval)",
    true,
    incr3.reused != [],
  );
};

/* ========================================================================
 * Coverage for more Exp.t forms beyond let + binops. Each test uses
 * replace_int_lit for in-place edits (to preserve ids), checks the final
 * value, and (where reuse should fire) asserts incr.reused != [] so we
 * can't silently degrade into a from-scratch re-evaluation. */

/* Function application: editing inside a function BODY should invalidate
 * every call site that depends on that function. Function bodies are
 * themselves deferred (not cached per-id inside), but the function binder
 * and the Ap sites in non-deferred positions are cached. The Ap entries'
 * co_ctx refers to the function name, so when the function binding's rhs
 * elab changes, the dirty-name marker forces both Ap calls to recompute. */
let test_function_body_edit_invalidates_apps = () => {
  /* Use a distinctive literal (9) inside the body so replace_int_lit only
   * targets the body and not the call sites. */
  let src = "let double = fun x -> x * 9 in double(5) + double(10)";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=9, ~to_=3, exp1);
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "Run 1: 45 + 90 = 135", parse_exp("135"), r1);
  check(
    dhexp_typ,
    "Run 2 after body edit: 15 + 30 = 45",
    parse_exp("45"),
    r2,
  );
  /* Literals 5 and 10 at the call sites are unchanged — they should reuse. */
  check(
    bool,
    "Second run still reuses some entries (call-site args)",
    true,
    incr2.reused != [],
  );
};

/* Editing one call's ARGUMENT should leave the other call's result reusable.
 * The function itself isn't dirty (its elab unchanged), and the second Ap's
 * elab (same args) is also unchanged, so its cached value is valid. */
let test_function_arg_edit_reuses_other_calls = () => {
  /* Pick unique literals so replace_int_lit only hits the targeted arg. */
  let src = "let double = fun x -> x * 2 in double(7) + double(11)";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=7, ~to_=100, exp1);
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "Run 1: 14 + 22 = 36", parse_exp("36"), r1);
  check(
    dhexp_typ,
    "Run 2 after first-arg edit: 200 + 22 = 222",
    parse_exp("222"),
    r2,
  );
  check(bool, "Second run reuses some entries", true, incr2.reused != []);
};

/* If: editing the UNTAKEN branch leaves the result unchanged; reuse should
 * fire because nothing the taken branch depends on has moved. We bind the
 * condition via a let to keep the if's elab small; the unchanged literal in
 * the taken branch and the binding's rhs should reuse. */
let test_if_untaken_branch_edit_reuses = () => {
  let src = "let taken = 42 in let skipped = 77 in if true then taken else skipped";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=77, ~to_=999, exp1);
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "Run 1: taken branch = 42", parse_exp("42"), r1);
  check(
    dhexp_typ,
    "Run 2 after untaken-branch edit: still 42",
    parse_exp("42"),
    r2,
  );
  check(
    bool,
    "Untaken-branch edit leaves reusable entries",
    true,
    incr2.reused != [],
  );
};

/* If: editing the TAKEN branch produces the new value. Sanity check that the
 * cache doesn't serve a stale answer when the result-producing branch moves. */
let test_if_taken_branch_edit_updates = () => {
  let src = "let taken = 42 in let skipped = 77 in if true then taken else skipped";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=42, ~to_=13, exp1);
  let (_, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(
    dhexp_typ,
    "Run 2 after taken-branch edit: 13 (not stale 42)",
    parse_exp("13"),
    r2,
  );
  check(
    bool,
    "Second run still reuses some entries",
    true,
    incr2.reused != [],
  );
};

/* Match (case): editing an UNTAKEN arm's body should leave the result
 * unchanged and allow reuse. */
let test_match_untaken_arm_edit_reuses = () => {
  /* `case 0 | 0 => 11 | _ => 22 end`: arm 0 is taken. Edit the 22 arm. */
  let src = "case 0 | 0 => 11 | _ => 22 end";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=22, ~to_=333, exp1);
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "Run 1: matched 0 -> 11", parse_exp("11"), r1);
  check(
    dhexp_typ,
    "Run 2 after untaken-arm edit: still 11",
    parse_exp("11"),
    r2,
  );
  check(
    bool,
    "Untaken-arm edit leaves reusable entries",
    true,
    incr2.reused != [],
  );
};

/* Match: editing the TAKEN arm produces the new value. */
let test_match_taken_arm_edit_updates = () => {
  let src = "case 0 | 0 => 11 | _ => 22 end";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=11, ~to_=555, exp1);
  let (_, _, incr1) = eval_incr(exp1);
  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
  check(
    dhexp_typ,
    "Run 2 after taken-arm edit: 555 (not stale 11)",
    parse_exp("555"),
    r2,
  );
};

/* Tuple as a returned value: editing one tuple element should produce a
 * tuple whose corresponding position is updated. */
let test_tuple_literal_edit_updates = () => {
  let src = "let x = (10, 20, 30) in x";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=20, ~to_=200, exp1);
  let (r1, _, _) = eval_incr(exp1);
  let (r2, _, _) = eval_incr(exp2);
  check(dhexp_typ, "Run 1: x = (10, 20, 30)", parse_exp("(10, 20, 30)"), r1);
  check(
    dhexp_typ,
    "Run 2 after editing tuple middle: x = (10, 200, 30)",
    parse_exp("(10, 200, 30)"),
    r2,
  );
};

/* Tuple destructuring: editing one tuple element should update the value
 * and invalidate consumers that depend on the affected binding. */
let test_tuple_destructuring_edit_updates = () => {
  let src = "let (a, b, c) = (10, 20, 30) in a + b + c";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=20, ~to_=200, exp1);
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "Run 1: 10 + 20 + 30 = 60", parse_exp("60"), r1);
  check(
    dhexp_typ,
    "Run 2 after editing tuple middle: 10 + 200 + 30 = 240",
    parse_exp("240"),
    r2,
  );
  check(bool, "Second run reuses some entries", true, incr2.reused != []);
};

/* List literal: same idea — editing one element shouldn't break the result.
 * We sum via a cons-accumulator-style expression to exercise Cons + fold. */
let test_list_literal_element_edit_updates = () => {
  /* Use length so the test is independent of fold syntax variations. */
  let src = "length([7, 8, 9])";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=8, ~to_=88, exp1);
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "Run 1: length [7, 8, 9] = 3", parse_exp("3"), r1);
  check(
    dhexp_typ,
    "Run 2 after element edit: length still 3",
    parse_exp("3"),
    r2,
  );
};

/* Shadowing: inner rebinding of the same name should not bleed changes out
 * to unrelated code paths. The outer `x`'s binding here (`x = 10`) is
 * referenced by `x + inner_result`, so editing the inner x's rhs should
 * update inner_result but leave outer x's contribution intact. */
let test_shadowing_inner_let_edit = () => {
  /* `let x = 10 in x + (let x = 7 in x)`
   * Run 1: 10 + 7 = 17.
   * Edit inner 7 -> 77: 10 + 77 = 87.
   * Outer x is shadowed inside the parenthesized let, so the outer x's
   * contribution of 10 must survive the inner edit. */
  let src = "let x = 10 in x + (let x = 7 in x)";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=7, ~to_=77, exp1);
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "Run 1: 10 + 7 = 17", parse_exp("17"), r1);
  check(
    dhexp_typ,
    "Run 2 after inner-x edit: 10 + 77 = 87",
    parse_exp("87"),
    r2,
  );
  check(bool, "Shadowing still allows reuse", true, incr2.reused != []);
};

/* Function bodies are a DEFERRED boundary: no incremental entries are
 * recorded for ids inside a closure body. Regression test that running a
 * program containing a function creates entries for the function binder
 * and call sites, but the entry count doesn't balloon with per-id body
 * entries.
 *
 * We just assert that calling a function twice with the same args still
 * re-executes the body each time (no per-id reuse across calls), by
 * checking that the incremental map is populated (so the outer caching is
 * working) while the final value is correct. */
let test_function_body_is_deferred = () => {
  let src = "let f = fun x -> x + 1 in f(5) + f(5)";
  let exp = parse_exp(src);
  let (r, _, incr) = eval_incr(exp);
  check(dhexp_typ, "f(5) + f(5) = 12", parse_exp("12"), r);
  check(
    bool,
    "Entries recorded for non-deferred positions",
    true,
    !IncrEval.is_empty(incr),
  );
};

/* Probe replay: samples from a cached run show up on a reused run. */
let test_probe_replay_on_reuse = () => {
  let src = "let x = 1 + 2 in x + 10";
  let exp = parse_exp(src);
  let (info_map, elab) = statics_and_elab(exp);
  let info_map =
    EvalInfoMap.of_info_map(~probe_all=CoreSettings.on.probe_all, info_map);
  /* First run: no probes targeted. */
  let (_, state1) =
    Evaluator.evaluate(
      ~prev=IncrEval.empty,
      ~info_map,
      ~env=Builtins.env_init,
      elab,
    );
  /* Second run: same elaboration, same (empty) targets. Just confirm the
   * slice gets replayed and step_count advances. */
  let (_, state2) =
    Evaluator.evaluate(
      ~prev=state1.incr_eval,
      ~info_map,
      ~env=Builtins.env_init,
      elab,
    );
  check(
    bool,
    "Reused run advances step_count via slice replay",
    true,
    EvaluatorState.get_step_count(state2) > 0,
  );
};

/* Regression for a counterexample shrunk out of
 * `qcheck_incremental_matches_fresh_after_edit`:
 *
 *   exp     = let (()) = (a=B, a=false) in 0
 *   edit    = 0 -> 1
 *   claim   = eval(edited, prev=empty) == eval(edited, prev=eval(exp).cache)
 *
 * The PBT reported the two sides disagreeing semantically. Both sides see
 * the same elaboration (we're only mutating the body literal in-place), so
 * any divergence implicates cache reuse, not semantics. The pattern is
 * intentionally weird (a 1-tuple of unit on the LHS, a 2-tuple with
 * duplicate label `a` on the RHS) to exercise whatever indet/error path
 * the generator stumbled onto. */
let test_pbt_regression_unit_pat_dup_label_dh_let = () => {
  let src = "let (()) = (a=B, a=false) in 0";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=0, ~to_=1, exp1);
  check(
    bool,
    "replace_int_lit actually changed the expression",
    true,
    !Exp.fast_equal(exp1, exp2),
  );
  let (r_fresh, _, _) = eval_incr(exp2);
  let (_, _, incr_prev) = eval_incr(exp1);
  let (r_incr, _, _) = eval_incr(~prev=incr_prev, exp2);
  check(
    dhexp_typ,
    "Incremental eval of edited matches fresh eval of edited",
    r_fresh,
    r_incr,
  );
};

/* Cross-module incremental reuse / UI tinting: when we edit a literal
 * inside module `a`, the unrelated module `c` should be visibly frozen
 * — every surface tile inside `c` should appear in the editor's
 * "frozen" decoration set.
 *
 * Background:
 *   `ModuleHelpers.lower` desugars `{ let bb = 12; let x = ... }` into
 *   a chain `Let(bb, 12, Let(x, ..., Let(...,Tuple(...))))`. The chain
 *   inverts surface nesting: the surface-outer Module M becomes the
 *   elab-innermost Tuple, and surface-sibling ModLets become elab-
 *   ancestors of one another.
 *
 *   When the evaluator hits the OUTERMOST elab Let on run 2 and finds
 *   its cached entry, it short-circuits via `Evaluator.re:158-164` and
 *   marks only that one id as reused (`IncrEval.mark_reused`). The
 *   surface-sibling inner ModLets `let x = fib(b)`, `let y = fib(b)`,
 *   `let z = x + y` are never visited and so end up in NEITHER
 *   `incr.reused` NOR `incr.recalculated` — leaving them un-tinted in
 *   the editor even though they're effectively frozen.
 *
 *   The fix is to derive a "frozen set" from `incr.reused` by walking
 *   each reused id's `prev_elab` and unioning all rep_ids encountered.
 *   That set is what the UI should paint as frozen. This test pins down
 *   the desired contents of that set. */
let test_module_c_inner_ids_in_frozen_set_after_edit_in_module_a = () => {
  let src = {|let fib = fun n ->
  if n < 2 then 1 else fib(n - 1) + fib(n - 2) in
let a = {
  let aa = 13;
  let xy = fib(aa);
  let yy = fib(aa);
  let zy = xy + yy
} in
let c = {
  let bb = 12;
  let x = fib(bb);
  let y = fib(bb);
  let z = x + y
} in (a, c)|};
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=13, ~to_=14, exp1);
  check(
    bool,
    "replace_int_lit actually changed the expression",
    true,
    !Exp.fast_equal(exp1, exp2),
  );
  /* Locate module c's RHS — the third top-level `let` (after fib, a). */
  let rec find_c_rhs = (depth, e: Exp.t): option(Exp.t) =>
    switch (e.term) {
    | Let(_, rhs, _) when depth == 2 => Some(rhs)
    | Let(_, _, body) => find_c_rhs(depth + 1, body)
    | _ => None
    };
  let c_rhs =
    switch (find_c_rhs(0, exp1)) {
    | Some(rhs) => rhs
    | None => failwith("could not locate module c's RHS module-block")
    };
  /* Pull out the surface ids of the inner ModLet items: `let x = fib(b)`,
   * `let y = fib(b)`, `let z = x + y`. We assert these all end up in the
   * frozen set after the edit. (The first item `let bb = 12` becomes the
   * elab-outermost wrapper that DOES get reused directly, so it's not the
   * interesting case here.) */
  let c_inner_modlet_ids: list(Id.t) =
    switch (c_rhs.term) {
    | Module(items) =>
      switch (items) {
      | [_first, ...rest] =>
        List.filter_map(
          (item: Mod.t) =>
            switch (item.term) {
            | ModLet(_, _) => Some(Mod.rep_id(item))
            | _ => None
            },
          rest,
        )
      | [] => []
      }
    | _ => failwith("c_rhs is not a Module")
    };
  check(
    int,
    "expected 3 inner ModLet items in module c (let x, let y, let z)",
    3,
    List.length(c_inner_modlet_ids),
  );
  let (_, _, incr1) = eval_incr(exp1);
  let (_, _, incr2) = eval_incr(~prev=incr1, exp2);
  /* The "frozen set" is what the UI should paint as frozen. Currently
   * `incr.reused` only contains ids that the evaluator actually visited
   * and short-circuited. The intended fix expands that to the elab-
   * descendant closure: for every reused id, walk its cached prev_elab
   * (in `incr.entries`) and union all rep_ids. */
  let frozen = IncrEval.frozen_ids(incr2);
  let missing = List.filter(id => !List.mem(id, frozen), c_inner_modlet_ids);
  check(
    int,
    "all inner ModLet ids of module c land in the frozen set",
    0,
    List.length(missing),
  );
};

/* Statics-level invariant: distinct TupLabel siblings of an elaborated
 * Tuple must have distinct rep_ids. The IncrEval cache is keyed by
 * rep_id; if two siblings collide on the same id, the cache silently
 * "last-write-wins" and one sibling's value gets returned for the
 * other's lookup whenever reuse fires below the parent.
 *
 * Regression for an earlier bug in Statics.uexp_to_info_map's Tuple arm:
 * the elaborated TupLabel was wrapped using the *outer Tuple's* rewrap
 * (the top-level shadow), so every elaborated TupLabel in a tuple
 * inherited the Tuple's id. This test pins that down by checking the
 * elaboration directly, independent of the evaluator. */
let test_tuple_elab_gives_distinct_tuplabel_ids = () => {
  let exp = parse_exp("let (()) = (a=B, a=false) in 0");
  let (_, elab) = statics_and_elab(exp);
  let rhs_tuple_elts =
    switch (elab.term) {
    | Let(_, rhs, _) =>
      switch (rhs.term) {
      | Tuple(elts) => elts
      | _ => []
      }
    | _ => []
    };
  let ids = List.map(Exp.rep_id, rhs_tuple_elts);
  check(
    int,
    "RHS tuple has 2 elements after elaboration",
    2,
    List.length(rhs_tuple_elts),
  );
  switch (ids) {
  | [id0, id1] =>
    check(
      bool,
      "TupLabel siblings have distinct rep_ids (no cache-key collision)",
      true,
      !Id.equal(id0, id1),
    )
  | _ => check(bool, "expected exactly 2 ids", true, false)
  };
};

/* Diagnostic: nested-module rhs change should mark the binder dirty.
 * Source: `let a = { let a = 5 } in let b = a in (b, 1)`.
 * Edit 5 -> 6. After re-eval, the result tuple's first component must
 * carry a=6, not the stale a=5 cached for `b`. If `a` is not marked
 * dirty for the body, `let b = a` reuses its stale value. */
let test_diag_nested_module_rhs_edit_marks_binder_dirty = () => {
  let src = {|let a = { let a = 5 } in let b = a in (b, 1)|};
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=5, ~to_=6, exp1);
  let (r1, _, incr1) = eval_incr(exp1);
  print_endline("=== run 2 begins ===");
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  print_endline("=== run 2 ends ===");
  print_endline("r1 = " ++ Exp.show(r1));
  print_endline("r2 = " ++ Exp.show(r2));
  check(
    bool,
    "r1 != r2 (edit propagated to result)",
    true,
    !Exp.fast_equal(r1, r2),
  );
  check(bool, "incr2 reused something", true, incr2.reused != []);
};

/* Repro: `let x = ({}, 0) in (x, 3)`. Edit `3` → `4`. The let-x rhs
 * `({}, 0)` is unchanged across runs, so the surface ids of:
 *   - `{}` (empty Module exp)
 *   - `0` (Atom)
 *   - the outer Tuple `({}, 0)` (the rhs)
 * should all land in `frozen_ids(incr2)`. Bug we're pinning: only `0`
 * tinted; `{}` and the Tuple didn't, because `SubexpProbeTargets`
 * pulled in synthetic ids from `ModuleHelpers.lower` and the resulting
 * MerkleSet construction shape diverged across runs. */
let test_diag_module_in_unchanged_rhs_tuple_lands_in_frozen = () => {
  let src = {|let x = ({}, 0) in (x, 3)|};
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=3, ~to_=4, exp1);
  check(
    bool,
    "replace_int_lit actually changed the expression",
    true,
    !Exp.fast_equal(exp1, exp2),
  );
  let rec find_let_x_rhs = (e: Exp.t): option(Exp.t) =>
    switch (e.term) {
    | Let(_, rhs, _) => Some(rhs)
    | Parens(inner) => find_let_x_rhs(inner)
    | _ => None
    };
  let rec unwrap_parens = (e: Exp.t): Exp.t =>
    switch (e.term) {
    | Parens(inner) => unwrap_parens(inner)
    | _ => e
    };
  let rhs =
    switch (find_let_x_rhs(exp1)) {
    | Some(rhs) => rhs
    | None => failwith("could not locate let x rhs")
    };
  let rhs_inner = unwrap_parens(rhs);
  let (tuple_id, module_id, zero_id) =
    switch (rhs_inner.term) {
    | Tuple([fst, snd]) =>
      let module_id =
        switch (unwrap_parens(fst).term) {
        | Module(_) => Exp.rep_id(unwrap_parens(fst))
        | _ => failwith("first slot is not a Module")
        };
      let zero_id =
        switch (unwrap_parens(snd).term) {
        | Atom(Int(_)) => Exp.rep_id(unwrap_parens(snd))
        | _ => failwith("second slot is not an Atom")
        };
      (Exp.rep_id(rhs_inner), module_id, zero_id);
    | _ => failwith("rhs inner is not a 2-tuple")
    };
  let (_, _, incr1) = eval_incr(exp1);
  let (_, _, incr2) = eval_incr(~prev=incr1, exp2);
  let frozen = IncrEval.frozen_ids(incr2);
  check(bool, "Atom 0 is in frozen set", true, List.mem(zero_id, frozen));
  check(
    bool,
    "Module {} is in frozen set",
    true,
    List.mem(module_id, frozen),
  );
  check(
    bool,
    "Tuple ({}, 0) is in frozen set",
    true,
    List.mem(tuple_id, frozen),
  );
};

/* =========================================================================
 * Shadowing correctness bugs in dirty_names propagation.
 *
 * The dirty_names mechanism in Evaluator.re tracks variable NAMES, not
 * binder identities. That means three failure modes are possible — these
 * tests pin all three down as currently-failing.
 *
 * Setup notes:
 * - We parse the WITH-inner-let version once, then derive the WITHOUT
 *   version via strip_let_with_int_rhs. The body's ids are identical
 *   between the two variants, so the inner body's cache entry from one
 *   run is keyed by an id that's still present in the other run — exactly
 *   the situation that triggers the bug.
 * - Each test also asserts incr2.reused != [] so we can't silently degrade
 *   into a from-scratch eval and accidentally produce the right answer. */

/* (1) Deleting an inner Let that was shadowing an outer same-named binding.
 *
 * Run 1 (exp_with):    let x = 10 in let x = 1 in x + x   -> 2
 * Run 2 (exp_without): let x = 10 in x + x                -> 20
 *
 * The body `x + x` retains the same id across the two variants. On run 2:
 * - The inner Let is gone, so its id never appears as an ancestor effect,
 *   and dirty_names is never extended with `x` from "x's rhs changed".
 * - The outer Let's rhs is `10` in both runs — no change, no dirty either.
 * - The body's cached entry has elab `x + x` (unchanged), co_ctx [x],
 *   dirty_names []. Reuse fires, returns cached value 2.
 *
 * Correct answer is 20 (outer x = 10 is now visible to the body). */
let test_delete_inner_let_uncovers_outer_binding = () => {
  let src = "let x = 10 in let x = 1 in x + x";
  let exp_with = parse_exp(src);
  let exp_without = strip_let_with_int_rhs(~rhs_val=1, exp_with);
  check(
    bool,
    "strip_let_with_int_rhs actually changed the expression",
    true,
    !Exp.fast_equal(exp_with, exp_without),
  );
  let (r1, _, incr1) = eval_incr(exp_with);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp_without);
  check(dhexp_typ, "Run 1: inner x=1 wins, x+x = 2", parse_exp("2"), r1);
  check(
    dhexp_typ,
    "Run 2 after deleting inner Let: outer x=10 wins, x+x = 20",
    parse_exp("20"),
    r2,
  );
  check(
    bool,
    "Second run reuses at least some entries (not a from-scratch eval)",
    true,
    incr2.reused != [],
  );
};

/* (2) Adding an inner Let that newly shadows an outer same-named binding.
 *
 * Run 1 (exp_without): let x = 10 in x + x                -> 20
 * Run 2 (exp_with):    let x = 10 in let x = 1 in x + x   -> 2
 *
 * On run 2: a new Let wraps the body. The new Let's id wasn't in prev, so
 * `id_value_changed` returns false for its rhs id (the (None, _) case in
 * newly_dirty_vars). No dirty propagation marks `x`. The inner body `x + x`
 * has the same id and elab as in run 1, co_ctx [x], dirty_names []. Reuse
 * fires, returns cached value 20.
 *
 * Correct answer is 2 (the new inner x=1 shadows outer x=10). */
let test_add_inner_let_shadows_outer_binding = () => {
  let src = "let x = 10 in let x = 1 in x + x";
  let exp_with = parse_exp(src);
  let exp_without = strip_let_with_int_rhs(~rhs_val=1, exp_with);
  let (r1, _, incr1) = eval_incr(exp_without);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp_with);
  check(dhexp_typ, "Run 1: only outer x=10, x+x = 20", parse_exp("20"), r1);
  check(
    dhexp_typ,
    "Run 2 after adding inner Let: inner x=1 shadows, x+x = 2",
    parse_exp("2"),
    r2,
  );
  check(
    bool,
    "Second run reuses at least some entries (not a from-scratch eval)",
    true,
    incr2.reused != [],
  );
};

/* (3) Spurious recalculation of a recursive function's call after an edit
 * to an unrelated wildcard binding — but ONLY when the function body is
 * parenthesized.
 *
 * Minimal repro (user-reported, modules are irrelevant — the trigger is the
 * Parens wrapper around the Fun body of a recursive let-binding):
 *
 *   let f = (fun (n:Int) ->
 *     if n < 2 then 1 else f(n - 1)) in
 *   let _ = 55 in
 *   f(8)
 *
 * Edit: 55 -> 77.
 *
 * `f(8)` doesn't reference the `_` binding at all — its co_ctx is [f], and
 * f's rhs hasn't changed, so the reuse_check at f(8)'s id should fire and
 * return the cached value. The Ap id is stable across runs and its elab is
 * unchanged.
 *
 * User-reported observation: with `fun n -> ...` (no parens) reuse fires;
 * with `(fun (n:Int) -> ...)` it doesn't. The Parens / type-annotation
 * presumably changes how the rhs elaborates (FixF wrapping, Parens around
 * the Fun, etc.), making `newly_dirty_vars` see a different value at f's
 * rhs id and falsely flag `f` dirty — which invalidates f(8). */
let test_paren_recursive_fun_app_reuses_after_unrelated_edit = () => {
  let src = {|let f = (fun (n:Int) ->
  if n < 2 then 1 else f(n - 1)) in
let _ = 55 in
f(8)|};
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=55, ~to_=77, exp1);
  check(
    bool,
    "replace_int_lit actually changed the expression",
    true,
    !Exp.fast_equal(exp1, exp2),
  );
  /* The `f(8)` Ap is the top-level call; its argument is Atom(Int(8)).
   * The recursive call inside f's body is `f(n - 1)`, which has a BinOp
   * argument, not an Atom — so filtering on Atom(Int(8)) uniquely picks
   * the top-level Ap. */
  let f8_id = {
    let found = ref(None);
    let f_exp = (continue, e: Exp.t): Exp.t => {
      switch (e.term) {
      | Ap(_, _, arg) =>
        switch (arg.term) {
        | Atom(Int(n)) when Bigint.to_string(n) == "8" =>
          found := Some(Exp.rep_id(e))
        | _ => ()
        }
      | _ => ()
      };
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, exp1);
    switch (found^) {
    | Some(id) => id
    | None => failwith("could not locate `f(8)` Ap node")
    };
  };
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "Run 1: f(8) = 1", parse_exp("1"), r1);
  check(dhexp_typ, "Run 2: f(8) = 1 (unchanged)", parse_exp("1"), r2);
  check(
    bool,
    "Run 2 reuses something (sanity, not a from-scratch run)",
    true,
    incr2.reused != [],
  );
  /* The actual bug we're pinning: `f(8)` should be reused — it doesn't
   * reference the `_ = 55` binding and `f`'s rhs is unchanged. With the
   * Parens around the Fun body, currently it lands in recalculated. */
  check(
    bool,
    "f(8) is reused on run 2 (it doesn't depend on the edited _-binding)",
    true,
    List.mem(f8_id, incr2.reused),
  );
};

/* (4) Three-run reuse: a leftmost BinOp that was reused on run 2 is not
 * reused on run 3, even though its subtree hasn't moved.
 *
 *   Run 1: 1 + 2 + 3 + 4   -> 10
 *   Run 2: 1 + 2 + 3 + 5   -> 11  (edit 4 -> 5)
 *   Run 3: 1 + 2 + 4 + 5   -> 12  (edit 3 -> 4)
 *
 * Left-associative parse: `((1 + 2) + 3) + 4`. The `1 + 2` BinOp has a
 * stable id and unchanged elab across all three runs. On run 2 the `1 + 2`
 * subtree should be reused from run 1, and indeed it is. On run 3 the same
 * subtree should be reused from run 2's cache — but it isn't.
 *
 * Suspected cause: when an id is reused on run 2, the cache entry is
 * re-added to state via `add_incr_entry` (Evaluator.re:155) — but its
 * state slice / probe-targets witness may not survive across to run 3's
 * `prev`, breaking elab_same / probe_targets equality on run 3. */
let test_three_run_leftmost_binop_reuses_on_run3 = () => {
  let src = "1 + 2 + 3 + 4";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=4, ~to_=5, exp1);
  let exp3 = replace_int_lit(~from=3, ~to_=4, exp2);
  /* Locate the `1 + 2` BinOp id — uniquely identified by both operands
   * being Atom(Int) literals 1 and 2. */
  let plus_1_2_id = {
    let found = ref(None);
    let f_exp = (continue, e: Exp.t): Exp.t => {
      switch (e.term) {
      | BinOp(_, lhs, rhs) =>
        switch (lhs.term, rhs.term) {
        | (Atom(Int(a)), Atom(Int(b)))
            when Bigint.to_string(a) == "1" && Bigint.to_string(b) == "2" =>
          found := Some(Exp.rep_id(e))
        | _ => ()
        }
      | _ => ()
      };
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, exp1);
    switch (found^) {
    | Some(id) => id
    | None => failwith("could not locate `1 + 2` BinOp")
    };
  };
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  let (r3, _, incr3) = eval_incr(~prev=incr2, exp3);
  check(dhexp_typ, "Run 1: 1+2+3+4 = 10", parse_exp("10"), r1);
  check(dhexp_typ, "Run 2: 1+2+3+5 = 11", parse_exp("11"), r2);
  check(dhexp_typ, "Run 3: 1+2+4+5 = 12", parse_exp("12"), r3);
  /* Sanity: on run 2, `(1+2)+3` is unchanged from run 1, so it should be
   * reused — short-circuiting the descent. That means `1+2` itself is
   * NOT directly visited on run 2 (its parent's reuse subsumed it), but
   * its prev_elab is covered by `frozen_ids`. */
  check(
    bool,
    "Run 2: `1 + 2` is in frozen_ids (subsumed by a reused ancestor)",
    true,
    List.mem(plus_1_2_id, IncrEval.frozen_ids(incr2)),
  );
  /* The actual bug: on run 3, `(1+2)+3` becomes `(1+2)+4` so its parent
   * (and the parent's parent) must be recalculated. The evaluator descends
   * past them into `1 + 2`, whose subtree hasn't changed at all — so this
   * id should land in incr3.reused.
   *
   * Currently fails because run 2's reuse at the `(1+2)+3` level drops
   * `1+2`'s cache entry from the outgoing incr.entries map (only the
   * reused id itself is re-added via add_incr_entry at Evaluator.re:155).
   * Run 3 then sees no entry for `1+2`'s id and recalculates from scratch. */
  check(
    bool,
    "Run 3: `1 + 2` is reused (its subtree is unchanged since run 1)",
    true,
    List.mem(plus_1_2_id, incr3.reused),
  );
};

/* (5) Outer-binding edit shouldn't dirty inner-shadowed use sites.
 *
 *   let x = 5 in
 *   let x = 4 in
 *   x
 *
 * The body `x` resolves to the inner `let x = 4`, not the outer `let x = 5`.
 * Editing 5 -> 6 changes the outer binding's rhs value but doesn't affect
 * the inner binding or the body, so the body's `x` (a Var node) should be
 * reused on run 2.
 *
 * Currently fails because `dirty_names` is a flat list of variable NAMES,
 * not (name, binder_id) pairs. The outer let dirties the name `x`, and
 * reuse_check at the body's `x` reference checks `co_ctx` name membership
 * against `dirty_names` without consulting which binder the use resolves
 * to. So the inner `x` use is wrongly invalidated even though it doesn't
 * depend on the edited outer binding. */
let test_outer_edit_does_not_dirty_inner_shadowed_use = () => {
  let src = "let x = 5 in let x = 4 in x";
  let exp1 = parse_exp(src);
  let exp2 = replace_int_lit(~from=5, ~to_=6, exp1);
  /* Locate the body `x` Var node — uniquely identifiable as the only
   * Exp.Var("x") in the source (the let pats are Pats, not Exps). */
  let body_x_id = {
    let found = ref(None);
    let f_exp = (continue, e: Exp.t): Exp.t => {
      switch (e.term) {
      | Var("x") => found := Some(Exp.rep_id(e))
      | _ => ()
      };
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, exp1);
    switch (found^) {
    | Some(id) => id
    | None => failwith("could not locate body `x` Var node")
    };
  };
  let (r1, _, incr1) = eval_incr(exp1);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
  check(dhexp_typ, "Run 1: inner x=4 wins, x = 4", parse_exp("4"), r1);
  check(
    dhexp_typ,
    "Run 2 after outer x=5->6: still 4 (inner shadows outer)",
    parse_exp("4"),
    r2,
  );
  /* The body `x` is not recalculated. It's either directly in `reused`
   * or subsumed by an ancestor's reuse (in which case it appears in the
   * frozen-ids set derived from `reused`). Without binder-id-aware dirty
   * propagation, this id ends up in `recalculated` because the outer
   * let's name-based dirty `x` falsely invalidates the inner `x` use. */
  check(
    bool,
    "Body `x` is NOT recalculated on run 2 (resolves to inner let, not edited outer)",
    false,
    List.mem(body_x_id, incr2.recalculated),
  );
};

/* (6) Edit that turns a let's rhs into a hole-containing BinOp should
 * invalidate the let's body.
 *
 *   Run 1: let x = 4 in x + 1          -> 5
 *   Run 2: let x = 4 + ? in x + 1      -> body is indet (x is a hole)
 *
 * On run 2 the body `x + 1` shouldn't reuse the cached value 5 — x's
 * binding is now indeterminate. Concretely the body `x + 1` id should not
 * be in incr2.reused, and the final result should differ from run 1's. */
let test_let_rhs_becomes_hole_invalidates_body = () => {
  let src = "let x = 4 + ? in x + 1";
  let exp_with_hole = parse_exp(src);
  /* Derive the without-hole version by finding the BinOp(Plus, 4, ?) and
   * replacing it with just its left operand (4). Preserves all surrounding
   * ids including the body `x + 1`. */
  let strip_plus_hole = (exp: Exp.t): Exp.t => {
    let f_exp = (continue, e: Exp.t): Exp.t =>
      switch (e.term) {
      | BinOp(_, lhs, rhs) =>
        switch (rhs.term) {
        | EmptyHole => lhs
        | _ => continue(e)
        }
      | _ => continue(e)
      };
    TermBase.Exp.map_term(~f_exp, exp);
  };
  let exp_without_hole = strip_plus_hole(exp_with_hole);
  check(
    bool,
    "strip_plus_hole actually changed the expression",
    true,
    !Exp.fast_equal(exp_with_hole, exp_without_hole),
  );
  /* Locate the body `x + 1` BinOp id — its lhs is Var("x") and rhs is
   * Atom(Int(1)). */
  let body_id = {
    let found = ref(None);
    let f_exp = (continue, e: Exp.t): Exp.t => {
      switch (e.term) {
      | BinOp(_, lhs, rhs) =>
        switch (lhs.term, rhs.term) {
        | (Var("x"), Atom(Int(n))) when Bigint.to_string(n) == "1" =>
          found := Some(Exp.rep_id(e))
        | _ => ()
        }
      | _ => ()
      };
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, exp_with_hole);
    switch (found^) {
    | Some(id) => id
    | None => failwith("could not locate body `x + 1` BinOp node")
    };
  };
  let (r1, _, incr1) = eval_incr(exp_without_hole);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp_with_hole);
  print_endline("DIAG r1 = " ++ Exp.show(r1));
  print_endline("DIAG r2 = " ++ Exp.show(r2));
  check(dhexp_typ, "Run 1: x = 4, x + 1 = 5", parse_exp("5"), r1);
  check(
    bool,
    "Run 2 result differs from Run 1 (x is now a hole, body shouldn't be 5)",
    true,
    !Exp.fast_equal(r1, r2),
  );
  check(
    bool,
    "Body `x + 1` is NOT reused on run 2 (its binding became indet)",
    false,
    List.mem(body_id, incr2.reused),
  );
};

let test_swap_counterexample = () => {
  let src = "let x = 1 in let x = 2 in x" |> parse_exp;
  let id1 = IdTagged.IdTag.fresh();
  let id2 = IdTagged.IdTag.fresh();
  let exp1 =
    src
    |> replace_int_lit(~from=1, ~to_=4, ~to_id=id1)
    |> replace_int_lit(~from=2, ~to_=5, ~to_id=id2);
  let exp2 =
    src
    |> replace_int_lit(~from=1, ~to_=5, ~to_id=id2)
    |> replace_int_lit(~from=2, ~to_=4, ~to_id=id1);
  let (_, _, incr1) = eval_incr(exp1);
  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
  // check value is correct after swap
  check(dhexp_typ, "Run 1: inner x=2 wins, x = 2", parse_exp("4"), r2);
};

let test_reuse_provenance_distinguishes_pattern_shapes = () => {
  let source_id = Id.mk();
  let x = () => Pat.fresh(Var("x"));
  let provenance = pat =>
    IncrEval.pat_provenance(~source_id, ~flag=IncrEval.Clean, pat);
  let direct = provenance(x());
  let tuple = provenance(Pat.fresh(Tuple([x()])));
  let list = provenance(Pat.fresh(ListLit([x()])));
  let cons = provenance(Pat.fresh(Cons(x(), Pat.fresh(Wild))));
  let some =
    provenance(Pat.fresh(Ap(Pat.fresh(Constructor("Some", None)), x())));
  check(
    bool,
    "Constructor payload provenance differs from direct binding",
    false,
    IncrEval.equal_reuse_map(some, direct),
  );
  check(
    bool,
    "Tuple element provenance differs from list element provenance",
    false,
    IncrEval.equal_reuse_map(tuple, list),
  );
  check(
    bool,
    "Tuple element provenance differs from cons-head provenance",
    false,
    IncrEval.equal_reuse_map(tuple, cons),
  );
};

/* Top-level application of a partially-applied (hence cast) function. The
 * cast-distribution fix makes the inner application reuse the call site's id;
 * at top level (call_stack==[], id in info_map) that id reaches IncrEval's
 * cache, where before it was a fresh (uncached) id. These guard against
 * re-entrant same-id caching corrupting the result or dirty propagation. */
let test_toplevel_cast_reuse = () => {
  let exp =
    parse_exp(
      "let add = fun (a, b) -> a + b in let g: Int -> Int = add(_, 1) in g(5)",
    );
  let (r1, _, incr1) = eval_incr(exp);
  let (r2, _, incr2) = eval_incr(~prev=incr1, exp);
  check(dhexp_typ, "cast call: reuse preserves result", r1, r2);
  check(bool, "cast call: reuse actually fired", true, incr2.reused != []);
};

let test_toplevel_cast_edit = () => {
  let exp1 =
    parse_exp(
      "let add = fun (a, b) -> a + b in let g: Int -> Int = add(_, 1) in g(5)",
    );
  let (_, _, incr1) = eval_incr(exp1);
  let exp2 = replace_int_lit(~from=1, ~to_=2, exp1);
  /* g(5) = add(5, 2) = 7 after the edit; stale reuse of the cast call's id
     would wrongly keep 6 */
  let (expected, _, _) = eval_incr(exp2);
  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
  check(
    dhexp_typ,
    "cast call: edit propagates (no stale reuse)",
    expected,
    r2,
  );
};

let tests = (
  "Evaluator.Incremental",
  [
    test_case(
      "Top-level cast call: incremental reuse preserves result",
      `Quick,
      test_toplevel_cast_reuse,
    ),
    test_case(
      "Top-level cast call: edit propagates (no stale reuse)",
      `Quick,
      test_toplevel_cast_edit,
    ),
    test_case(
      "DIAG module in unchanged rhs tuple lands in frozen",
      `Quick,
      test_diag_module_in_unchanged_rhs_tuple_lands_in_frozen,
    ),
    test_case(
      "DIAG nested-module rhs edit marks binder dirty",
      `Quick,
      test_diag_nested_module_rhs_edit_marks_binder_dirty,
    ),
    test_case(
      "PBT regression: let (()) = (a=B, a=false) in 0, edit 0 -> 1",
      `Quick,
      test_pbt_regression_unit_pat_dup_label_dh_let,
    ),
    test_case(
      "Tuple elab gives distinct rep_ids to TupLabel siblings",
      `Quick,
      test_tuple_elab_gives_distinct_tuplabel_ids,
    ),
    test_case(
      "Module c inner ids land in frozen set after edit in module a",
      `Quick,
      test_module_c_inner_ids_in_frozen_set_after_edit_in_module_a,
    ),
    test_case(
      "Populates entries on fresh run",
      `Quick,
      test_populates_entries,
    ),
    test_case(
      "Reuses previous run when elaboration unchanged",
      `Quick,
      test_reuse_same_program,
    ),
    test_case(
      "Nested let-bodies below outer let populate cache entries",
      `Quick,
      test_nested_lets_populate_entries,
    ),
    test_case(
      "Partial reuse: editing innermost let still produces right answer",
      `Quick,
      test_partial_reuse_after_edit,
    ),
    test_case(
      "Dirty propagates to downstream consumers (sum of a, b)",
      `Quick,
      test_dirty_propagates_to_downstream_sum,
    ),
    test_case(
      "RHS edit right after a body edit still invalidates body",
      `Quick,
      test_rhs_edit_after_body_edit_invalidates_body,
    ),
    test_case(
      "Function: edit inside body invalidates all call sites",
      `Quick,
      test_function_body_edit_invalidates_apps,
    ),
    test_case(
      "Function: editing one call's arg reuses the other call",
      `Quick,
      test_function_arg_edit_reuses_other_calls,
    ),
    test_case(
      "If: untaken-branch edit preserves result and reuses",
      `Quick,
      test_if_untaken_branch_edit_reuses,
    ),
    test_case(
      "If: taken-branch edit produces new value (no stale cache)",
      `Quick,
      test_if_taken_branch_edit_updates,
    ),
    test_case(
      "Match: untaken-arm edit preserves result and reuses",
      `Quick,
      test_match_untaken_arm_edit_reuses,
    ),
    test_case(
      "Match: taken-arm edit produces new value",
      `Quick,
      test_match_taken_arm_edit_updates,
    ),
    test_case(
      "Tuple: editing literal element updates returned tuple",
      `Quick,
      test_tuple_literal_edit_updates,
    ),
    test_case(
      "Tuple destructuring: editing one element propagates correctly",
      `Quick,
      test_tuple_destructuring_edit_updates,
    ),
    test_case(
      "List literal: element edit keeps list consumers correct",
      `Quick,
      test_list_literal_element_edit_updates,
    ),
    test_case(
      "Shadowing: inner let edit doesn't bleed into outer scope",
      `Quick,
      test_shadowing_inner_let_edit,
    ),
    test_case(
      "Function bodies are a deferred boundary",
      `Quick,
      test_function_body_is_deferred,
    ),
    test_case(
      "Replays probe slice on reuse (step_count advances)",
      `Quick,
      test_probe_replay_on_reuse,
    ),
    test_case(
      "SHADOWING: deleting inner Let uncovers outer binding (wrong cache hit)",
      `Quick,
      test_delete_inner_let_uncovers_outer_binding,
    ),
    test_case(
      "SHADOWING: adding inner Let newly shadows outer binding (wrong cache hit)",
      `Quick,
      test_add_inner_let_shadows_outer_binding,
    ),
    test_case(
      "SHADOWING: parenthesized recursive Fun — f(8) reuses after unrelated _=55 edit",
      `Quick,
      test_paren_recursive_fun_app_reuses_after_unrelated_edit,
    ),
    test_case(
      "THREE-RUN: leftmost `1+2` reuses on run 3 (1+2+3+4 -> 1+2+3+5 -> 1+2+4+5)",
      `Quick,
      test_three_run_leftmost_binop_reuses_on_run3,
    ),
    test_case(
      "SHADOWING: outer edit doesn't dirty inner-shadowed use (let x=5 in let x=4 in x)",
      `Quick,
      test_outer_edit_does_not_dirty_inner_shadowed_use,
    ),
    test_case(
      "HOLE: let rhs becoming `4 + ?` invalidates body `x + 1`",
      `Quick,
      test_let_rhs_becomes_hole_invalidates_body,
    ),
    test_case(
      "SWAP: swapping shadowed variables keeping their id",
      `Quick,
      test_swap_counterexample,
    ),
    test_case(
      "Reuse provenance distinguishes pattern projection shapes",
      `Quick,
      test_reuse_provenance_distinguishes_pattern_shapes,
    ),
  ],
);

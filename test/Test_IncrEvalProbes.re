open Alcotest;
open Language;
open Haz3lcore;
open Test_Evaluator_Prelude;

/* Does aM (and a2, and aStar) break PROBES?
 *
 * The property under test: probe output must be INVARIANT under caching.
 * Evaluating a program with a cache from a previous edit must produce the
 * same samples, with the same values, the same multiplicity and the same
 * order, as evaluating it under a0 with no cache at all. a0 is the only
 * admissible reference -- it is the calculus with no cache to be stale.
 *
 * That this is the codebase's own intent, not an assumption of this file:
 *  - a cache entry carries `state`, the EvaluatorState slice its subtree
 *    produced, and eval_4_reuse splices it back with EvaluatorState.append
 *    on a hit (Evaluator.re:474). Probe samples live in that slice. If
 *    samples were meant to report only freshly-evaluated points, carrying
 *    the slice would be pointless work.
 *  - reuse_check refuses a hit whose recorded probe_targets differ from the
 *    current ones (IncrEval.re, `EvalInfo.equal_probe_targets`), which only
 *    makes sense if a hit is expected to reproduce the recorded samples.
 *  - EvaluatorState.append goes to real trouble to REBASE a replayed slice's
 *    step window onto the current timeline (`shift_sample`), which only
 *    matters if replayed samples are supposed to sit where freshly minted
 *    ones would have.
 *
 * This file compares strictly more of a sample than Test_IncrEval does: the
 * call stack's FRAME IDS (not just its depth), the frame the sample was
 * attributed to, the captured environment, the argument value, the origin,
 * the step window, AND the cross-probe interleaving, which no per-id report
 * can see. */

let show = Test_IncrEval.show;
let eval_under = Test_IncrEval.eval_under;
let statics_and_elab = Test_IncrEval.statics_and_elab;
let lit = Test_IncrEval.lit;
let id_edit = Test_IncrEval.id_edit;

/* --- rendering ------------------------------------------------------ */

/* Collapse whitespace so a failure prints on one line. */
let squash = (s: string): string => {
  let b = Stdlib.Buffer.create(String.length(s));
  let sp = ref(false);
  String.iter(
    c =>
      switch (c) {
      | ' '
      | '\t'
      | '\n'
      | '\r' => sp := true
      | c =>
        if (sp^ && Stdlib.Buffer.length(b) > 0) {
          Stdlib.Buffer.add_char(b, ' ');
        };
        sp := false;
        Stdlib.Buffer.add_char(b, c);
      },
    s,
  );
  Stdlib.Buffer.contents(b);
};

let short = (id: Id.t): string => {
  let s = Id.to_string(id);
  String.length(s) >= 6 ? String.sub(s, 0, 6) : s;
};

let show_elided = (v: CallStack.elided_value): string =>
  switch (v) {
  | Opaque => "<opaque>"
  | Val(d) => show(d)
  };

/* The call stack as the probe UI sees it: which calls, in which order, by
 * application id. `show_sample` in Test_IncrEval compares only the DEPTH, so
 * a sample attributed to the wrong call of the same depth is invisible to
 * it. */
let show_stack = (cs: CallStack.t): string =>
  cs
  |> List.map((f: CallStack.frame) =>
       (
         switch (f.name) {
         | Some(n) => n
         | None => "?"
         }
       )
       ++ "@"
       ++ short(f.id)
     )
  |> String.concat("/");

let show_env = (e: Sample.Env.t): string =>
  e
  |> List.map((en: Sample.Env.entry) =>
       en.binding.name ++ "=" ++ show_elided(en.value)
     )
  |> String.concat(",");

/* Everything about a sample that is stable across runs. `seq`, `time` and
 * `id` are deliberately excluded: seq and time are minted per sample (seq is
 * the re-use detector below), and `id` is a bounded hash of the stack. */
let show_sample = (s: Sample.t): string =>
  squash(
    Printf.sprintf(
      "%s |stack=%s |frame=%s |env=%s |args=%s |%s |steps=%d-%d",
      show(s.value),
      show_stack(s.call_stack),
      switch (s.frame) {
      | None => "-"
      | Some(f) => show_stack([f])
      },
      show_env(s.env),
      switch (s.args) {
      | None => "-"
      | Some(a) => show_elided(a)
      },
      Sample.show_origin(s.origin),
      s.step_start,
      s.step_end,
    ),
  );

/* Per-syntax-id report over the WHOLE sample map, so `print` statements
 * (which are never in `targets`) are compared alongside probes. The list
 * order is the order EvaluatorState.append built it in, so multiplicity and
 * per-id order are both pinned here. */
let per_id_report = (st: EvaluatorState.t): string =>
  Sample.Map.fold(
    (id, samples, acc) =>
      [
        Printf.sprintf(
          "%s x%d=[%s]",
          short(id),
          List.length(samples),
          String.concat(" ; ", List.map(show_sample, samples)),
        ),
        ...acc,
      ],
    EvaluatorState.get_probes(st),
    [],
  )
  |> List.sort(compare)
  |> String.concat("\n  ");

/* Cross-probe interleaving. A per-id report cannot see the relative order of
 * samples from DIFFERENT probes; this recovers it from the step window each
 * sample carries, which is exactly the timeline EvaluatorState.append
 * rebases a replayed slice onto. */
let timeline_report = (st: EvaluatorState.t): string =>
  Sample.Map.fold(
    (id, samples, acc) =>
      List.map(
        (s: Sample.t) => (s.step_start, s.step_end, short(id), s),
        samples,
      )
      @ acc,
    EvaluatorState.get_probes(st),
    [],
  )
  |> List.sort(((a1, a2, a3, _), (b1, b2, b3, _)) =>
       compare((a1, a2, a3), (b1, b2, b3))
     )
  |> List.map(((st_, en, id, s): (int, int, string, Sample.t)) =>
       Printf.sprintf("%d-%d %s=%s", st_, en, id, show(s.value))
     )
  |> String.concat(" | ");

/* --- running -------------------------------------------------------- */

type run = {
  value: string,
  per_id: string,
  timeline: string,
  /* A sample that a cache entry REPLAYS keeps the seq it was minted with on
   * the earlier run, so a seq predating this run witnesses re-use. Without
   * this a passing comparison is indistinguishable from one where no re-use
   * ever fired. */
  replayed: int,
  total: int,
};

let report_of = ((v: Exp.t, st: EvaluatorState.t), cutoff: int): run => {
  let (replayed, total) =
    Sample.Map.fold(
      (_, samples, (r, t)) =>
        (
          r
          + List.length(
              List.filter((s: Sample.t) => s.seq <= cutoff, samples),
            ),
          t + List.length(samples),
        ),
      EvaluatorState.get_probes(st),
      (0, 0),
    );
  {
    value: show(v),
    per_id: per_id_report(st),
    timeline: timeline_report(st),
    replayed,
    total,
  };
};

let programs_of = (term: Exp.t, edits: list(Exp.t => Exp.t)): list(Exp.t) =>
  List.rev(
    List.fold_left(
      (acc, edit) =>
        switch (acc) {
        | [] => [edit(term)]
        | [prev, ..._] => [edit(prev), ...acc]
        },
      [term],
      edits,
    ),
  );

/* Replay an edit chain under one calculus, threading the cache, and return
 * one `run` per step. Probe targets are taken from the original zipper's
 * refractors against each step's own info_map -- every edit here preserves
 * ids, which is what makes that well defined. */
let runs_under =
    (~calculus: Calculus.t, ~z: Zipper.t, ~programs: list(Exp.t)): list(run) => {
  let prev = ref(IncrEval.empty);
  List.map(
    p => {
      let (info_map, _) = statics_and_elab(p);
      let targets = targets_of_zipper(z, info_map);
      let cutoff = Sample.seq_counter^;
      let (v, st) = eval_under(~calculus, ~prev=prev^, ~targets, p);
      prev := st.incr_eval;
      report_of((v, st), cutoff);
    },
    programs,
  );
};

let with_chain = (~src: string, ~edits: list(Exp.t => Exp.t), k) =>
  switch (Parser.to_zipper(~root=Exp, src)) {
  | None => failwith("could not parse: " ++ src)
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
    k(~z, ~programs=programs_of(term, edits));
  };

/* The oracle. Every available calculus must agree with a0 on the value, on
 * every sample of every probe and print (value, stack, frame, env, args,
 * origin, step window, multiplicity, per-id order) and on the cross-probe
 * interleaving, at every step of the chain. */
let probe_diffs =
    (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t))
    : list(string) =>
  with_chain(
    ~src,
    ~edits,
    (~z, ~programs) => {
      let want = runs_under(~calculus=Calculus.A0, ~z, ~programs);
      /* Non-vacuity: if the program produced no samples at all under a0, the
       * comparison below has tested nothing. Only step 0 is required to have
       * them -- a shape-changing edit may legitimately delete a probe. */
      switch (want) {
      | [{total: 0, _}, ..._] => [name ++ ": NO SAMPLES at step 0"]
      | _ =>
        let diffs = ref([]);
        List.iter(
          (calculus: Calculus.t) => {
            let got = runs_under(~calculus, ~z, ~programs);
            List.iteri(
              (i, g: run) => {
                let w: run = List.nth(want, i);
                let tag =
                  Printf.sprintf(
                    "%s | step %d | %s",
                    name,
                    i,
                    Calculus.name(calculus),
                  );
                if (w.value != g.value) {
                  diffs :=
                    [
                      tag
                      ++ " VALUE\n  want "
                      ++ w.value
                      ++ "\n  got  "
                      ++ g.value,
                      ...diffs^,
                    ];
                };
                if (w.per_id != g.per_id) {
                  diffs :=
                    [
                      tag
                      ++ " SAMPLES\n  want\n  "
                      ++ w.per_id
                      ++ "\n  got\n  "
                      ++ g.per_id,
                      ...diffs^,
                    ];
                };
                if (w.timeline != g.timeline) {
                  diffs :=
                    [
                      tag
                      ++ " INTERLEAVING\n  want "
                      ++ w.timeline
                      ++ "\n  got  "
                      ++ g.timeline,
                      ...diffs^,
                    ];
                };
              },
              got,
            );
          },
          Calculus.available,
        );
        List.rev(diffs^);
      };
    },
  );

let check_probes =
    (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t)) =>
  switch (probe_diffs(~name, ~src, ~edits)) {
  | [] => ()
  | ds => Alcotest.fail(String.concat("\n", ds))
  };

/* As above, and additionally require that re-use ACTUALLY FIRED under the
 * named calculus at some step past the first: a probe comparison that passes
 * because nothing was ever re-used has tested nothing. */
let check_probes_witnessed =
    (
      ~name: string,
      ~src: string,
      ~edits: list(Exp.t => Exp.t),
      ~witness: Calculus.t,
    ) => {
  check_probes(~name, ~src, ~edits);
  with_chain(
    ~src,
    ~edits,
    (~z, ~programs) => {
      let got = runs_under(~calculus=witness, ~z, ~programs);
      let replayed_later =
        switch (got) {
        | []
        | [_] => 0
        | [_, ...rest] =>
          List.fold_left((n, r: run) => n + r.replayed, 0, rest)
        };
      let totals = List.map((r: run) => r.total, got);
      Printf.printf(
        "WITNESS %-56s %s replayed=%d totals=[%s]\n",
        name,
        Calculus.name(witness),
        replayed_later,
        String.concat(",", List.map(string_of_int, totals)),
      );
      check(
        bool,
        name
        ++ ": "
        ++ Calculus.name(witness)
        ++ " actually replayed a cached probe sample",
        true,
        replayed_later > 0,
      );
    },
  );
};

/* ====================================================================
 * 1. MISSING SAMPLES -- the first-order prediction.
 *
 * A probe sample is a side effect of evaluation. When a calculus re-uses a
 * cached value, the evaluation that would have emitted the sample never
 * runs. Does the sample survive?
 * ==================================================================== */

/* The canonical aM win: editing one component of `z` leaves `f(b)` re-usable.
 * `f(b)` IS the probed expression, so a naive cache would simply lose it. */
let test_missing_probe_at_the_reused_node = () =>
  check_probes_witnessed(
    ~name="1a: probe AT the re-used node",
    ~src=
      "let f : Int -> Int = fun n -> n * 2 in
let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
p + q",
    ~edits=[lit(3, 7), lit(7, 5)],
    ~witness=Calculus.AM,
  );

/* The probe is strictly INSIDE the re-used subtree rather than at its root:
 * the entry is for `q`'s right-hand side, the sample is minted one level
 * down. It has to ride along in the entry's state slice. */
let test_missing_probe_inside_the_reused_subtree = () =>
  check_probes_witnessed(
    ~name="1b: probe strictly inside the re-used subtree",
    ~src=
      "let z = (3, 4) in
let (a, b) = z in
let p = (^^probe(a + 1)) * 10 in
let q = (^^probe(b + 1)) * 10 in
p + q",
    ~edits=[lit(3, 7), lit(7, 5)],
    ~witness=Calculus.AM,
  );

/* Probe inside a function BODY, with the call re-used: the sample was minted
 * at a non-empty call stack, so replaying it re-injects a sample the
 * evaluator would otherwise never produce at top level. */
let test_missing_probe_in_reused_call_body = () =>
  check_probes_witnessed(
    ~name="1c: probe in a function body, call re-used",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (3, 4) in
let (a, b) = z in
let p = f(a) in
let q = f(b) in
p * 100 + q",
    ~edits=[lit(3, 7), lit(7, 5)],
    ~witness=Calculus.AM,
  );

/* Two probes nested inside one another inside a re-used subtree. */
let test_missing_nested_probes = () =>
  check_probes_witnessed(
    ~name="1d: nested probes inside a re-used subtree",
    ~src=
      "let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(a + 1) in
let q = ^^probe(^^probe(b + 1) * 10) in
p + q",
    ~edits=[lit(3, 7), lit(7, 5)],
    ~witness=Calculus.AM,
  );

/* ====================================================================
 * 3. MULTIPLICITY -- one probe, N calls.
 * ==================================================================== */

let test_multiplicity_three_calls = () =>
  check_probes_witnessed(
    ~name="3a: one probe, three calls, one component edited",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (3, 4) in
let (a, b) = z in
f(a) + f(b) + f(a)",
    ~edits=[lit(3, 7), lit(7, 5)],
    ~witness=Calculus.AM,
  );

/* The probe fires once per recursion level, so the sample COUNT is a
 * function of the edited value. If re-use collapsed the count, or kept a
 * level from the previous depth alive, this is where it shows. */
let test_multiplicity_recursion_depth_changes = () =>
  check_probes_witnessed(
    ~name="3b: recursion depth changes the sample count",
    ~src=
      "let g : Int -> Int = fun n -> if n < 1 then 0 else ^^probe(n) + g(n - 1) in
let z = (2, 3) in
let (a, b) = z in
g(a) * 100 + g(b)",
    ~edits=[lit(2, 5), lit(5, 1), lit(3, 4)],
    ~witness=Calculus.AM,
  );

/* Same, but the dirtied side is the one whose depth grows, so the re-used
 * side's samples must survive unchanged next to a longer fresh run. */
let test_multiplicity_both_sides = () =>
  check_probes_witnessed(
    ~name="3c: multiplicity with both sides probed",
    ~src=
      "let g : Int -> Int = fun n -> if n < 1 then 0 else ^^probe(n) + g(n - 1) in
let z = (2, 3) in
let (a, b) = z in
let p = ^^probe(g(a)) in
let q = ^^probe(g(b)) in
p * 100 + q",
    ~edits=[lit(2, 6), lit(3, 1)],
    ~witness=Calculus.AM,
  );

/* ====================================================================
 * 4. ORDER AND INTERLEAVING.
 * ==================================================================== */

/* The re-used call comes FIRST, so a replayed slice is spliced before a
 * freshly minted one. EvaluatorState.append prepends `ext` to `base`, so
 * this is where an inverted order would show. */
let test_order_reused_first = () =>
  check_probes_witnessed(
    ~name="4a: re-used call first, fresh call second",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (3, 4) in
let (a, b) = z in
let q = f(b) in
let p = f(a) in
p * 100 + q",
    ~edits=[lit(3, 7), lit(7, 5)],
    ~witness=Calculus.AM,
  );

/* Three probes whose relative order is fixed by evaluation order, with the
 * middle one re-used. Only the interleaving report can see this. */
let test_order_reused_in_the_middle = () =>
  check_probes_witnessed(
    ~name="4b: re-used probe between two fresh ones",
    ~src=
      "let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(a + 1) in
let q = ^^probe(b + 1) in
let r = ^^probe(a + 2) in
p + q + r",
    ~edits=[lit(3, 7), lit(7, 5)],
    ~witness=Calculus.AM,
  );

/* Probes on both sides of a `;` sequence, which fixes their order
 * independently of the let-chain. */
let test_order_across_a_sequence = () =>
  check_probes_witnessed(
    ~name="4c: probes across a sequence, one side re-used",
    ~src=
      "let z = (3, 4) in
let (a, b) = z in
let u = ^^probe(b + 1) in
let v = ^^probe(a + 1) in
let w = ^^probe(b + 2) in
u + v + w",
    ~edits=[lit(3, 7), lit(4, 9), lit(7, 3)],
    ~witness=Calculus.AM,
  );

/* Print the re-use witness for EVERY calculus, so "a2 never re-used
 * anything" cannot masquerade as "a2 is fine". */
let witness_all = (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t)) =>
  with_chain(~src, ~edits, (~z, ~programs) =>
    List.iter(
      (c: Calculus.t) => {
        let got = runs_under(~calculus=c, ~z, ~programs);
        let later =
          switch (got) {
          | []
          | [_] => 0
          | [_, ...rest] =>
            List.fold_left((n, r: run) => n + r.replayed, 0, rest)
          };
        Printf.printf(
          "WITNESSALL %-46s %-6s replayed=%d totals=[%s]\n",
          name,
          Calculus.name(c),
          later,
          String.concat(
            ",",
            List.map((r: run) => string_of_int(r.total), got),
          ),
        );
      },
      Calculus.available,
    )
  );

/* ====================================================================
 * 5. aM's TUPLE FLAGS -- the newest surface.
 *
 * Partial cleanliness means one component of a tuple is re-used while
 * another is re-evaluated. A probe inside the re-used component must still
 * report; a probe inside the re-evaluated one must report the NEW value.
 * ==================================================================== */

/* A probe in each component of the tuple literal itself. The per-component
 * guard in exp_flag's Tuple case decides each independently. */
let test_tuple_probe_in_each_component = () =>
  check_probes_witnessed(
    ~name="5a: probe in each tuple component",
    ~src=
      "let z = (^^probe(1 + 1), ^^probe(2 + 2)) in
let (a, b) = z in
a * 10 + b",
    ~edits=[lit(1, 6), lit(6, 3)],
    ~witness=Calculus.AM,
  );

/* Probes in the components AND downstream of the destructuring, so a
 * partly-clean flag is both produced and consumed with probes on both
 * sides of it. */
let test_tuple_probe_component_and_projection = () =>
  check_probes_witnessed(
    ~name="5b: probes in components and in projections",
    ~src=
      "let f : Int -> Int = fun n -> n * 2 in
let z = (^^probe(3 + 0), ^^probe(4 + 0)) in
let (a, b) = z in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
p * 100 + q",
    ~edits=[lit(3, 7), lit(4, 9), lit(7, 3)],
    ~witness=Calculus.AM,
  );

/* Nested tuples: the inner tuple's flag is a component of the outer's, so
 * `split` is applied to a `Parts` flag that itself came from `norm`. */
let test_tuple_probe_nested = () =>
  check_probes_witnessed(
    ~name="5c: probes inside nested tuples",
    ~src=
      "let z = ((^^probe(1 + 0), ^^probe(2 + 0)), ^^probe(3 + 0)) in
let ((a, b), c) = z in
a * 10000 + b * 100 + c",
    ~edits=[lit(2, 7), lit(1, 9), lit(3, 8)],
    ~witness=Calculus.AM,
  );

/* A tuple whose components are CALLS with probes in the callee body: the
 * re-used component's body samples come out of a cache entry, the dirtied
 * one's are minted fresh, and they have to interleave correctly. */
let test_tuple_probe_components_are_calls = () =>
  check_probes_witnessed(
    ~name="5d: tuple components are probed calls",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (f(3), f(4)) in
let (a, b) = z in
a * 100 + b",
    ~edits=[lit(3, 7), lit(4, 9)],
    ~witness=Calculus.AM,
  );

/* Shape-changing edits under a probe: the component-id guard is what stops a
 * dropped/permuted component from claiming a cached value, and these are the
 * edits that previously broke the VALUE. Check the probe output too. */
let test_tuple_probe_shape_changes = () => {
  check_probes(
    ~name="5e: tuple loses a component, with probes",
    ~src=
      "let z = (^^probe(1 + 0), ^^probe(2 + 0), ^^probe(3 + 0)) in
let (a, b, c) = z in
a * 10000 + b * 100 + c",
    ~edits=[Test_IncrEval.drop_last_tuple_component, lit(1, 9)],
  );
  check_probes(
    ~name="5f: tuple components permuted, with probes",
    ~src=
      "let z = (^^probe(1 + 0), ^^probe(2 + 0)) in
let (a, b) = z in
a * 100 + b",
    ~edits=[Test_IncrEval.swap_tuple_components, lit(1, 9)],
  );
  check_probes(
    ~name="5g: tuple emptied, with probes",
    ~src=
      "let z = (^^probe(1 + 0), ^^probe(2 + 0)) in
let (a, b) = z in
a * 100 + b",
    ~edits=[Test_IncrEval.empty_the_tuple, id_edit],
  );
  check_probes(
    ~name="5h: tuple PATTERN permuted, with probes",
    ~src=
      "let z = (^^probe(1 + 0), ^^probe(2 + 0)) in
let (a, b) = z in
let p = ^^probe(a) in
let q = ^^probe(b) in
p * 100 + q",
    ~edits=[Test_IncrEval.swap_pat_components, lit(1, 9)],
  );
};

/* ====================================================================
 * 6. THE NEW Var AND Parens GUARDS.
 *
 * They force Dirty where the flag used to be Clean. For probes that must be
 * conservative -- MORE re-evaluation, never less, and never a change of
 * sample identity.
 * ==================================================================== */

/* Retyping one character of a name repoints an occurrence at a different
 * binding while its id survives. This was a VALUE bug before the Var guard;
 * here it is checked at probe granularity. */
let test_var_repoint_probe = () =>
  check_probes(
    ~name="6a: repointed variable occurrence, probed",
    ~src=
      "let x = 1 in
let y = 2 in
let a = ^^probe(x) in
let c = ^^probe(a + 10) in
c",
    ~edits=[Test_IncrEval.rename_var_occurrences(~from="x", ~to_="y")],
  );

/* Same, through a tuple, so the repointed occurrence is a tuple COMPONENT
 * and the per-component guard is what has to notice. */
let test_var_repoint_probe_through_tuple = () =>
  check_probes(
    ~name="6b: repointed occurrence inside a probed tuple",
    ~src=
      "let x = 1 in
let y = 2 in
let z = (^^probe(9), ^^probe(x)) in
let (p, q) = z in
let r = ^^probe(q + 10) in
r",
    ~edits=[Test_IncrEval.rename_var_occurrences(~from="x", ~to_="y")],
  );

/* The Parens shape the guard is named for: an id-preserving edit that
 * deletes the node BETWEEN a parenthesis and its surviving child, leaving
 * both the parens' id and the child's id in place. */
let collapse_parens_to_var = (exp: Exp.t): Exp.t => {
  let find_var = (e: Exp.t): option(Exp.t) =>
    switch (e.term) {
    | Var(_) => Some(e)
    | _ =>
      let found = ref(None);
      let f_exp = (continue, c: Exp.t): Exp.t => {
        if (found^ == None) {
          switch (c.term) {
          | Var(_) when !Exp.fast_equal(c, e) => found := Some(c)
          | _ => ()
          };
        };
        continue(c);
      };
      let _ = TermBase.Exp.map_term(~f_exp, e);
      found^;
    };
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Parens(inner) =>
      switch (inner.term, find_var(inner)) {
      | (Var(_), _) => continue(e)
      | (_, Some(v)) => {
          annotation: e.annotation,
          term: Parens(v),
        }
      | (_, None) => continue(e)
      }
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

let test_parens_collapse_probe = () =>
  check_probes(
    ~name="6c: parens child replaced in place, probed",
    ~src=
      "let k = 5 in
let m = 7 in
let a = ^^probe((0 - k)) in
let b = ^^probe(a + m) in
b",
    ~edits=[collapse_parens_to_var],
  );

/* Conservativeness, stated as a test rather than asserted in prose: with NO
 * repointing, the same shape must still re-use (so the guards did not simply
 * turn re-use off), and the probe output must still match a0. */
let test_guards_are_conservative_not_disabling = () =>
  check_probes_witnessed(
    ~name="6d: guards still permit re-use when nothing is repointed",
    ~src=
      "let x = 1 in
let y = 2 in
let z = (x, y) in
let (a, b) = z in
let p = ^^probe(a + 100) in
let q = ^^probe(b + 200) in
p + q",
    ~edits=[lit(1, 6), lit(6, 3)],
    ~witness=Calculus.AM,
  );

/* ====================================================================
 * 7. a2 / aStar -- probes inside callstack-keyed entries.
 * ==================================================================== */

/* a2's own win, with a probe sitting on it: a sub-expression of the body
 * that does NOT depend on the parameter is re-usable inside the call even
 * though the call itself is not. The sample then comes out of an entry keyed
 * at a NON-EMPTY callstack. */
let a2_inner_src = "let k = 5 in
let f : Int -> Int = fun n -> let c = ^^probe(k * 2) in c + n in
let z = (3, 4) in
let (a, b) = z in
f(a) * 100 + f(b)";

let test_a2_probe_inside_call_reused = () =>
  check_probes_witnessed(
    ~name="7a: probe on a parameter-independent body expression",
    ~src=a2_inner_src,
    ~edits=[lit(3, 7), lit(4, 9)],
    ~witness=Calculus.A2,
  );

/* The same program, but the edit invalidates what the inner entry depends
 * on, so every sample must be FRESH and carry the new value. */
let test_a2_probe_inside_call_invalidated = () =>
  check_probes(
    ~name="7b: the inner entry's dependency is edited",
    ~src=a2_inner_src,
    ~edits=[lit(3, 7), lit(5, 6), lit(6, 5)],
  );

/* Two call sites of the same function at DIFFERENT depths, so the same
 * syntactic probe is recorded under two different callstack paths. A sample
 * attributed to the wrong call has the same depth in one case and a
 * different one in the other -- only the frame ids distinguish them. */
let test_a2_probe_two_depths = () =>
  check_probes_witnessed(
    ~name="7c: same probe reached at two callstack depths",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n + 1) in
let g : Int -> Int = fun m -> f(m) * 10 in
let z = (3, 4) in
let (a, b) = z in
f(a) * 1000 + g(b)",
    ~edits=[lit(3, 7), lit(4, 9), lit(7, 3)],
    ~witness=Calculus.AStar,
  );

/* Recursion whose DEPTH changes: run 1 records entries at paths of length
 * 1..2, run 2 walks paths of length 1..5. The entry at a shared path was
 * recorded for a different `n`. If it were re-used, the probe would report
 * the previous run's value at that depth. */
let test_a2_recursion_depth_shifts = () =>
  check_probes(
    ~name="7d: recursion depth changes under callstack keys",
    ~src=
      "let r : Int -> Int = fun n -> if n < 1 then 0 else ^^probe(n) + r(n - 1) in
let z = (2, 1) in
let (a, b) = z in
r(a) * 100 + r(b)",
    ~edits=[lit(2, 5), lit(5, 3), lit(1, 4)],
  );

/* Recursion where the probe's own dependency is CLEAN at every depth while
 * the depth itself changes: the inner entries are the ones a2 would most
 * like to re-use, so this is where a stale one would surface. */
let test_a2_recursion_clean_probe = () =>
  check_probes(
    ~name="7e: clean probe inside recursion whose depth changes",
    ~src=
      "let k = 100 in
let r : Int -> Int = fun n -> if n < 1 then 0 else ^^probe(k + 1) + r(n - 1) in
let z = (2, 1) in
let (a, b) = z in
r(a) * 100 + r(b)",
    ~edits=[lit(2, 5), lit(100, 200), lit(5, 2)],
  );

/* Deeper than Calculus.callstack_depth_limit (4), so the guard refuses to
 * key entries past a point and the probe crosses that boundary. */
let test_a2_beyond_the_depth_guard = () =>
  check_probes(
    ~name="7f: probe beyond the callstack depth guard",
    ~src=
      "let r : Int -> Int = fun n -> if n < 1 then 0 else ^^probe(n) + r(n - 1) in
let z = (7, 2) in
let (a, b) = z in
r(a) * 100 + r(b)",
    ~edits=[lit(7, 8), lit(2, 3), lit(8, 6)],
  );

/* Three steps, so copy_descendant_entries is exercised: step 1 re-uses a
 * call and copies its whole child node forward, step 2 then dirties that
 * call. Stale nested entries kept alive by the copy would surface here. */
let test_a2_copied_descendants_then_dirtied = () =>
  check_probes(
    ~name="7g: re-used call's descendants copied, then dirtied",
    ~src=
      "let f : Int -> Int = fun n -> let t = ^^probe(n * 2) in t + 1 in
let z = (3, 4) in
let (a, b) = z in
let p = f(a) in
let q = f(b) in
p * 100 + q",
    ~edits=[lit(3, 7), lit(4, 9), lit(7, 3), lit(9, 4)],
  );

/* ====================================================================
 * 8. PROBES INSIDE A RE-USED CLOSURE BODY.
 * ==================================================================== */

/* The closure is built by a call, so its body's free variable comes from a
 * captured environment rather than the top-level one. */
let test_closure_body_probe = () =>
  check_probes_witnessed(
    ~name="8a: probe in the body of a returned closure",
    ~src=
      "let mk : Int -> (Int -> Int) = fun m -> fun n -> ^^probe(m + n) in
let g = mk(10) in
let z = (3, 4) in
let (a, b) = z in
g(a) * 100 + g(b)",
    ~edits=[lit(3, 7), lit(4, 9), lit(10, 20)],
    ~witness=Calculus.AM,
  );

/* A closure stored in a TUPLE component, so transport_across_env has to
 * decide whether the caller's provenance survives into the body, and the
 * tuple's own flag has a function-valued component. */
let test_closure_in_tuple_probe = () =>
  check_probes(
    ~name="8b: closure as a tuple component, probed body",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (f, 4) in
let (h, b) = z in
h(3) + h(b)",
    ~edits=[lit(4, 9), lit(3, 6), lit(9, 4)],
  );

/* A closure passed as an argument and applied inside another function, so
 * the probe fires two callstack frames down through a value the caller
 * supplied. */
let test_closure_passed_and_applied = () =>
  check_probes_witnessed(
    ~name="8c: closure passed in and applied, probed body",
    ~src=
      "let ap : (Int -> Int, Int) -> Int = fun (h, x) -> h(x) in
let f : Int -> Int = fun n -> ^^probe(n * 3) in
let z = (3, 4) in
let (a, b) = z in
ap((f, a)) * 100 + ap((f, b))",
    ~edits=[lit(3, 7), lit(4, 9), lit(7, 3)],
    ~witness=Calculus.AM,
  );

/* Recursive closure, probe in the body, with the function's own definition
 * edited so the closure itself changes identity. */
let test_closure_definition_edited = () =>
  check_probes(
    ~name="8d: the probed closure's own definition is edited",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (3, 4) in
let (a, b) = z in
f(a) * 100 + f(b)",
    ~edits=[lit(2, 5), lit(3, 7), lit(5, 2)],
  );

let test_witnesses = () => {
  witness_all(
    ~name="a2 inner re-use",
    ~src=a2_inner_src,
    ~edits=[lit(3, 7), lit(4, 9)],
  );
  witness_all(
    ~name="probe in fn body",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (3, 4) in
let (a, b) = z in
f(a) * 100 + f(b)",
    ~edits=[lit(3, 7), lit(4, 9)],
  );
};

/* ====================================================================
 * 2. STALE SAMPLES, made visible where the VALUE cannot see them.
 *
 * Every test above would also catch a stale replay if the replayed value
 * differed. These are built so the value is IDENTICAL across the edit and
 * only the sample's other fields move -- which is precisely the regime the
 * `hazel bench-incr` oracle, and any value-only comparison, is blind to.
 * ==================================================================== */

/* `b * 0 + 7` is 7 whatever b is, but the probe's capture spec collects the
 * refs IN the probed expression, so the sample's env records b. A replayed
 * sample would show the previous b. */
let test_stale_env_only = () =>
  check_probes(
    ~name="2a: value constant, captured env changes",
    ~src=
      "let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(b * 0 + 7) in
let q = ^^probe(a * 0 + 8) in
p + q",
    ~edits=[lit(4, 9), lit(3, 5), lit(9, 4)],
  );

/* A probe on an application records the ARGUMENT value in `args`. The
 * function ignores it, so the sample's value never moves. */
let test_stale_args_only = () =>
  check_probes(
    ~name="2b: value constant, recorded argument changes",
    ~src=
      "let f : Int -> Int = fun n -> 7 in
let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(f(b)) in
let q = ^^probe(f(a)) in
p + q",
    ~edits=[lit(4, 9), lit(3, 5), lit(9, 4)],
  );

/* Constant-valued probe inside a function body whose argument changes: the
 * sample's env records the parameter. */
let test_stale_env_in_call = () =>
  check_probes(
    ~name="2c: constant probe in a body, parameter changes",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 0 + 7) in
let z = (3, 4) in
let (a, b) = z in
f(a) + f(b)",
    ~edits=[lit(4, 9), lit(3, 5)],
  );

/* Long chain over one program so an entry has many chances to survive edits
 * it should not survive. Values return to earlier ones deliberately. */
let test_stale_long_chain = () =>
  check_probes(
    ~name="2d: long edit chain over one probed program",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
let r = ^^probe(p + q) in
r",
    ~edits=[
      lit(3, 7),
      lit(4, 9),
      lit(7, 3),
      lit(9, 4),
      lit(3, 5),
      lit(4, 6),
      lit(5, 3),
    ],
  );

/* ====================================================================
 * A randomized net over the generator from Test_IncrEval, read through the
 * probe oracle rather than the value oracle. The generated programs already
 * sprinkle `^^probe`, `print` and `test` through top-level chains and
 * function bodies.
 * ==================================================================== */

let fuzz_probes =
    (
      ~seeds: list(int),
      ~binds: int,
      ~edits: int,
      ~mk: (~n: int, list(int)) => list(Exp.t => Exp.t),
      ~tag: string,
    ) => {
  let failures = ref([]);
  let no_samples = ref(0);
  List.iter(
    seed => {
      Test_IncrEval.Fuzz.reset(seed);
      Test_IncrEval.Fuzz.lit_pool := 0;
      Test_IncrEval.Fuzz.names := 0;
      let src = Test_IncrEval.Fuzz.gen_program(~binds);
      let present = List.rev(Test_IncrEval.Fuzz.lits^);
      let name = Printf.sprintf("%s-seed-%d", tag, seed);
      let es = mk(~n=edits, present);
      let ds =
        try(probe_diffs(~name, ~src, ~edits=es)) {
        | e => [name ++ " RAISED " ++ Printexc.to_string(e)]
        };
      switch (ds) {
      | [] => ()
      | [one] when one == name ++ ": NO SAMPLES at step 0" =>
        no_samples := no_samples^ + 1
      | ds =>
        Printf.printf("FUZZSRC %s\n%s\n", name, src);
        failures := List.rev(ds) @ failures^;
      };
    },
    seeds,
  );
  Printf.printf(
    "PROBEFUZZ %s: %d seeds, %d with no samples (skipped), %d diffs\n",
    tag,
    List.length(seeds),
    no_samples^,
    List.length(failures^),
  );
  switch (List.rev(failures^)) {
  | [] => ()
  | fs => Alcotest.fail(String.concat("\n", fs))
  };
};

let test_probe_fuzz_literal = () =>
  fuzz_probes(
    ~seeds=Test_IncrEval.range(1, 120),
    ~binds=7,
    ~edits=4,
    ~mk=Test_IncrEval.fuzz_edits,
    ~tag="probefuzz",
  );

let test_probe_fuzz_structural = () =>
  fuzz_probes(
    ~seeds=Test_IncrEval.range(1, 120),
    ~binds=7,
    ~edits=2,
    ~mk=Test_IncrEval.fuzz_structural_edits,
    ~tag="probefuzzS",
  );

/* ====================================================================
 * PROBE-ALL: the densest oracle available, and a real user-facing mode.
 *
 * CachedStatics.compute_targets with settings.probe_all targets EVERY
 * expression and EVERY PATTERN in the info_map (`all_probeable_ids`), which
 * is what the "probe all" toggle in the nut menu does. Two things follow
 * that make it worth running separately from the hand-placed probes above:
 *
 *  - pattern ids become targets, so samples are minted from
 *    RecordPatMatch's `samples` closures rather than from an expression's
 *    observation span. That is a different minting path entirely, and it is
 *    the path a re-used `let` skips.
 *  - EvalInfo.equal_probe_targets is (ProbeAll, ProbeAll) => true, so the
 *    probe-target premise of reuse_check is DISABLED in this mode: a re-use
 *    that a change of probe targets would otherwise have blocked goes
 *    through.
 * ==================================================================== */

let probe_all_settings: CoreSettings.t = {
  ...CoreSettings.on,
  probe_all: true,
};

let eval_probe_all =
    (
      ~calculus: Calculus.t,
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      exp: Exp.t,
    )
    : (Exp.t, EvaluatorState.t) => {
  let (info_map, elab) = statics_and_elab(exp);
  let targets =
    CachedStatics.compute_targets(
      ~settings=probe_all_settings,
      ~info_map,
      ~probe_ids=Id.Map.empty,
    );
  let eval_info = EvalInfo.of_info_map(~probe_all=true, ~targets, info_map);
  Evaluator.evaluate(
    ~calculus,
    ~prev,
    ~eval_info,
    ~env=Builtins.env_init,
    elab,
  );
};

let runs_probe_all =
    (~calculus: Calculus.t, ~programs: list(Exp.t)): list(run) => {
  let prev = ref(IncrEval.empty);
  List.map(
    p => {
      let cutoff = Sample.seq_counter^;
      let (v, st) = eval_probe_all(~calculus, ~prev=prev^, p);
      prev := st.incr_eval;
      report_of((v, st), cutoff);
    },
    programs,
  );
};

let probe_all_diffs =
    (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t))
    : list(string) => {
  let programs = programs_of(parse_exp(src), edits);
  let want = runs_probe_all(~calculus=Calculus.A0, ~programs);
  switch (want) {
  | [{total: 0, _}, ..._] => [name ++ ": NO SAMPLES at step 0"]
  | _ =>
    let diffs = ref([]);
    List.iter(
      (calculus: Calculus.t) => {
        let got = runs_probe_all(~calculus, ~programs);
        List.iteri(
          (i, g: run) => {
            let w: run = List.nth(want, i);
            let tag =
              Printf.sprintf(
                "%s | step %d | %s | probe_all",
                name,
                i,
                Calculus.name(calculus),
              );
            if (w.value != g.value) {
              diffs :=
                [
                  tag ++ " VALUE\n  want " ++ w.value ++ "\n  got  " ++ g.value,
                  ...diffs^,
                ];
            };
            if (w.per_id != g.per_id) {
              diffs :=
                [
                  tag
                  ++ " SAMPLES\n  want\n  "
                  ++ w.per_id
                  ++ "\n  got\n  "
                  ++ g.per_id,
                  ...diffs^,
                ];
            };
            if (w.timeline != g.timeline) {
              diffs :=
                [
                  tag
                  ++ " INTERLEAVING\n  want "
                  ++ w.timeline
                  ++ "\n  got  "
                  ++ g.timeline,
                  ...diffs^,
                ];
            };
          },
          got,
        );
      },
      Calculus.available,
    );
    List.rev(diffs^);
  };
};

let check_probe_all =
    (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t)) =>
  switch (probe_all_diffs(~name, ~src, ~edits)) {
  | [] => ()
  | ds => Alcotest.fail(String.concat("\n", ds))
  };

let probe_all_corpus = [
  (
    "PA tuple destructured",
    "let z = (3, 4) in
let (a, b) = z in
a * 100 + b",
    [lit(3, 7), lit(4, 9), lit(7, 3)],
  ),
  (
    "PA tuple through a call",
    "let f : Int -> Int = fun n -> n * 2 in
let z = (3, 4) in
let (a, b) = z in
f(a) * 100 + f(b)",
    [lit(3, 7), lit(4, 9), lit(7, 3)],
  ),
  (
    "PA parameter-independent body expression",
    "let k = 5 in
let f : Int -> Int = fun n -> let c = k * 2 in c + n in
let z = (3, 4) in
let (a, b) = z in
f(a) * 100 + f(b)",
    [lit(3, 7), lit(5, 6), lit(4, 9)],
  ),
  (
    "PA recursion whose depth changes",
    "let r : Int -> Int = fun n -> if n < 1 then 0 else n + r(n - 1) in
let z = (2, 1) in
let (a, b) = z in
r(a) * 100 + r(b)",
    [lit(2, 5), lit(5, 3), lit(1, 4)],
  ),
  (
    "PA nested tuples",
    "let z = ((1, 2), (3, 4)) in
let ((a, b), (c, d)) = z in
a * 1000 + b * 100 + c * 10 + d",
    [lit(4, 9), lit(1, 6), lit(9, 4)],
  ),
  (
    "PA closure returned and applied",
    "let mk : Int -> (Int -> Int) = fun m -> fun n -> m + n in
let g = mk(10) in
let z = (3, 4) in
let (a, b) = z in
g(a) * 100 + g(b)",
    [lit(3, 7), lit(10, 20), lit(4, 9)],
  ),
  (
    "PA labeled tuple",
    "let z : (a = Int, b = Int) = (a = 1, b = 2) in
let (a = p, b = q) = z in
p * 100 + q",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "PA case over a tuple",
    "let z = (1, 2) in
case z
  | (0, y) => y
  | (x, y) => x * 100 + y
end",
    [lit(2, 7), lit(1, 0), lit(0, 5)],
  ),
  (
    "PA shadowing through a tuple",
    "let z = (1, 2) in
let (a, b) = z in
let c = a in
let a = b in
let d = a in
c * 100 + d",
    [lit(1, 5), lit(2, 7), lit(5, 1)],
  ),
  (
    "PA print and test alongside probes",
    "let z = (1, 2) in
let (a, b) = z in
let _ = print(a) in
test b < 9 end;
a * 100 + b",
    [lit(1, 5), lit(2, 7)],
  ),
];

let test_probe_all_corpus = () =>
  List.iter(
    ((name, src, edits)) => check_probe_all(~name, ~src, ~edits),
    probe_all_corpus,
  );

/* Shape-changing edits under probe_all, where pattern targets and the
 * per-component tuple guard meet. */
let test_probe_all_shape_changes = () => {
  check_probe_all(
    ~name="PA tuple loses a component",
    ~src="let z = (1, 2, 3) in
let (a, b, c) = z in
a * 10000 + b * 100 + c",
    ~edits=[Test_IncrEval.drop_last_tuple_component, lit(1, 9)],
  );
  check_probe_all(
    ~name="PA tuple permuted",
    ~src="let z = (1, 2) in
let (a, b) = z in
a * 100 + b",
    ~edits=[Test_IncrEval.swap_tuple_components, lit(1, 9)],
  );
  check_probe_all(
    ~name="PA tuple pattern permuted",
    ~src="let z = (1, 2) in
let (a, b) = z in
a * 100 + b",
    ~edits=[Test_IncrEval.swap_pat_components, lit(1, 9)],
  );
  check_probe_all(
    ~name="PA variable occurrence repointed",
    ~src="let x = 1 in
let y = 2 in
let a = x in
a",
    ~edits=[Test_IncrEval.rename_var_occurrences(~from="x", ~to_="y")],
  );
  check_probe_all(
    ~name="PA literal collapses to a hole",
    ~src="let z = (1, 2) in
let (a, b) = z in
a * 100 + b",
    ~edits=[Test_IncrEval.blank_int_lit(~from=1), id_edit],
  );
};

/* Witness that probe_all is dense and that re-use still fires under it. */
let test_probe_all_witness = () => {
  let programs =
    programs_of(
      parse_exp(
        "let f : Int -> Int = fun n -> n * 2 in
let z = (3, 4) in
let (a, b) = z in
f(a) * 100 + f(b)",
      ),
      [lit(3, 7), lit(4, 9)],
    );
  List.iter(
    (c: Calculus.t) => {
      let got = runs_probe_all(~calculus=c, ~programs);
      let later =
        switch (got) {
        | []
        | [_] => 0
        | [_, ...rest] =>
          List.fold_left((n, r: run) => n + r.replayed, 0, rest)
        };
      Printf.printf(
        "PROBEALL %-6s replayed=%d totals=[%s]\n",
        Calculus.name(c),
        later,
        String.concat(
          ",",
          List.map((r: run) => string_of_int(r.total), got),
        ),
      );
    },
    Calculus.available,
  );
  let a0 = runs_probe_all(~calculus=Calculus.A0, ~programs);
  check(
    bool,
    "probe_all really samples a lot of points",
    true,
    switch (a0) {
    | [r, ..._] => r.total > 15
    | [] => false
    },
  );
};

let fuzz_probe_all =
    (
      ~seeds: list(int),
      ~binds: int,
      ~edits: int,
      ~mk: (~n: int, list(int)) => list(Exp.t => Exp.t),
      ~tag: string,
    ) => {
  let failures = ref([]);
  List.iter(
    seed => {
      Test_IncrEval.Fuzz.reset(seed);
      Test_IncrEval.Fuzz.lit_pool := 0;
      Test_IncrEval.Fuzz.names := 0;
      let src = Test_IncrEval.Fuzz.gen_program(~binds);
      let present = List.rev(Test_IncrEval.Fuzz.lits^);
      let name = Printf.sprintf("%s-seed-%d", tag, seed);
      let ds =
        try(probe_all_diffs(~name, ~src, ~edits=mk(~n=edits, present))) {
        | e => [name ++ " RAISED " ++ Printexc.to_string(e)]
        };
      switch (ds) {
      | [] => ()
      | ds =>
        Printf.printf("FUZZSRC %s\n%s\n", name, src);
        failures := List.rev(ds) @ failures^;
      };
    },
    seeds,
  );
  Printf.printf(
    "PROBEALLFUZZ %s: %d seeds, %d diffs\n",
    tag,
    List.length(seeds),
    List.length(failures^),
  );
  switch (List.rev(failures^)) {
  | [] => ()
  | fs => Alcotest.fail(String.concat("\n", fs))
  };
};

let test_probe_all_fuzz = () =>
  fuzz_probe_all(
    ~seeds=Test_IncrEval.range(1, 50),
    ~binds=6,
    ~edits=3,
    ~mk=Test_IncrEval.fuzz_edits,
    ~tag="pafuzz",
  );

let test_probe_all_fuzz_structural = () =>
  fuzz_probe_all(
    ~seeds=Test_IncrEval.range(1, 50),
    ~binds=6,
    ~edits=2,
    ~mk=Test_IncrEval.fuzz_structural_edits,
    ~tag="pafuzzS",
  );

/* ====================================================================
 * Minimization support for a fuzz hit. Not a test in itself.
 * ==================================================================== */

/* Per-id sample lists, so a diff can name exactly which ids moved instead of
 * printing the whole map. */
let per_id_assoc = (st: EvaluatorState.t): list((string, list(string))) =>
  Sample.Map.fold(
    (id, samples, acc) =>
      [(short(id), List.map(show_sample, samples)), ...acc],
    EvaluatorState.get_probes(st),
    [],
  )
  |> List.sort(compare);

let compact_sample = (s: Sample.t): string =>
  squash(
    Printf.sprintf(
      "%s@[%s]/%d-%d",
      show(s.value),
      show_stack(s.call_stack),
      s.step_start,
      s.step_end,
    ),
  );

let per_id_compact = (st: EvaluatorState.t): list((string, list(string))) =>
  Sample.Map.fold(
    (id, samples, acc) =>
      [(short(id), List.map(compact_sample, samples)), ...acc],
    EvaluatorState.get_probes(st),
    [],
  )
  |> List.sort(compare);

/* Only the ids whose sample list differs, rendered compactly. */
let sample_id_diffs =
    (a: EvaluatorState.t, b: EvaluatorState.t): list(string) => {
  let la = per_id_compact(a);
  let lb = per_id_compact(b);
  let ids = List.sort_uniq(compare, List.map(fst, la) @ List.map(fst, lb));
  List.filter_map(
    id => {
      let get = l =>
        switch (List.assoc_opt(id, l)) {
        | Some(v) => v
        | None => []
        };
      let (x, y) = (get(la), get(lb));
      x == y
        ? None
        : Some(
            Printf.sprintf(
              "  id %s\n    a0 x%d=[%s]\n    got x%d=[%s]",
              id,
              List.length(x),
              String.concat(" ; ", x),
              List.length(y),
              String.concat(" ; ", y),
            ),
          );
    },
    ids,
  );
};

/* Does `calculus` differ from a0 on SAMPLES (not value) anywhere along the
 * chain, under probe_all? */
let probe_all_sample_divergence =
    (~calculus: Calculus.t, ~src: string, ~edits: list(Exp.t => Exp.t)): bool =>
  try({
    let programs = programs_of(parse_exp(src), edits);
    let want = runs_probe_all(~calculus=Calculus.A0, ~programs);
    switch (want) {
    | [{total: 0, _}, ..._] => false
    | _ =>
      let got = runs_probe_all(~calculus, ~programs);
      List.exists2(
        (w: run, g: run) => w.value == g.value && w.per_id != g.per_id,
        want,
        got,
      );
    };
  }) {
  | _ => false
  };

let split_lines = (s: string): list(string) =>
  String.split_on_char('\n', s);

let minimize_probe_all =
    (~calculus: Calculus.t, ~src: string, ~edits: list(Exp.t => Exp.t))
    : string => {
  let cur = ref(split_lines(src));
  let changed = ref(true);
  while (changed^) {
    changed := false;
    let i = ref(0);
    while (i^ < List.length(cur^)) {
      let candidate =
        List.filteri((j, _) => j != i^, cur^) |> String.concat("\n");
      if (probe_all_sample_divergence(~calculus, ~src=candidate, ~edits)) {
        cur := split_lines(candidate);
        changed := true;
      } else {
        i := i^ + 1;
      };
    };
  };
  String.concat("\n", cur^);
};

let report_probe_all_seed = (~seed: int, ~binds: int, ~edits_n: int) => {
  Test_IncrEval.Fuzz.reset(seed);
  Test_IncrEval.Fuzz.lit_pool := 0;
  Test_IncrEval.Fuzz.names := 0;
  let src = Test_IncrEval.Fuzz.gen_program(~binds);
  let present = List.rev(Test_IncrEval.Fuzz.lits^);
  let edits = Test_IncrEval.fuzz_structural_edits(~n=edits_n, present);
  Printf.printf("### seed %d\n", seed);
  List.iter(
    (c: Calculus.t) =>
      Printf.printf(
        "  %s sample-only divergence: %b\n",
        Calculus.name(c),
        probe_all_sample_divergence(~calculus=c, ~src, ~edits),
      ),
    Calculus.available,
  );
  let m = minimize_probe_all(~calculus=Calculus.APL, ~src, ~edits);
  Printf.printf("MINIMIZED seed %d:\n%s\n", seed, m);
  let programs = programs_of(parse_exp(m), edits);
  List.iteri(
    (i, p) => Printf.printf("  src %d: %s\n", i, show(p)),
    programs,
  );
  let prev0 = ref(IncrEval.empty);
  let prevA = ref(IncrEval.empty);
  List.iteri(
    (i, p) => {
      let (v0, s0) = eval_probe_all(~calculus=Calculus.A0, ~prev=prev0^, p);
      let (va, sa) = eval_probe_all(~calculus=Calculus.APL, ~prev=prevA^, p);
      prev0 := s0.incr_eval;
      prevA := sa.incr_eval;
      let ds = sample_id_diffs(s0, sa);
      Printf.printf(
        "  step %d value a0=%s aPL=%s ; %d differing ids\n%s\n",
        i,
        show(v0),
        show(va),
        List.length(ds),
        String.concat("\n", ds),
      );
    },
    programs,
  );
};

let dump_probe_all =
    (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t)) => {
  let programs = programs_of(parse_exp(src), edits);
  Printf.printf("=== %s ===\n", name);
  List.iteri(
    (i, p) => Printf.printf("  src %d: %s\n", i, show(p)),
    programs,
  );
  let prev0 = ref(IncrEval.empty);
  let prevs = List.map(_ => ref(IncrEval.empty), Calculus.available);
  List.iteri(
    (i, p) => {
      let (v0, s0) = eval_probe_all(~calculus=Calculus.A0, ~prev=prev0^, p);
      prev0 := s0.incr_eval;
      List.iteri(
        (j, c: Calculus.t) => {
          let pr = List.nth(prevs, j);
          let (v, st) = eval_probe_all(~calculus=c, ~prev=pr^, p);
          pr := st.incr_eval;
          let ds = sample_id_diffs(s0, st);
          if (ds != [] || show(v) != show(v0)) {
            Printf.printf(
              "  step %d %-6s value a0=%s got=%s ; %d differing ids\n%s\n",
              i,
              Calculus.name(c),
              show(v0),
              show(v),
              List.length(ds),
              String.concat("\n", ds),
            );
          };
        },
        Calculus.available,
      );
    },
    programs,
  );
};

/* The same dump, but with ordinary hand-placed probes/prints rather than
 * probe_all, i.e. what a user actually gets. */
let dump_hand = (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t)) =>
  with_chain(
    ~src,
    ~edits,
    (~z, ~programs) => {
      Printf.printf("=== %s ===\n", name);
      List.iteri(
        (i, p) => Printf.printf("  src %d: %s\n", i, show(p)),
        programs,
      );
      let want = runs_under(~calculus=Calculus.A0, ~z, ~programs);
      List.iter(
        (c: Calculus.t) => {
          let got = runs_under(~calculus=c, ~z, ~programs);
          List.iteri(
            (i, g: run) => {
              let w: run = List.nth(want, i);
              if (w.per_id != g.per_id
                  || w.timeline != g.timeline
                  || w.value != g.value) {
                Printf.printf(
                  "  step %d %-6s\n    a0 value=%s timeline=%s\n    got value=%s timeline=%s\n",
                  i,
                  Calculus.name(c),
                  w.value,
                  w.timeline,
                  g.value,
                  g.timeline,
                );
              };
            },
            got,
          );
        },
        Calculus.available,
      );
      ();
    },
  );

/* Mint a fresh id for the SECOND component of every 2-tuple, leaving the
 * term itself alone. This is what destroying and retyping a token does: the
 * text is the same, the node is new. `Exp.fast_equal` ignores ids, so the
 * cache's entry at the tuple's id still compares equal to the new
 * elaboration. */
let refresh_second_component = (exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Tuple([a, b]) => {
        annotation: e.annotation,
        term:
          Tuple([
            continue(a),
            {
              ...b,
              annotation: IdTagged.IdTag.fresh(),
            },
          ]),
      }
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

let test_probe_fresh_id_component = () => {
  dump_probe_all(
    ~name="F1 second component gets a fresh id, same text",
    ~src="let v = (2, 2) in
v",
    ~edits=[refresh_second_component],
  );
  dump_probe_all(
    ~name="F2 same, components are calls",
    ~src="let f : Int -> Int = fun n -> n + 1 in
let v = (f(3), f(3)) in
v",
    ~edits=[refresh_second_component],
  );
  dump_probe_all(
    ~name="F3 same, inside a destructuring",
    ~src="let v = (2, 2) in
let (a, b) = v in
a + b",
    ~edits=[refresh_second_component],
  );
};

let test_probe_order_user_visible = () => {
  let swap = Test_IncrEval.swap_tuple_components;
  dump_hand(
    ~name="U1 two prints in a tuple, components swapped",
    ~src="let v = (print(1), print(2)) in
v",
    ~edits=[swap],
  );
  dump_hand(
    ~name="U2 two probes on equal-valued calls, components swapped",
    ~src=
      "let f : Int -> Int = fun n -> 7 in
let v = (^^probe(f(1)), ^^probe(f(2))) in
v",
    ~edits=[swap],
  );
  dump_hand(
    ~name="U3 prints with a following edit",
    ~src="let w = 9 in
let v = (print(1), print(2)) in
w",
    ~edits=[swap, lit(9, 8)],
  );
  dump_hand(
    ~name="U4 prints inside a destructured tuple",
    ~src="let v = (print(1), print(2)) in
let (a, b) = v in
a",
    ~edits=[swap],
  );
  dump_hand(
    ~name="U5 probes on equal literals",
    ~src="let v = (^^probe(2), ^^probe(2)) in
v",
    ~edits=[swap],
  );
  dump_hand(
    ~name="U6 sequence of prints, no tuple",
    ~src="let _ = print(1) in
let _ = print(2) in
0",
    ~edits=[id_edit],
  );
};
let test_probe_all_handmin = () => {
  let swap = Test_IncrEval.swap_tuple_components;
  dump_probe_all(
    ~name="H1 two variable components, swapped",
    ~src="let k = 2 in
let v = (k, k) in
v",
    ~edits=[swap],
  );
  dump_probe_all(
    ~name="H2 two distinct variable components, swapped",
    ~src="let k = 2 in
let m = 5 in
let v = (k, m) in
v",
    ~edits=[swap],
  );
  dump_probe_all(
    ~name="H3 literal components, swapped",
    ~src="let v = (2, 5) in
v",
    ~edits=[swap],
  );
  dump_probe_all(
    ~name="H4 compound components, swapped",
    ~src="let v = (2 + 0, 5 + 0) in
v",
    ~edits=[swap],
  );
  dump_probe_all(
    ~name="H5 variable components, destructured",
    ~src=
      "let k = 2 in
let m = 5 in
let v = (k, m) in
let (a, b) = v in
a * 100 + b",
    ~edits=[swap],
  );
  dump_probe_all(
    ~name="H6 swap then an unrelated literal edit",
    ~src="let k = 2 in
let m = 5 in
let w = 9 in
let v = (k, m) in
v",
    ~edits=[swap, lit(9, 8)],
  );
};
let test_probe_all_minimize = () => {
  report_probe_all_seed(~seed=33, ~binds=6, ~edits_n=2);
  report_probe_all_seed(~seed=38, ~binds=6, ~edits_n=2);
};

/* ====================================================================
 * FINDINGS. Both root-cause to one line:
 *
 *   src/language/dynamics/IncrEval.re:759
 *     let elab_same = Exp.fast_equal(entry.prev_elab, info.elab_term);
 *
 * `Exp.fast_equal` is `Equality.equality(syntactic_settings)`, and those
 * settings compare no ids at all. So an entry is re-used whenever the
 * elaboration at this id is structurally the same -- even when the IDENTITIES
 * of the nodes inside it have changed. The entry's `value` survives that
 * (the value really is the same), but the entry's `state` does not: probe
 * samples are keyed BY SYNTAX ID and carry call-stack frames that are
 * application IDS, so replaying the slice re-injects samples attributed to
 * the previous run's node identities.
 *
 * Neither finding is specific to aM. aPL, aM, a2 and aStar all reproduce
 * them identically, which is what the per-calculus tables below record.
 * ==================================================================== */

/* FINDING P1 (soundness, probe ORDER; all incremental calculi).
 *
 * Permuting two equal-valued sibling components leaves the enclosing value
 * unchanged, so the enclosing entry hits; the replayed slice carries the
 * step windows the components had in their OLD positions. a0 evaluates
 * left-to-right in the new order and attributes them the other way round, so
 * the two probes are transposed on the timeline.
 *
 * a0     timeline: 1-1 <left id>=2 | 2-2 <right id>=2
 * aPL/aM/a2/aStar: 1-1 <right id>=2 | 2-2 <left id>=2   */
let test_pin_probe_order_transposed = () =>
  check_probes(
    ~name="P1: equal-valued components permuted, probe timeline transposed",
    ~src="let v = (^^probe(2), ^^probe(2)) in
v",
    ~edits=[Test_IncrEval.swap_tuple_components],
  );

/* FINDING P2 (soundness, probe IDENTITY; all incremental calculi).
 *
 * A component is destroyed and retyped with the same text, so its node gets
 * a fresh id while the tuple's own id survives -- which is exactly what the
 * id-preserving edit machinery is for. `fast_equal` cannot see the change,
 * so the tuple's entry hits and its slice is replayed, producing:
 *   - a sample attributed to a syntax id that IS NOT IN THE PROGRAM, and
 *   - NO sample at the id that is.
 * Run under probe_all, which is the shipped "probe everything" mode. */
let test_pin_probe_sample_at_dead_id = () =>
  check_probe_all(
    ~name="P2: sample attributed to a node that no longer exists",
    ~src="let v = (2, 2) in
v",
    ~edits=[refresh_second_component],
  );

/* FINDING P2b: the same defect reaching the CALL STACK. The retyped
 * component is an application, so its app id changes; every sample minted
 * inside that call is replayed carrying the PREVIOUS run's frame id, i.e.
 * attributed to a call site that no longer exists. This is the "does the
 * probe attribute its sample to the right call" question, answered no.
 *
 * a0  : 3@[f@c6e000]  (the current application's id)
 * got : 3@[f@bee000]  (the previous run's application id) */
let test_pin_probe_stale_call_frame = () =>
  check_probe_all(
    ~name="P2b: replayed samples carry the previous run's call frame",
    ~src="let f : Int -> Int = fun n -> n + 1 in
let v = (f(3), f(3)) in
v",
    ~edits=[refresh_second_component],
  );

/* FINDING P2c: the same, with the tuple destructured downstream, to show it
 * is not an artifact of the tuple being the whole program. */
let test_pin_probe_dead_id_destructured = () =>
  check_probe_all(
    ~name="P2c: dead-id sample with the tuple destructured",
    ~src="let v = (2, 2) in
let (a, b) = v in
a + b",
    ~edits=[refresh_second_component],
  );

/* FINDING P2d: P2 does NOT need the two siblings to be equal-valued. The
 * only thing `fast_equal` has to miss is the change of identity, so a
 * retyped component with unchanged TEXT is enough, whatever its neighbours
 * are. (P1, by contrast, does need them equal-valued: a permutation of
 * unequal components changes the enclosing value and the entry misses.) */
let test_pin_probe_dead_id_distinct_siblings = () =>
  check_probe_all(
    ~name="P2d: dead-id sample with distinct sibling components",
    ~src="let v = (2, 5) in
v",
    ~edits=[refresh_second_component],
  );

/* Controls. These PASS, and they are what localizes the defect to id-blind
 * structural equality rather than to re-use in general:
 *
 *  - permuting components that are NOT equal-valued changes the enclosing
 *    value, the entry misses, and the timeline is right;
 *  - giving a component a fresh id AND different text makes `fast_equal`
 *    notice, the entry misses, and the samples are right.
 *
 * So it is precisely "same text, different node" that the cache cannot see.
 */
let test_control_distinct_components = () =>
  check_probes(
    ~name="control: distinct components permuted",
    ~src="let v = (^^probe(2), ^^probe(5)) in
v",
    ~edits=[Test_IncrEval.swap_tuple_components],
  );

let test_control_fresh_id_and_new_text = () =>
  check_probe_all(
    ~name="control: fresh id AND changed text",
    ~src="let v = (2, 5) in
v",
    ~edits=[
      e =>
        Test_IncrEval.replace_int_lit(
          ~from=5,
          ~to_=8,
          refresh_second_component(e),
        ),
    ],
  );

/* ====================================================================
 * Is the edit P2 needs one the EDITOR actually performs?
 *
 * P2's premise is that an ordinary edit can mint a fresh id for a
 * sub-expression while the enclosing node keeps its id. That is precisely
 * what IdMatch is for, and IdMatch.re:31 states the assumption this audit
 * is testing:
 *
 *   "Id assignment is a performance concern only: IncrEval.reuse_check
 *    re-checks `Exp.fast_equal(entry.prev_elab, info.elab_term)` before
 *    using a cache entry, so a wrong match costs a wasted lookup, not a
 *    wrong answer."
 *
 * So rather than assert the premise, drive the real Zipper the way
 * CLI/BenchIncr does and read the ids off the result.
 * ==================================================================== */

let perform = (z: Zipper.t, a: Action.t): Zipper.t => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    CachedStatics.init_from_term(
      ~settings=CoreSettings.on,
      ~is_dynamic_term=true,
      term,
    );
  switch (
    Perform.go(
      ~settings=CoreSettings.on,
      ~statics,
      ~syntax=CachedSyntax.init(z),
      ~root=Sort.Exp,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => z
  | Error(err) => failwith("action failed: " ++ Action.Failure.show(err))
  };
};

let act = (s: string): Action.t =>
  Action.t_of_sexp(Sexplib.Sexp.of_string(s));

let term_of_zipper = (z: Zipper.t): Exp.t =>
  MakeTerm.from_zip_for_sem(z, ~root=Exp).term;

/* Every id in the term, in traversal order. */
let ids_of = (e: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let f_exp = (continue, x: Exp.t): Exp.t => {
    acc := [Exp.rep_id(x), ...acc^];
    continue(x);
  };
  let _ = TermBase.Exp.map_term(~f_exp, e);
  List.rev(acc^);
};

let round_tripped_with_no_fresh_ids = ref(false);

let test_real_edit_id_pattern = () => {
  let src = "let v = (2, 2) in v";
  switch (Parser.to_zipper(~root=Exp, src)) {
  | None => Printf.printf("parse failed\n")
  | Some(z0) =>
    let before = term_of_zipper(z0);
    let ids0 = ids_of(before);
    List.iter(
      n => {
        let z = ref(z0);
        try(
          {
            z := perform(z^, act("(Move End)"));
            for (_ in 1 to n) {
              z := perform(z^, act("(Move (Local Left ByChar))"));
            };
            z := perform(z^, act("(Destruct Left)"));
            z := perform(z^, act("(Insert \"2\")"));
            let after = term_of_zipper(z^);
            let ids1 = ids_of(after);
            let kept =
              List.length(List.filter(i => List.mem(i, ids0), ids1));
            Printf.printf(
              "REALEDIT n=%d src=%-34s ids before=%d after=%d kept=%d fresh=%d\n",
              n,
              show(after),
              List.length(ids0),
              List.length(ids1),
              kept,
              List.length(ids1) - kept,
            );
            if (show(after) == show(before)
                && List.length(ids1) == List.length(ids0)
                && kept == List.length(ids1)) {
              round_tripped_with_no_fresh_ids := true;
            };
          }
        ) {
        | e =>
          Printf.printf(
            "REALEDIT n=%d raised %s\n",
            n,
            Printexc.to_string(e),
          )
        };
      },
      [3, 4, 5, 6, 7, 8, 9],
    );
    /* The claim this records: deleting and retyping a token round-trips to
     * the SAME ids, because IdMatch transplants them. That is what keeps
     * P1/P2 off the editor's own paths -- the safety comes from IdMatch,
     * not from reuse_check. */
    check(
      bool,
      "a delete+retype round-trip keeps every id (IdMatch transplants)",
      true,
      round_tripped_with_no_fresh_ids^,
    );
  };
};

/* The probe oracle driven by REAL editor actions, exactly as
 * CLI/BenchIncr.run_trace replays a trace: one zipper, actions applied in
 * sequence, statics and evaluation recomputed per step, cache threaded.
 * This is the only way to answer "can the editor actually produce this?"
 * for a probe finding, since `hazel bench-incr` compares values only. */
let real_edit_chain =
    (~name: string, ~src: string, ~steps: list(list(string))) => {
  switch (Parser.to_zipper(~root=Exp, src)) {
  | None => Printf.printf("REALCHAIN %s: parse failed\n", name)
  | Some(z0) =>
    let z = ref(z0);
    let terms =
      List.map(
        actions => {
          List.iter(a => z := perform(z^, act(a)), actions);
          term_of_zipper(z^);
        },
        steps,
      );
    Printf.printf(
      "REALCHAIN %s: %s\n",
      name,
      String.concat(" -> ", List.map(show, terms)),
    );
    let bad = ref([]);
    let prev0 = ref(IncrEval.empty);
    let prevs = List.map(_ => ref(IncrEval.empty), Calculus.available);
    List.iteri(
      (i, t) => {
        let (v0, s0) =
          eval_probe_all(~calculus=Calculus.A0, ~prev=prev0^, t);
        prev0 := s0.incr_eval;
        List.iteri(
          (j, c: Calculus.t) => {
            let pr = List.nth(prevs, j);
            let (v, st) = eval_probe_all(~calculus=c, ~prev=pr^, t);
            pr := st.incr_eval;
            let ds = sample_id_diffs(s0, st);
            if (ds != [] || show(v) != show(v0)) {
              bad :=
                [
                  Printf.sprintf(
                    "%s step %d %s value a0=%s got=%s\n%s",
                    name,
                    i,
                    Calculus.name(c),
                    show(v0),
                    show(v),
                    String.concat("\n", ds),
                  ),
                  ...bad^,
                ];
            };
          },
          Calculus.available,
        );
      },
      terms,
    );
    switch (List.rev(bad^)) {
    | [] => ()
    | ds => Alcotest.fail(String.concat("\n", ds))
    };
  };
};

let test_real_edit_chains = () => {
  /* Re-parse one binding's definition to the SAME text. `Update Definition`
   * is an ordinary editor action (the bench traces use it); it replaces the
   * definition's segment, so the ids inside it are re-minted while the
   * enclosing `let` keeps its own. */
  real_edit_chain(
    ~name="R1 Update Definition to the same text",
    ~src="let v = (2, 2) in
v",
    ~steps=[[], ["(Structural (Update Definition v \"(2, 2)\"))"]],
  );
  real_edit_chain(
    ~name="R2 Update Definition, distinct components",
    ~src="let v = (2, 5) in
v",
    ~steps=[[], ["(Structural (Update Definition v \"(2, 5)\"))"]],
  );
  real_edit_chain(
    ~name="R3 Update Definition with calls",
    ~src="let f : Int -> Int = fun n -> n + 1 in
let v = (f(3), f(3)) in
v",
    ~steps=[[], ["(Structural (Update Definition v \"(f(3), f(3))\"))"]],
  );
  /* Retype a component's digit to a new value and back again. */
  real_edit_chain(
    ~name="R4 retype a component away and back",
    ~src="let v = (2, 2) in
v",
    ~steps=[
      [],
      [
        "(Move End)",
        "(Move (Local Left ByChar))",
        "(Move (Local Left ByChar))",
        "(Move (Local Left ByChar))",
        "(Move (Local Left ByChar))",
        "(Move (Local Left ByChar))",
        "(Move (Local Left ByChar))",
        "(Destruct Left)",
        "(Insert \"5\")",
      ],
      ["(Destruct Left)", "(Insert \"2\")"],
    ],
  );
};

/* Test_IncrEval documents `swap_tuple_components` as "the shape of edit that
 * LabeledTupleHelpers.align_exp performs (it reorders a Tuple's components
 * to match an expected label order while deliberately preserving the Tuple's
 * id)". If align_exp runs during elaboration, a labeled tuple written out of
 * declaration order is permuted with ids preserved by the compiler itself,
 * which would give P1 a code path that needs no synthetic edit. */
let test_labeled_tuple_alignment = () => {
  let swap = Test_IncrEval.swap_tuple_components;
  check_probe_all(
    ~name="L1 labeled fields swapped, equal values",
    ~src="let z : (a = Int, b = Int) = (a = 2, b = 2) in
z",
    ~edits=[swap],
  );
  check_probe_all(
    ~name="L2 labeled fields swapped, distinct values",
    ~src="let z : (a = Int, b = Int) = (a = 2, b = 5) in
z",
    ~edits=[swap],
  );
  check_probe_all(
    ~name="L3 labeled fields, written out of declared order",
    ~src="let z : (a = Int, b = Int) = (b = 2, a = 2) in
z",
    ~edits=[swap],
  );
  check_probe_all(
    ~name="L4 labeled fields destructured",
    ~src=
      "let z : (a = Int, b = Int) = (a = 2, b = 2) in
let (a = p, b = q) = z in
p + q",
    ~edits=[swap],
  );
};

let tests = (
  "IncrEvalProbes",
  [
    test_case(
      "1a probe at the re-used node",
      `Quick,
      test_missing_probe_at_the_reused_node,
    ),
    test_case(
      "1b probe inside the re-used subtree",
      `Quick,
      test_missing_probe_inside_the_reused_subtree,
    ),
    test_case(
      "1c probe in a re-used call body",
      `Quick,
      test_missing_probe_in_reused_call_body,
    ),
    test_case("1d nested probes", `Quick, test_missing_nested_probes),
    test_case(
      "3a multiplicity, three calls",
      `Quick,
      test_multiplicity_three_calls,
    ),
    test_case(
      "3b multiplicity, recursion depth",
      `Quick,
      test_multiplicity_recursion_depth_changes,
    ),
    test_case(
      "3c multiplicity, both sides",
      `Quick,
      test_multiplicity_both_sides,
    ),
    test_case("4a order, re-used first", `Quick, test_order_reused_first),
    test_case(
      "4b order, re-used in the middle",
      `Quick,
      test_order_reused_in_the_middle,
    ),
    test_case(
      "4c order across a sequence",
      `Quick,
      test_order_across_a_sequence,
    ),
    test_case(
      "5a probe in each tuple component",
      `Quick,
      test_tuple_probe_in_each_component,
    ),
    test_case(
      "5b components and projections",
      `Quick,
      test_tuple_probe_component_and_projection,
    ),
    test_case("5c nested tuples", `Quick, test_tuple_probe_nested),
    test_case(
      "5d tuple components are calls",
      `Quick,
      test_tuple_probe_components_are_calls,
    ),
    test_case(
      "5e-h shape-changing edits",
      `Quick,
      test_tuple_probe_shape_changes,
    ),
    test_case("6a repointed variable", `Quick, test_var_repoint_probe),
    test_case(
      "6b repointed in a tuple",
      `Quick,
      test_var_repoint_probe_through_tuple,
    ),
    test_case("6c parens child replaced", `Quick, test_parens_collapse_probe),
    test_case(
      "6d guards still permit re-use",
      `Quick,
      test_guards_are_conservative_not_disabling,
    ),
    test_case(
      "7a a2 inner probe re-used",
      `Quick,
      test_a2_probe_inside_call_reused,
    ),
    test_case(
      "7b a2 inner probe invalidated",
      `Quick,
      test_a2_probe_inside_call_invalidated,
    ),
    test_case(
      "7c same probe at two depths",
      `Quick,
      test_a2_probe_two_depths,
    ),
    test_case(
      "7d recursion depth shifts",
      `Quick,
      test_a2_recursion_depth_shifts,
    ),
    test_case(
      "7e clean probe in recursion",
      `Quick,
      test_a2_recursion_clean_probe,
    ),
    test_case(
      "7f beyond the depth guard",
      `Quick,
      test_a2_beyond_the_depth_guard,
    ),
    test_case(
      "7g copied descendants then dirtied",
      `Quick,
      test_a2_copied_descendants_then_dirtied,
    ),
    test_case("8a closure body probe", `Quick, test_closure_body_probe),
    test_case("8b closure in a tuple", `Quick, test_closure_in_tuple_probe),
    test_case(
      "8c closure passed and applied",
      `Quick,
      test_closure_passed_and_applied,
    ),
    test_case(
      "8d closure definition edited",
      `Quick,
      test_closure_definition_edited,
    ),
    test_case("2a stale env only", `Quick, test_stale_env_only),
    test_case("2b stale args only", `Quick, test_stale_args_only),
    test_case("2c stale env in a call", `Quick, test_stale_env_in_call),
    test_case("2d long edit chain", `Quick, test_stale_long_chain),
    test_case("probe fuzz, literal edits", `Quick, test_probe_fuzz_literal),
    test_case(
      "probe fuzz, structural edits",
      `Quick,
      test_probe_fuzz_structural,
    ),
    test_case("probe_all witness", `Quick, test_probe_all_witness),
    test_case("probe_all corpus", `Quick, test_probe_all_corpus),
    test_case(
      "probe_all shape changes",
      `Quick,
      test_probe_all_shape_changes,
    ),
    test_case("probe_all fuzz", `Quick, test_probe_all_fuzz),
    test_case(
      "control: distinct components",
      `Quick,
      test_control_distinct_components,
    ),
    test_case(
      "control: fresh id and new text",
      `Quick,
      test_control_fresh_id_and_new_text,
    ),
    test_case("witnesses", `Quick, test_witnesses),
    test_case(
      "R: delete+retype keeps ids",
      `Quick,
      test_real_edit_id_pattern,
    ),
    test_case("R: real editor action chains", `Quick, test_real_edit_chains),
    test_case(
      "L: labeled tuple alignment",
      `Quick,
      test_labeled_tuple_alignment,
    ),
    /* ---- intentional failing pins ---- */
    test_case(
      "PIN P1: probe timeline transposed by a permutation",
      `Quick,
      test_pin_probe_order_transposed,
    ),
    test_case(
      "PIN P2: sample at a syntax id that no longer exists",
      `Quick,
      test_pin_probe_sample_at_dead_id,
    ),
    test_case(
      "PIN P2b: replayed sample carries a stale call frame",
      `Quick,
      test_pin_probe_stale_call_frame,
    ),
    test_case(
      "PIN P2c: dead-id sample, tuple destructured",
      `Quick,
      test_pin_probe_dead_id_destructured,
    ),
    test_case(
      "PIN P2d: dead-id sample, distinct siblings",
      `Quick,
      test_pin_probe_dead_id_distinct_siblings,
    ),
    test_case(
      "PIN: probe_all fuzz, structural edits",
      `Quick,
      test_probe_all_fuzz_structural,
    ),
  ],
);

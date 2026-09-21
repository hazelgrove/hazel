/* Replay an editing trace and time re-evaluation under each incremental
 * calculus.
 *
 * The point of the benchmark is the cost of the SECOND and later evaluations,
 * where a cache from the previous version of the program is available. That
 * only means anything if expression ids survive the edit, so the trace is
 * replayed as real editor actions against a live zipper rather than as a list
 * of independently parsed programs, which would mint fresh ids every step and
 * defeat every calculus equally.
 *
 * Trace format (JSON):
 *
 *   { "name": "rename-a-binder",
 *     "program": "let x = 5 in x + 1",
 *     "steps": [ { "label": "bump the literal",
 *                  "actions": ["(Move (Extreme Up))", "(Insert \"1\")"] } ] }
 *
 * Actions are the s-expression form of Haz3lcore.Action.t, which is what the
 * editor itself records, so a trace can be captured from a real session (or
 * emitted by an agent) without a translation layer. */

open Language;
open Util;

module Trace = {
  type step = {
    label: string,
    actions: list(Haz3lcore.Action.t),
  };

  type t = {
    name: string,
    program: string,
    steps: list(step),
  };

  let action_of_string = (s: string): Haz3lcore.Action.t =>
    try(Haz3lcore.Action.t_of_sexp(Sexplib.Sexp.of_string(s))) {
    | e =>
      failwith(
        "Could not read action " ++ s ++ ": " ++ Printexc.to_string(e),
      )
    };

  let of_json = (json: Yojson.Safe.t): t => {
    open Yojson.Safe.Util;
    let steps =
      json
      |> member("steps")
      |> to_list
      |> List.map(step => {
           let label =
             switch (step |> member("label")) {
             | `Null => ""
             | l => to_string(l)
             };
           let actions =
             step
             |> member("actions")
             |> to_list
             |> List.map(a => a |> to_string |> action_of_string);
           {
             label,
             actions,
           };
         });
    {
      name:
        switch (json |> member("name")) {
        | `Null => "trace"
        | n => to_string(n)
        },
      program: json |> member("program") |> to_string,
      steps,
    };
  };

  let load = (path: string): t => path |> Yojson.Safe.from_file |> of_json;
};

/* One timed step of one calculus run. */
type measurement = {
  /* Which IdMatch policy produced the edits this measurement timed. Carried
   * per-measurement rather than per-run because a sweep interleaves policies
   * within one process (see [sample]). */
  policy: string,
  calculus: Calculus.t,
  step_index: int,
  label: string,
  statics_ms: float,
  eval_ms: float,
  /* Cache entries carried into the NEXT step. Zero throughout for a0, and a
   * useful check that a calculus is actually caching what we think. */
  entries: int,
  /* The value this step evaluated to. Every calculus is meant to be sound
   * with respect to plain evaluation, so all of them must agree with a0 here;
   * a faster scheme that disagrees is a bug, not a speedup. */
  result: string,
};

let now = () =>
  Js_of_ocaml.Js.Unsafe.global##.performance##now()##valueOf
  |> Js_of_ocaml.Js.float_of_number;

let settings: CoreSettings.t = CoreSettings.on;

let zipper_of_program = (program: string): Haz3lcore.Zipper.t =>
  switch (
    Haz3lcore.PersistentZipper.parse_text(~source="bench", ~root=Exp, program)
  ) {
  | Some(z) => z
  | None => failwith("Could not parse trace program")
  };

/* Apply one editor action, exactly as the editor would. */
let perform_action =
    (z: Haz3lcore.Zipper.t, a: Haz3lcore.Action.t): Haz3lcore.Zipper.t => {
  let term = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    Haz3lcore.CachedStatics.init_from_term(
      ~settings,
      ~is_dynamic_term=true,
      term,
    );
  switch (
    Haz3lcore.Perform.go(
      ~settings,
      ~statics,
      ~syntax=Haz3lcore.CachedSyntax.init(z),
      ~root=Haz3lcore.Sort.Exp,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => z
  | Error(err) =>
    failwith("Action failed: " ++ Haz3lcore.Action.Failure.show(err))
  };
};

/* Replay the whole trace under a single calculus, threading the cache from
 * each step into the next. The zipper is rebuilt from the trace's starting
 * program for every calculus so that each run sees an identical edit
 * sequence. */
let run_trace =
    (~policy: string="default", ~calculus: Calculus.t, trace: Trace.t)
    : list(measurement) => {
  let zipper = ref(zipper_of_program(trace.program));
  let prev = ref(IncrEval.empty);
  let measurements = ref([]);

  List.iteri(
    (step_index, step: Trace.step) => {
      zipper := List.fold_left(perform_action, zipper^, step.actions);

      let t0 = now();
      let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper^, ~root=Exp).term;
      let (elab, eval_info) = Run.elab_and_eval_info(term);
      let t1 = now();

      let (value, state) =
        Evaluator.evaluate(
          ~calculus,
          ~prev=prev^,
          ~eval_info,
          ~env=Builtins.env_init,
          elab,
        );
      let t2 = now();

      prev := state.incr_eval;
      measurements :=
        [
          {
            policy,
            calculus,
            step_index,
            label: step.label,
            statics_ms: t1 -. t0,
            eval_ms: t2 -. t1,
            entries: Id.Map.cardinal(state.incr_eval.entries),
            result: Print.print(value),
          },
          ...measurements^,
        ];
    },
    trace.steps,
  );
  List.rev(measurements^);
};

/* ---------------------------------------------------------------------------
 * Trace audit.
 *
 * A recorded agent trace is not automatically a fair benchmark. The failure
 * mode that matters is the NO-OP step: an edit that leaves the program text
 * exactly as it was. Agents produce these when they re-apply a change they
 * have already made, and they are poison for a caching benchmark, because
 * every calculus can serve an unchanged program entirely from cache. A trace
 * padded with them shows a large speedup while measuring no real work.
 *
 * They are not silently dropped. A no-op is a real thing the agent did, and
 * which steps were free is part of reading the result -- so the audit reports
 * them and lets the caller decide, rather than quietly editing the evidence.
 * ------------------------------------------------------------------------- */

let text_of_zipper = (z: Haz3lcore.Zipper.t): string =>
  Haz3lcore.Printer.of_zipper(z);

type step_audit = {
  index: int,
  label: string,
  /* The step's actions ran but left the program text unchanged. */
  noop: bool,
};

/* Replay the trace once, untimed, recording which steps changed the program.
 * Untimed and outside [sample] because this is about what the trace IS, not
 * about how fast any calculus runs it. */
let audit_trace = (trace: Trace.t): list(step_audit) => {
  let zipper = ref(zipper_of_program(trace.program));
  let text = ref(text_of_zipper(zipper^));
  List.mapi(
    (index, step: Trace.step) => {
      zipper := List.fold_left(perform_action, zipper^, step.actions);
      let after = text_of_zipper(zipper^);
      let noop = after == text^ && step.actions != [];
      text := after;
      {
        index,
        label: step.label,
        noop,
      };
    },
    trace.steps,
  );
};

let report_audit = (trace: Trace.t, audits: list(step_audit)): unit => {
  let noops = List.filter(a => a.noop, audits);
  /* Step 0 conventionally has no actions (it is the cold evaluation of the
     starting program), so it is not counted as an edit either way. */
  let edits =
    List.length(
      List.filter((s: Trace.step) => s.actions != [], trace.steps),
    );
  switch (noops) {
  | [] => ()
  | _ =>
    Printf.eprintf(
      "NOTE: %d of %s's %d edit step(s) left the program unchanged:\n",
      List.length(noops),
      trace.name,
      edits,
    );
    List.iter(
      a => Printf.eprintf("  step %d (%s)\n", a.index, a.label),
      noops,
    );
    Printf.eprintf(
      "  These are free under every caching scheme and cost a0 a full re-evaluation,\n\
      \  so they flatter the incremental calculi. Read the per-step rows, not just the\n\
      \  total, or re-record the trace.\n%!",
    );
  };
};

/* Structural id-matching policy (IdMatch.re:74-99), selectable so a run can
 * measure what id preservation is worth rather than assume it.
 *
 * Every agent-supplied code string is re-parsed, which mints fresh ids for
 * everything it touches; IdMatch diffs the replacement against the syntax it
 * replaces and carries unchanged parts' ids across (CompositionGo.re:392-398).
 * Without that, an edit that rewrites a whole binding would make the entire
 * binding uncacheable, and every calculus would collapse toward a0.
 *
 * `none` is the un-diffed baseline, `isomorphic` is the top-down pass alone
 * (no descent into containers), `default` is what the editor actually runs.
 * IdMatch.Policy.current is a ref exposed for exactly this (IdMatch.re:100-105).
 *
 * The policy is global and set once per process, so the a0 reference run and
 * every timed calculus see the SAME programs -- otherwise the soundness check
 * would be comparing different edits against each other. */
let id_policies = ["none", "isomorphic", "default"];

let validate_id_policy = (name: string): Haz3lcore.IdMatch.Policy.t =>
  Haz3lcore.IdMatch.Policy.(
    switch (name) {
    | "none" => none
    | "isomorphic" => isomorphic_only
    | "default" => default
    | other =>
      Printf.eprintf(
        "Unknown --id-policy %s. Available: %s\n",
        other,
        String.concat(", ", id_policies),
      );
      exit(2);
    }
  );

let set_id_policy = (name: string): unit =>
  Haz3lcore.IdMatch.Policy.current := validate_id_policy(name);

let default_id_policy = "default";

/* ---------------------------------------------------------------------------
 * Repetitions.
 *
 * A single timed pass per (calculus, step) is not reportable: on this machine
 * two runs of one unchanged trace gave aPL step 3 as 1.37ms and then 4.15ms,
 * and a0's cold statics read high purely because a0 happened to run first and
 * absorbed the JIT warmup. So we take N samples per step and report the
 * median with its spread, after warming the JIT and rotating which calculus
 * goes first.
 *
 * The unit of repetition is a whole replay of the trace, not a single step:
 * each calculus threads its cache from one step into the next, so a step's
 * time is only meaningful as part of a full pass from a cold cache.
 * ------------------------------------------------------------------------- */

/* Summary of one step's repeated samples. [p25]/[p75] give the IQR; [min]/
 * [max] show how bad the tails are, which is what actually tells you whether
 * a difference between two calculi survives the noise. */
type stat = {
  median: float,
  min: float,
  max: float,
  p25: float,
  p75: float,
};

/* Nearest-rank on the sorted samples, except the median, which interpolates
 * for even counts so an even --reps is not silently biased low. */
let stat_of_samples = (xs: list(float)): stat => {
  let sorted = List.sort(compare, xs) |> Array.of_list;
  let n = Array.length(sorted);
  if (n == 0) {
    {
      median: 0.,
      min: 0.,
      max: 0.,
      p25: 0.,
      p75: 0.,
    };
  } else {
    let quantile = q => {
      let i = int_of_float(Float.of_int(n) *. q);
      sorted[min(max(i, 0), n - 1)];
    };
    let median =
      n mod 2 == 1
        ? sorted[n / 2] : (sorted[n / 2 - 1] +. sorted[n / 2]) /. 2.;
    {
      median,
      min: sorted[0],
      max: sorted[n - 1],
      p25: quantile(0.25),
      p75: quantile(0.75),
    };
  };
};

/* One step of one calculus, aggregated over reps. [entries] and [result] are
 * deterministic across reps, so they are carried through unaggregated. */
type aggregate = {
  policy: string,
  calculus: Calculus.t,
  step_index: int,
  label: string,
  statics: stat,
  eval: stat,
  entries: int,
  result: string,
  reps: int,
};

let aggregate_step = (ms: list(measurement)): aggregate =>
  switch (ms) {
  | [] => failwith("aggregate_step: no samples")
  | [first, ..._] => {
      policy: first.policy,
      calculus: first.calculus,
      step_index: first.step_index,
      label: first.label,
      statics:
        stat_of_samples(List.map((m: measurement) => m.statics_ms, ms)),
      eval: stat_of_samples(List.map((m: measurement) => m.eval_ms, ms)),
      entries: first.entries,
      result: first.result,
      reps: List.length(ms),
    }
  };

/* Group every rep's measurements by (calculus, step) and aggregate. Ordering
 * follows [selected] then step index, so the table reads the same as before. */
let aggregate_all =
    (
      ~policies: list(string),
      ~selected: list(Calculus.t),
      ~n_steps: int,
      reps: list(list(measurement)),
    )
    : list(aggregate) => {
  let flat = List.concat(reps);
  List.concat_map(
    policy =>
      List.concat_map(
        calculus =>
          List.filter_map(
            step_index => {
              let ms =
                List.filter(
                  (m: measurement) =>
                    m.policy == policy
                    && m.calculus == calculus
                    && m.step_index == step_index,
                  flat,
                );
              switch (ms) {
              | [] => None
              | _ => Some(aggregate_step(ms))
              };
            },
            List.init(n_steps, i => i),
          ),
        selected,
      ),
    policies,
  );
};

/* Rotate so that rep [i] starts with a different calculus. Whichever runs
 * first in a pass pays for any residual warmup, and without rotation that
 * cost lands on the same calculus in every rep and turns into a systematic
 * bias rather than noise. */
let rotate = (xs: list('a), i: int): list('a) => {
  let n = List.length(xs);
  if (n <= 1) {
    xs;
  } else {
    let k = i mod n;
    let rec split = (k, acc, rest) =>
      switch (k, rest) {
      | (0, _) => (List.rev(acc), rest)
      | (_, []) => (List.rev(acc), [])
      | (_, [x, ...tl]) => split(k - 1, [x, ...acc], tl)
      };
    let (head, tail) = split(k, [], xs);
    tail @ head;
  };
};

/* Sum of eval time over every step but the first, for one rep. The summary
 * takes the median of these per-rep totals rather than summing the per-step
 * medians: the latter is not a quantity any single run ever exhibited. */
let incr_total_of_rep =
    (~policy: string, ~calculus: Calculus.t, ms: list(measurement))
    : option((float, float)) => {
  let mine =
    List.filter(
      (m: measurement) => m.policy == policy && m.calculus == calculus,
      ms,
    )
    |> List.sort((a: measurement, b: measurement) =>
         compare(a.step_index, b.step_index)
       );
  switch (mine) {
  | [] => None
  | [cold, ...rest] =>
    Some((
      cold.eval_ms,
      List.fold_left((acc, m: measurement) => acc +. m.eval_ms, 0., rest),
    ))
  };
};

/* ---------------------------------------------------------------------------
 * Progress.
 *
 * A full sweep is reps x policies x calculi replays of a trace, and on a trace
 * whose program takes a few hundred ms to evaluate that is minutes of silence
 * before the table appears. Report each completed pass on STDERR -- stdout
 * carries the table, which a caller may be piping -- so a long run can be
 * watched, and so a run that is going to take an hour says so in the first
 * few seconds rather than at the end.
 * ------------------------------------------------------------------------- */

let fmt_duration = (ms: float): string =>
  if (ms < 1000.) {
    Printf.sprintf("%.0fms", ms);
  } else if (ms < 60_000.) {
    Printf.sprintf("%.1fs", ms /. 1000.);
  } else {
    Printf.sprintf(
      "%dm%02ds",
      int_of_float(ms) / 60_000,
      int_of_float(ms) mod 60_000 / 1000,
    );
  };

/* [done_] counts passes finished, [total] passes planned. The ETA is a flat
 * extrapolation from the mean pass so far; passes are the same amount of work
 * as each other, so that is honest enough to steer by, and it is labelled as
 * an estimate rather than a promise. */
let report_progress =
    (
      ~t_start: float,
      ~done_: int,
      ~total: int,
      ~phase: string,
      ~detail: string,
    )
    : unit => {
  let elapsed = now() -. t_start;
  let eta =
    done_ == 0
      ? "?"
      : fmt_duration(
          elapsed /. float_of_int(done_) *. float_of_int(total - done_),
        );
  Printf.eprintf(
    "[%3d/%3d] %-7s %-28s elapsed %8s  eta %8s\n%!",
    done_,
    total,
    phase,
    detail,
    fmt_duration(elapsed),
    eta,
  );
};

/* Timed sampling.
 *
 * [warmup] full passes over every (policy, calculus) run first and are
 * discarded, so the first MEASURED pass is not the one that JIT-compiles
 * MakeTerm, statics and the evaluator.
 *
 * Policies are swept INSIDE one process and interleaved with the reps, not by
 * running the binary once per policy. That matters: a0 has no cache and cannot
 * be causally affected by the id-matching policy, yet across separate
 * invocations its total drifted 8163 -> 6927 -> 10299 ms purely from machine
 * load. Any policy effect smaller than that drift is unreadable when policies
 * live in different processes. Interleaving puts every policy under the same
 * conditions within a rep, so the comparison survives a loaded machine.
 *
 * Both lists are rotated by the rep index so neither a policy nor a calculus
 * is systematically the one that runs first. */
let sample =
    (
      ~policies: list(string),
      ~selected: list(Calculus.t),
      ~reps: int,
      ~warmup: int,
      trace: Trace.t,
    )
    : list(list(measurement)) => {
  let n_combos = List.length(policies) * List.length(selected);
  let total = (warmup + reps) * n_combos;
  let t_start = now();
  let done_ = ref(0);
  Printf.eprintf(
    "%s: %d step(s) x %d calculi x %d polic(ies) x (%d warmup + %d reps) = %d passes\n%!",
    trace.name,
    List.length(trace.steps),
    List.length(selected),
    List.length(policies),
    warmup,
    reps,
    total,
  );
  let pass = (~phase: string, policy, calculus) => {
    set_id_policy(policy);
    let ms = run_trace(~policy, ~calculus, trace);
    incr(done_);
    report_progress(
      ~t_start,
      ~done_=done_^,
      ~total,
      ~phase,
      ~detail=
        Printf.sprintf(
          "%s/%s %s",
          Calculus.name(calculus),
          policy,
          /* Eval time for this pass excluding the cold step, which is the
           * quantity the summary table ends up reporting. Showing it live
           * means a sweep that is going wrong is visible immediately rather
           * than after every rep has been paid for. */
          switch (incr_total_of_rep(~policy, ~calculus, ms)) {
          | Some((_, incr_ms)) =>
            Printf.sprintf("incr %s", fmt_duration(incr_ms))
          | None => ""
          },
        ),
    );
    ms;
  };
  for (_ in 1 to warmup) {
    List.iter(
      policy =>
        List.iter(
          calculus => ignore(pass(~phase="warmup", policy, calculus)),
          selected,
        ),
      policies,
    );
  };
  List.init(reps, rep =>
    List.concat_map(
      policy =>
        List.concat_map(
          calculus =>
            pass(~phase=Printf.sprintf("rep %d", rep + 1), policy, calculus),
          rotate(selected, rep),
        ),
      rotate(policies, rep),
    )
  );
};

let json_of_measurement = (~rep: int, m: measurement): Yojson.Safe.t =>
  `Assoc([
    ("policy", `String(m.policy)),
    ("calculus", `String(Calculus.name(m.calculus))),
    ("rep", `Int(rep)),
    ("step", `Int(m.step_index)),
    ("label", `String(m.label)),
    ("statics_ms", `Float(m.statics_ms)),
    ("eval_ms", `Float(m.eval_ms)),
    ("entries", `Int(m.entries)),
    ("result", `String(m.result)),
  ]);

let json_of_stat = (s: stat): Yojson.Safe.t =>
  `Assoc([
    ("median", `Float(s.median)),
    ("min", `Float(s.min)),
    ("max", `Float(s.max)),
    ("p25", `Float(s.p25)),
    ("p75", `Float(s.p75)),
  ]);

let json_of_aggregate = (a: aggregate): Yojson.Safe.t =>
  `Assoc([
    ("policy", `String(a.policy)),
    ("calculus", `String(Calculus.name(a.calculus))),
    ("step", `Int(a.step_index)),
    ("label", `String(a.label)),
    ("reps", `Int(a.reps)),
    ("statics_ms", json_of_stat(a.statics)),
    ("eval_ms", json_of_stat(a.eval)),
    ("entries", `Int(a.entries)),
    ("result", `String(a.result)),
  ]);

let print_table =
    (
      ~reps: int,
      ~warmup: int,
      ~policies: list(string),
      trace: Trace.t,
      aggs: list(aggregate),
      per_rep: list(list(measurement)),
      selected: list(Calculus.t),
    )
    : unit => {
  Printf.printf(
    "\ntrace: %s (%d steps, %d reps, %d warmup pass(es))\n",
    trace.name,
    List.length(trace.steps),
    reps,
    warmup,
  );
  List.iter(
    policy => {
      Printf.printf("\n[id-policy: %s]\n", policy);
      Printf.printf(
        "%-8s %5s  %-22s %9s %9s %9s %9s %7s\n",
        "calculus",
        "step",
        "label",
        "stat~med",
        "eval~med",
        "eval~IQR",
        "eval~rng",
        "entries",
      );
      Printf.printf("%s\n", String.make(88, '-'));
      List.iter(
        (a: aggregate) =>
          if (a.policy == policy) {
            Printf.printf(
              "%-8s %5d  %-22s %9.2f %9.2f %9s %9s %7d\n",
              Calculus.name(a.calculus),
              a.step_index,
              a.label,
              a.statics.median,
              a.eval.median,
              Printf.sprintf("%.2f-%.2f", a.eval.p25, a.eval.p75),
              Printf.sprintf("%.2f-%.2f", a.eval.min, a.eval.max),
              a.entries,
            );
          },
        aggs,
      );
    },
    policies,
  );

  /* Totals across the trace, excluding step 0: the first evaluation is a cold
   * run with an empty cache under every calculus, so including it flatters
   * whichever scheme is slowest to warm up.
   *
   * The vs-a0 column is the number to read. Absolute times drift with machine
   * load between runs, but a0 is the control -- it holds no cache, so nothing
   * a calculus or an id-policy does can causally change it. Dividing by the a0
   * measured in the SAME reps under the SAME policy cancels that drift, so a
   * ratio is comparable across runs in a way a raw millisecond figure is not.
   * A ratio at or above 1.00 means the scheme lost to doing no caching at
   * all. */
  Printf.printf(
    "\n%-8s %-11s %12s %12s %16s %8s\n",
    "calculus",
    "id-policy",
    "cold~med",
    "incr~med",
    "incr~IQR",
    "vs-a0",
  );
  Printf.printf("%s\n", String.make(74, '-'));
  List.iter(
    policy => {
      let totals_for = mode =>
        List.filter_map(
          ms => incr_total_of_rep(~policy, ~calculus=mode, ms),
          per_rep,
        );
      let baseline =
        switch (totals_for(Calculus.A0)) {
        | [] => None
        | ts => Some(stat_of_samples(List.map(snd, ts)).median)
        };
      List.iter(
        (mode: Calculus.t) =>
          switch (totals_for(mode)) {
          | [] => ()
          | totals =>
            let cold = stat_of_samples(List.map(fst, totals));
            let incr = stat_of_samples(List.map(snd, totals));
            Printf.printf(
              "%-8s %-11s %12.2f %12.2f %16s %8s\n",
              Calculus.name(mode),
              policy,
              cold.median,
              incr.median,
              Printf.sprintf("%.2f-%.2f", incr.p25, incr.p75),
              switch (baseline) {
              | Some(b) when b > 0. =>
                Printf.sprintf("%.3f", incr.median /. b)
              | _ => "-"
              },
            );
          },
        selected,
      );
    },
    policies,
  );
  print_newline();
};

/* Every calculus is sound with respect to plain evaluation, so each step must
 * produce the value a0 produced. Checking it here is what keeps a timing
 * number meaningful: a scheme that re-uses a stale entry would otherwise just
 * look fast. Returns the disagreements, empty when all calculi agree.
 *
 * a0 is the only admissible reference: it is the calculus with no cache to be
 * stale. Comparing the selected calculi against each other instead would let a
 * single-mode run agree with itself and certify a provably wrong answer, so a
 * caller that has not run a0 gets no verdict rather than a vacuous pass. */
let disagreements = (results: list(measurement)): option(list(string)) => {
  let reference = Calculus.A0;
  let expected =
    results
    |> List.filter((m: measurement) => m.calculus == reference)
    |> List.map((m: measurement) => (m.step_index, m.result));
  switch (expected) {
  | [] => None
  | _ =>
    Some(
      results
      |> List.filter_map((m: measurement) =>
           switch (List.assoc_opt(m.step_index, expected)) {
           | Some(want) when want != m.result =>
             Some(
               Printf.sprintf(
                 "step %d (%s): %s gave %s, %s gave %s",
                 m.step_index,
                 m.label,
                 Calculus.name(m.calculus),
                 m.result,
                 Calculus.name(reference),
                 want,
               ),
             )
           | _ => None
           }
         ),
    )
  };
};

/* Report soundness failures loudly and make them the process's exit status:
 * a benchmark run that quietly produced wrong answers is worse than one that
 * did not run at all. */
let report_disagreements = (trace: Trace.t, results: list(measurement)): bool =>
  switch (disagreements(results)) {
  | None =>
    Printf.eprintf(
      "UNCHECKED: %s ran without a0, so nothing verified these answers.\n",
      trace.name,
    );
    false;
  | Some([]) =>
    /* Print.print renders every hole as `?`, so two different indeterminate
     * results compare equal. Say so rather than let a trace that goes
     * indeterminate bank an agreement it did not really earn. */
    if (List.exists(
          (m: measurement) => String.contains(m.result, '?'),
          results,
        )) {
      Printf.eprintf(
        "WEAK CHECK: %s produced indeterminate results, which all compare equal.\n",
        trace.name,
      );
    };
    true;
  | Some(problems) =>
    Printf.eprintf(
      "UNSOUND: calculi disagreed on %s; timings below are meaningless.\n",
      trace.name,
    );
    List.iter(p => Printf.eprintf("  %s\n", p), problems);
    false;
  };

let default_reps = 5;
let default_warmup = 1;

let bench_incr =
    (
      modes: list(string),
      reps: int,
      warmup: int,
      id_policies_sel: list(string),
      json_out: option(string),
      paths: list(string),
    )
    : unit => {
  let policies =
    switch (id_policies_sel) {
    | [] => [default_id_policy]
    | ps =>
      List.iter(p => ignore(validate_id_policy(p)), ps);
      ps;
    };
  if (reps < 1) {
    prerr_endline("--reps must be at least 1.");
    exit(2);
  };
  if (warmup < 0) {
    prerr_endline("--warmup cannot be negative.");
    exit(2);
  };
  let selected =
    switch (modes) {
    | [] => Calculus.available
    | names =>
      let available =
        String.concat(", ", List.map(Calculus.name, Calculus.available));
      /* Naming a calculus whose machinery has not landed yet is a normal
       * mistake while the family is being filled in, so say what this build
       * can actually run rather than raising. */
      let unusable = reason => {
        Printf.eprintf("%s Available: %s\n", reason, available);
        exit(2);
      };
      List.map(
        name =>
          switch (Calculus.of_name(name)) {
          | None => unusable("Unknown calculus " ++ name ++ ".")
          | Some(mode) when !Calculus.is_available(mode) =>
            unusable("Calculus " ++ name ++ " is not implemented yet.")
          | Some(mode) => mode
          },
        names,
      );
    };

  let sound = ref(true);
  let all = ref([]);
  List.iter(
    path => {
      let trace = Trace.load(path);
      /* Before any timing: say what this trace actually contains, so a
         suspicious result can be read against the trace's shape rather than
         taken at face value. */
      let audits = audit_trace(trace);
      report_audit(trace, audits);
      let per_rep = sample(~policies, ~selected, ~reps, ~warmup, trace);
      let aggs =
        aggregate_all(
          ~policies,
          ~selected,
          ~n_steps=List.length(trace.steps),
          per_rep,
        );
      /* Timing one scheme in isolation is the normal way to use this, and it
       * must not cost the soundness check, so run the control regardless and
       * keep it out of the table when it was not asked for. This reference
       * pass runs EXACTLY ONCE no matter how large --reps is: it is a
       * correctness oracle, not a sample, and repeating it would add
       * evaluation work without adding information.
       *
       * The check itself is pure post-processing over result strings that the
       * reps already produced, so it costs nothing inside the timed region.
       * Feeding it every rep rather than just the first is therefore free,
       * and strictly stronger: it would also catch a calculus that answered
       * nondeterministically across passes. */
      let reference =
        List.mem(Calculus.A0, selected)
          ? []
          /* Under the first swept policy, once. a0's VALUE must not depend
           * on id-policy -- if it did that would itself be a soundness bug,
           * and comparing every policy's measurements against this single
           * reference is what would catch it. */
          : {
            set_id_policy(List.hd(policies));
            run_trace(
              ~policy=List.hd(policies),
              ~calculus=Calculus.A0,
              trace,
            );
          };
      print_table(~reps, ~warmup, ~policies, trace, aggs, per_rep, selected);
      if (!report_disagreements(trace, reference @ List.concat(per_rep))) {
        sound := false;
      };
      all := all^ @ [(path, aggs, per_rep)];
    },
    paths,
  );

  switch (json_out) {
  | None => ()
  | Some(out) =>
    /* Both views: the aggregates that the table shows, and every raw sample
     * behind them, so a reader can recompute the statistics or check the
     * spread themselves rather than taking the median on trust. */
    let with_trace = (path, fields) =>
      switch (fields) {
      | `Assoc(fs) => `Assoc([("trace", `String(path)), ...fs])
      | other => other
      };
    let aggregates =
      List.concat_map(
        ((path, aggs, _)) =>
          List.map(a => with_trace(path, json_of_aggregate(a)), aggs),
        all^,
      );
    let samples =
      List.concat_map(
        ((path, _, per_rep)) =>
          List.concat(
            List.mapi(
              (rep, ms) =>
                List.map(
                  m => with_trace(path, json_of_measurement(~rep, m)),
                  ms,
                ),
              per_rep,
            ),
          ),
        all^,
      );
    let json =
      `Assoc([
        ("reps", `Int(reps)),
        ("warmup", `Int(warmup)),
        ("id_policies", `List(List.map(p => `String(p), policies))),
        ("aggregates", `List(aggregates)),
        ("samples", `List(samples)),
      ]);
    Yojson.Safe.to_file(out, json);
    Printf.printf("wrote %s\n", out);
  };

  if (! sound^) {
    exit(1);
  };
};

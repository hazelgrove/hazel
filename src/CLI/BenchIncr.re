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
let run_trace = (~calculus: Calculus.t, trace: Trace.t): list(measurement) => {
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

let json_of_measurement = (m: measurement): Yojson.Safe.t =>
  `Assoc([
    ("calculus", `String(Calculus.name(m.calculus))),
    ("step", `Int(m.step_index)),
    ("label", `String(m.label)),
    ("statics_ms", `Float(m.statics_ms)),
    ("eval_ms", `Float(m.eval_ms)),
    ("entries", `Int(m.entries)),
    ("result", `String(m.result)),
  ]);

let print_table = (trace: Trace.t, results: list(measurement)): unit => {
  Printf.printf(
    "\ntrace: %s (%d steps)\n",
    trace.name,
    List.length(trace.steps),
  );
  Printf.printf(
    "%-8s %6s  %-28s %12s %12s %9s\n",
    "calculus",
    "step",
    "label",
    "statics(ms)",
    "eval(ms)",
    "entries",
  );
  Printf.printf("%s\n", String.make(82, '-'));
  List.iter(
    (m: measurement) =>
      Printf.printf(
        "%-8s %6d  %-28s %12.2f %12.2f %9d\n",
        Calculus.name(m.calculus),
        m.step_index,
        m.label,
        m.statics_ms,
        m.eval_ms,
        m.entries,
      ),
    results,
  );

  /* Totals across the trace, excluding step 0: the first evaluation is a cold
   * run with an empty cache under every calculus, so including it flatters
   * whichever scheme is slowest to warm up. */
  Printf.printf(
    "\n%-8s %14s %14s\n",
    "calculus",
    "cold(ms)",
    "incr-total(ms)",
  );
  Printf.printf("%s\n", String.make(38, '-'));
  List.iter(
    (mode: Calculus.t) => {
      let mine =
        List.filter((m: measurement) => m.calculus == mode, results);
      switch (mine) {
      | [] => ()
      | [cold, ...rest] =>
        let total =
          List.fold_left(
            (acc, m: measurement) => acc +. m.eval_ms,
            0.,
            rest,
          );
        Printf.printf(
          "%-8s %14.2f %14.2f\n",
          Calculus.name(mode),
          cold.eval_ms,
          total,
        );
      };
    },
    Calculus.available,
  );
  print_newline();
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

let bench_incr =
    (modes: list(string), json_out: option(string), paths: list(string))
    : unit => {
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
  let all =
    List.concat_map(
      path => {
        let trace = Trace.load(path);
        let results =
          List.concat_map(calculus => run_trace(~calculus, trace), selected);
        /* Timing one scheme in isolation is the normal way to use this, and it
         * must not cost the soundness check, so run the control regardless and
         * keep it out of the table when it was not asked for. */
        let reference =
          List.mem(Calculus.A0, selected)
            ? [] : run_trace(~calculus=Calculus.A0, trace);
        print_table(trace, results);
        if (!report_disagreements(trace, reference @ results)) {
          sound := false;
        };
        List.map(m => (path, m), results);
      },
      paths,
    );

  switch (json_out) {
  | None => ()
  | Some(out) =>
    let json =
      `List(
        List.map(
          ((path, m)) =>
            switch (json_of_measurement(m)) {
            | `Assoc(fields) =>
              `Assoc([("trace", `String(path)), ...fields])
            | other => other
            },
          all,
        ),
      );
    Yojson.Safe.to_file(out, json);
    Printf.printf("wrote %s\n", out);
  };

  if (! sound^) {
    exit(1);
  };
};

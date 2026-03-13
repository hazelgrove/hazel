/* Hazel performance benchmarks with scenario-based timing.
 *
 * Measures CachedSyntax and CachedStatics pipeline phases across four scenarios:
 *   cold   - caches cleared before each iteration
 *   warm   - caches primed, measuring steady-state
 *   move   - caches primed with original, measuring after cursor movement
 *   modify - caches primed with original, measuring after content edit
 *
 * Phases instrumented via PhaseTiming (syntax and statics groups).
 * Cache control via ResettableMemo.clear_all().
 * 10 iterations per scenario, report median per phase.
 *
 * Usage:
 *   node bench/hazel_bench.bc.js            # table output
 *   node bench/hazel_bench.bc.js --json     # JSON output for CI comparison
 *   node bench/hazel_bench.bc.js --reps 10  # 10 repetitions (default: 10)
 *   node bench/hazel_bench.bc.js --filter cold --filter let100
 */

open Haz3lcore;
open Language;
open Util;

/* --- High-resolution timing via performance.now() --- */

let now_ms = (): float => {
  let perf =
    Js_of_ocaml.Js.Unsafe.get(Js_of_ocaml.Js.Unsafe.global, "performance");
  Js_of_ocaml.Js.Unsafe.meth_call(perf, "now", [||]);
};

/* Force Node.js garbage collection. Requires --expose-gc flag. */
let force_gc = (): unit =>
  Js_of_ocaml.Js.Unsafe.meth_call(
    Js_of_ocaml.Js.Unsafe.global,
    "gc",
    [||],
  );

/* --- Program generators --- */

let gen_let_chain = (n: int): string => {
  let buf = Stdlib.Buffer.create(n * 30);
  for (i in 0 to n - 1) {
    if (i == 0) {
      Stdlib.Buffer.add_string(buf, "let x0 = 0 in\n");
    } else {
      Stdlib.Buffer.add_string(
        buf,
        "let x"
        ++ string_of_int(i)
        ++ " = x"
        ++ string_of_int(i - 1)
        ++ " + 1 in\n",
      );
    };
  };
  Stdlib.Buffer.add_string(buf, "x" ++ string_of_int(n - 1));
  Stdlib.Buffer.contents(buf);
};

/* --- Parsing --- */

let parse_to_zipper = (program: string): Zipper.t =>
  switch (Parser.to_zipper(program)) {
  | Some(z) => z
  | None =>
    let len = min(40, String.length(program));
    failwith("Failed to parse: " ++ String.sub(program, 0, len));
  };

/* --- Pipeline --- */

let settings = CoreSettings.on;

/* Run CachedSyntax + CachedStatics with PhaseTiming enabled.
 * Returns the list of (phase_name, nanoseconds) pairs. */
let run_measured = (z: Zipper.t): list((string, float)) => {
  PhaseTiming.recordings := [];
  PhaseTiming.enabled := true;
  let _syntax = CachedSyntax.init(z);
  let _statics =
    CachedStatics.init(
      ~settings,
      ~is_dynamic_term=true,
      ~stitch=Fun.id,
      z,
    );
  PhaseTiming.enabled := false;
  PhaseTiming.get_and_clear();
};

/* Run pipeline without timing to populate caches. */
let prime = (z: Zipper.t): unit => {
  PhaseTiming.enabled := false;
  let _syntax = CachedSyntax.init(z);
  let _statics =
    CachedStatics.init(
      ~settings,
      ~is_dynamic_term=true,
      ~stitch=Fun.id,
      z,
    );
  ();
};

/* Perform an editor action on a zipper. */
let perform_action =
    (z: Zipper.t, syntax: CachedSyntax.t, action: Action.t): Zipper.t =>
  switch (
    Perform.go(
      ~statics=CachedStatics.empty,
      ~syntax,
      action,
      {zipper: z, col_target: None},
    )
  ) {
  | Ok(new_z) => new_z
  | Error(_) => z
  };

/* --- Scenario runners --- */

/* Each returns a list of (phase_name, nanoseconds) for one iteration. */

let run_cold_iter = (z: Zipper.t): list((string, float)) => {
  ResettableMemo.clear_all();
  force_gc();
  run_measured(z);
};

let run_warm_iter = (z: Zipper.t): list((string, float)) => {
  force_gc();
  run_measured(z);
};

/* For move/modify: clear caches, prime with original z, then measure on z'. */
let run_incremental_iter =
    (z_original: Zipper.t, z_modified: Zipper.t): list((string, float)) => {
  ResettableMemo.clear_all();
  force_gc();
  prime(z_original);
  run_measured(z_modified);
};

/* --- Data types --- */

type measurement = {
  name: string,
  time_ns: float,
};

type parsed_program = {
  label: string,
  z: Zipper.t,
};

/* --- Scenario orchestration --- */

let run_scenario =
    (label: string, scenario: string, reps: int, run_iter: unit => list((string, float)))
    : list(list(measurement)) =>
  List.init(reps, _rep => {
    let phases = run_iter();
    List.map(
      ((phase, ns)) => {name: label ++ "/" ++ scenario ++ "/" ++ phase, time_ns: ns},
      phases,
    );
  });

let run_all_scenarios =
    (prog: parsed_program, reps: int): list(list(measurement)) => {
  let z = prog.z;
  let label = prog.label;

  /* Cold: clear caches before each iteration */
  Printf.eprintf("  cold...%!");
  let cold = run_scenario(label, "cold", reps, () => run_cold_iter(z));

  /* Warm: prime once, then measure repeated runs */
  Printf.eprintf(" warm...%!");
  ResettableMemo.clear_all();
  prime(z);
  let warm = run_scenario(label, "warm", reps, () => run_warm_iter(z));

  /* Move: prime with original, measure after cursor movement */
  Printf.eprintf(" move...%!");
  let syntax = CachedSyntax.init(z);
  let z_moved = perform_action(z, syntax, Move(Local(Left, ByChar)));
  let move =
    run_scenario(label, "move", reps, () => run_incremental_iter(z, z_moved));

  /* Modify: prime with original, measure after content edit */
  Printf.eprintf(" modify...%!");
  let z_modified = perform_action(z, syntax, Insert("x"));
  let modify =
    run_scenario(label, "modify", reps, () =>
      run_incremental_iter(z, z_modified)
    );

  Printf.eprintf(" done\n%!");
  cold @ warm @ move @ modify;
};

/* --- Statistics --- */

let median = (values: array(float)): float => {
  let sorted = Array.copy(values);
  Array.sort(compare, sorted);
  let n = Array.length(sorted);
  if (n == 0) {
    0.0;
  } else if (n mod 2 == 1) {
    sorted[n / 2];
  } else {
    (sorted[n / 2 - 1] +. sorted[n / 2]) /. 2.0;
  };
};

/* Aggregate measurements across repetitions by taking the median. */
let aggregate = (all_reps: list(list(measurement))): list(measurement) => {
  let tbl: Hashtbl.t(string, list(float)) = Hashtbl.create(128);
  let order: ref(list(string)) = ref([]);

  List.iter(
    results =>
      List.iter(
        (m: measurement) =>
          switch (Hashtbl.find_opt(tbl, m.name)) {
          | Some(vs) => Hashtbl.replace(tbl, m.name, [m.time_ns, ...vs])
          | None =>
            Hashtbl.replace(tbl, m.name, [m.time_ns]);
            order := [m.name, ...order^];
          },
        results,
      ),
    all_reps,
  );

  List.rev(order^)
  |> List.map(name => {
       let vs = Hashtbl.find(tbl, name);
       {name, time_ns: median(Array.of_list(vs))};
     });
};

/* Insert Total rows after each {size}/{scenario} group. */
let add_totals = (results: list(measurement)): list(measurement) => {
  let get_group = (name: string): string => {
    /* Find second '/' — group is "{size}/{scenario}" */
    let slash_count = ref(0);
    let pos = ref(String.length(name));
    for (i in 0 to String.length(name) - 1) {
      if (name.[i] == '/') {
        slash_count := slash_count^ + 1;
        if (slash_count^ == 2) {
          pos := i;
        };
      };
    };
    String.sub(name, 0, pos^);
  };

  let output = ref([]);
  let cur_group = ref("");
  let cur_sum = ref(0.0);

  let flush = () =>
    if (cur_group^ != "") {
      output :=
        [{name: cur_group^ ++ "/Total", time_ns: cur_sum^}, ...output^];
    };

  List.iter(
    (m: measurement) => {
      let group = get_group(m.name);
      if (group != cur_group^) {
        flush();
        cur_group := group;
        cur_sum := 0.0;
      };
      output := [m, ...output^];
      cur_sum := cur_sum^ +. m.time_ns;
    },
    results,
  );
  flush();

  List.rev(output^);
};

/* --- Output --- */

let format_time = (ns: float): string =>
  if (ns >= 1e9) {
    Printf.sprintf("%.2f s", ns /. 1e9);
  } else if (ns >= 1e6) {
    Printf.sprintf("%.2f ms", ns /. 1e6);
  } else if (ns >= 1e3) {
    Printf.sprintf("%.2f us", ns /. 1e3);
  } else {
    Printf.sprintf("%.0f ns", ns);
  };

let output_json = (results: list(measurement), reps: int): unit => {
  let entries =
    List.map(
      (m: measurement) =>
        Printf.sprintf(
          {|  {"name": "%s", "time_ns": %.2f, "samples": %d}|},
          m.name,
          m.time_ns,
          reps,
        ),
      results,
    );
  print_endline("[");
  print_endline(String.concat(",\n", entries));
  print_endline("]");
};

let output_table = (results: list(measurement)): unit => {
  let name_w =
    List.fold_left(
      (acc, m: measurement) => max(acc, String.length(m.name)),
      10,
      results,
    );
  let col_w = 12;

  Printf.printf(
    "\n%-*s  %*s\n",
    name_w,
    "Benchmark",
    col_w,
    "Time (median)",
  );
  Printf.printf("%s\n", String.make(name_w + col_w + 2, '-'));

  let prev_group = ref("");
  List.iter(
    (m: measurement) => {
      let group = {
        let slash_count = ref(0);
        let pos = ref(String.length(m.name));
        for (i in 0 to String.length(m.name) - 1) {
          if (m.name.[i] == '/') {
            slash_count := slash_count^ + 1;
            if (slash_count^ == 2) {
              pos := i;
            };
          };
        };
        String.sub(m.name, 0, pos^);
      };
      if (group != prev_group^) {
        if (prev_group^ != "") {
          print_newline();
        };
        prev_group := group;
      };
      Printf.printf(
        "%-*s  %*s\n",
        name_w,
        m.name,
        col_w,
        format_time(m.time_ns),
      );
    },
    results,
  );
  print_newline();
};

/* --- CLI --- */

let parse_int_arg = (argv: list(string), flag: string, default: int): int => {
  let rec go =
    fun
    | [] => default
    | [f, v, ...rest] when f == flag =>
      switch (int_of_string_opt(v)) {
      | Some(n) => n
      | None => go(rest)
      }
    | [_, ...rest] => go(rest);
  go(argv);
};

let parse_filters = (argv: list(string)): list(string) => {
  let filters = ref([]);
  let rec go =
    fun
    | [] => ()
    | ["--filter", pattern, ...rest] => {
        filters := [pattern, ...filters^];
        go(rest);
      }
    | [_, ...rest] => go(rest);
  go(argv);
  filters^;
};

let is_substring = (haystack: string, needle: string): bool => {
  let nlen = String.length(needle);
  let hlen = String.length(haystack);
  if (nlen > hlen) {
    false;
  } else {
    let found = ref(false);
    for (i in 0 to hlen - nlen) {
      if (!found^ && String.sub(haystack, i, nlen) == needle) {
        found := true;
      };
    };
    found^;
  };
};

/* --- Main --- */

let () = {
  let argv = Array.to_list(Sys.argv);
  let json_mode = List.mem("--json", argv);
  let reps = parse_int_arg(argv, "--reps", 10);
  let filters = parse_filters(argv);

  let programs = [("let100", gen_let_chain(100)), ("let500", gen_let_chain(500))];

  /* Only parse programs that match the filter. */
  let all_labels = List.map(((label, _)) => label, programs);
  let filter_targets_any_program = (pat: string): bool =>
    List.exists(
      label =>
        is_substring(label, pat)
        || String.length(pat) >= String.length(label)
        && String.sub(pat, 0, String.length(label)) == label,
      all_labels,
    );
  let programs =
    switch (filters) {
    | [] => programs
    | _ =>
      List.filter(
        ((label, _)) =>
          List.exists(
            pat =>
              is_substring(label, pat)
              || String.length(pat) >= String.length(label)
              && String.sub(pat, 0, String.length(label)) == label
              || !filter_targets_any_program(pat),
            filters,
          ),
        programs,
      )
    };

  let parsed =
    List.map(
      ((label, program)) => {
        let t0 = now_ms();
        let z = parse_to_zipper(program);
        let t1 = now_ms();
        Printf.eprintf("==> Parsed %s in %.0f ms\n%!", label, t1 -. t0);
        {label, z};
      },
      programs,
    );

  Printf.eprintf("==> Running %d iterations per scenario\n%!", reps);

  /* Run all scenarios for each program, collecting per-iteration results. */
  let all_reps: list(list(measurement)) =
    List.flatten(
      List.map(
        prog => {
          Printf.eprintf("==> %s:", prog.label);
          run_all_scenarios(prog, reps);
        },
        parsed,
      ),
    );

  /* Take median across iterations */
  let results = aggregate(all_reps);

  /* Add Total rows */
  let results = add_totals(results);

  /* Apply filters to output */
  let results =
    switch (filters) {
    | [] => results
    | _ =>
      List.filter(
        (m: measurement) =>
          List.exists(pat => is_substring(m.name, pat), filters),
        results,
      )
    };

  if (results == []) {
    Printf.eprintf(
      "No benchmarks matched filter(s): %s\n",
      String.concat(", ", filters),
    );
  } else if (json_mode) {
    output_json(results, reps);
  } else {
    output_table(results);
  };
};

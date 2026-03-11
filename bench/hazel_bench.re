/* Hazel performance benchmarks with scenario-based timing.
 *
 * Measures pipeline phases across four scenarios:
 *   cold   - fresh input, no memoization cache hits
 *   warm   - same input as cold, caches populated
 *   move   - after cursor movement (Move Left)
 *   modify - after content edit (Insert "x")
 *
 * Each scenario times: MakeTerm, Measured, Statics, Elaborate, Evaluate
 * (plus Perform for move/modify). Totals are computed per size/scenario.
 *
 * Usage:
 *   node bench/hazel_bench.bc.js            # table output
 *   node bench/hazel_bench.bc.js --json     # JSON output for CI comparison
 *   node bench/hazel_bench.bc.js --reps 10  # 10 repetitions (default: 7)
 *   node bench/hazel_bench.bc.js --filter cold --filter let100
 */

open Haz3lcore;
open Language;

/* --- High-resolution timing via performance.now() --- */

let now_ms = (): float => {
  let perf =
    Js_of_ocaml.Js.Unsafe.get(Js_of_ocaml.Js.Unsafe.global, "performance");
  Js_of_ocaml.Js.Unsafe.meth_call(perf, "now", [||]);
};

/* Time a single function call. Returns (nanoseconds, result). */
let time_call = (f: unit => 'a): (float, 'a) => {
  let t0 = now_ms();
  let result = f();
  let t1 = now_ms();
  ((t1 -. t0) *. 1e6, result);
};

/* --- Program generators --- */

/* Generate a let-chain program string with n bindings.
 * Each binding adds ~5-10 AST nodes. */
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

/* --- Pipeline timing --- */

type measurement = {
  name: string,
  time_ns: float,
};

/* Run the full pipeline on a zipper, timing each phase individually.
 * Each phase feeds its real output to the next phase. */
let time_pipeline = (label: string, z: Zipper.t): list(measurement) => {
  let segment = Zipper.unselect_and_zip(z);

  let (t_mt, make_term_result) = time_call(() => MakeTerm.go(segment));
  let term = make_term_result.term;

  let (t_meas, _) =
    time_call(() => Measured.of_segment(segment, Id.Map.empty, Id.Map.empty));

  let ctx = Builtins.ctx_init(Some(Int));
  let (t_stat, info_map) =
    time_call(() => Statics.mk(CoreSettings.on, ctx, term));

  let (t_elab, (dhexp, _)) =
    time_call(() => Elaborator.elaborate(info_map, term));

  let (t_eval, _) =
    time_call(() => Evaluator.evaluate(~env=Builtins.env_init, dhexp));

  [
    {name: label ++ "/MakeTerm", time_ns: t_mt},
    {name: label ++ "/Measured", time_ns: t_meas},
    {name: label ++ "/Statics", time_ns: t_stat},
    {name: label ++ "/Elaborate", time_ns: t_elab},
    {name: label ++ "/Evaluate", time_ns: t_eval},
  ];
};

/* Run pipeline with a Perform action as the first timed phase. */
let time_pipeline_with_action =
    (label: string, z: Zipper.t, syntax: CachedSyntax.t, action: Action.t)
    : list(measurement) => {
  let (t_perf, acted_z) =
    time_call(() =>
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
      }
    );

  let pipeline = time_pipeline(label, acted_z);
  [{name: label ++ "/Perform", time_ns: t_perf}, ...pipeline];
};

/* --- Cache isolation --- */

/* Clone a zipper with fresh IDs on every piece. This ensures
 * Core.Memo.general (structural equality) and WeakMap (physical identity)
 * caches miss, giving true cold-start measurements each repetition.
 * Uses Segment.IDs.replace_piece which recursively freshens children. */
let fresh_ids = (z: Zipper.t): Zipper.t =>
  ZipperBase.MapPiece.go(p => [Segment.IDs.replace_piece(p)], z);

/* --- Scenario runner --- */

type parsed_program = {
  label: string,
  z: Zipper.t,
};

/* Run all four scenarios for one program at one repetition. */
let run_scenarios = (prog: parsed_program): list(measurement) => {
  let z = fresh_ids(prog.z);
  let syntax = CachedSyntax.init(z);
  let label = prog.label;

  /* Cold: first run with this input, all caches empty for these values */
  let cold = time_pipeline(label ++ "/cold", z);

  /* Warm: identical inputs, caches now populated from cold run */
  let warm = time_pipeline(label ++ "/warm", z);

  /* Move: cursor movement then full pipeline */
  let move =
    time_pipeline_with_action(
      label ++ "/move",
      z,
      syntax,
      Move(Local(Left, ByChar)),
    );

  /* Modify: content edit then full pipeline */
  let modify =
    time_pipeline_with_action(
      label ++ "/modify",
      z,
      syntax,
      Insert("x"),
    );

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
let aggregate = (all_reps: list(list(measurement)), reps: int): list(measurement) => {
  let tbl: Hashtbl.t(string, array(float)) = Hashtbl.create(128);
  let order: ref(list(string)) = ref([]);

  List.iteri(
    (rep_idx, results) =>
      List.iter(
        (m: measurement) => {
          let arr =
            switch (Hashtbl.find_opt(tbl, m.name)) {
            | Some(a) => a
            | None =>
              let a = Array.make(reps, 0.0);
              Hashtbl.replace(tbl, m.name, a);
              order := [m.name, ...order^];
              a;
            };
          arr[rep_idx] = m.time_ns;
        },
        results,
      ),
    all_reps,
  );

  List.rev(order^)
  |> List.map(name => {name, time_ns: median(Hashtbl.find(tbl, name))});
};

/* Insert Total rows after each {size}/{scenario} group. */
let add_totals = (results: list(measurement)): list(measurement) => {
  let get_group = (name: string): string => {
    /* Find the last '/' and return everything before it */
    let last = ref(0);
    for (i in 0 to String.length(name) - 1) {
      if (name.[i] == '/') {
        last := i;
      };
    };
    String.sub(name, 0, last^);
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
    List.fold_left((acc, m: measurement) => max(acc, String.length(m.name)), 10, results);
  let col_w = 12;

  Printf.printf("\n%-*s  %*s\n", name_w, "Benchmark", col_w, "Time (median)");
  Printf.printf("%s\n", String.make(name_w + col_w + 2, '-'));

  let prev_group = ref("");
  List.iter(
    (m: measurement) => {
      let group = {
        let last = ref(0);
        for (i in 0 to String.length(m.name) - 1) {
          if (m.name.[i] == '/') {
            last := i;
          };
        };
        String.sub(m.name, 0, last^);
      };
      if (group != prev_group^) {
        if (prev_group^ != "") {
          print_newline();
        };
        prev_group := group;
      };
      Printf.printf("%-*s  %*s\n", name_w, m.name, col_w, format_time(m.time_ns));
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
  let reps = parse_int_arg(argv, "--reps", 7);
  let filters = parse_filters(argv);

  /* Parse all programs once (expensive). Subsequent repetitions clone
   * the zipper with fresh IDs instead of re-parsing. */
  let programs = [
    ("let100", gen_let_chain(100)),
    ("let500", gen_let_chain(500)),
  ];
  /* Only parse programs that match the filter (parsing is expensive).
   * A filter like "let100" or "let100/cold" targets a specific program;
   * a filter like "cold" or "MakeTerm" doesn't target any program and
   * requires all programs to be parsed. */
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
              /* Pattern matches this program's label */
              is_substring(label, pat)
              /* Pattern starts with this program's label (e.g. "let100/cold") */
              || String.length(pat) >= String.length(label)
              && String.sub(pat, 0, String.length(label)) == label
              /* Pattern targets scenarios/phases, not a specific program */
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

  Printf.eprintf("==> Running %d repetitions\n%!", reps);

  /* Collect results from all repetitions */
  let all_reps =
    List.init(reps, rep => {
      Printf.eprintf("==> Repetition %d/%d\n%!", rep + 1, reps);
      List.flatten(List.map(prog => run_scenarios(prog), parsed));
    });

  /* Take median across repetitions */
  let results = aggregate(all_reps, reps);

  /* Add Total rows */
  let results = add_totals(results);

  /* Apply filters */
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

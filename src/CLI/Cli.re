open Cmdliner;

/* Read from stdin or file depending on argument */
let read_input = path => {
  Core.(
    switch (path) {
    | "-" => In_channel.input_all(In_channel.stdin)
    | file => In_channel.read_all(file)
    }
  );
};

/* Fast-first parsing shared with persistence load
   (PersistentZipper.parse_text); the difference is failure policy —
   a CLI caller wants a reported error (None → failwith below), where
   persistence loads an empty buffer rather than brick boot. */
let parse_to_zipper = (s: string): option(Haz3lcore.Zipper.t) =>
  Haz3lcore.(PersistentZipper.parse_text(~source="cli", ~root=Exp, s));

let parse_program = (s: string) =>
  switch (parse_to_zipper(s)) {
  | Some(z) => Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term
  | None => failwith("Failed to parse expression: " ++ s)
  };

let run_hazel = path => {
  let program = read_input(path);
  let parsed = parse_program(program);
  let evaluated = Run.evaluate(parsed);
  print_endline(Print.print(evaluated));
};

let strip_leading_whitespace = (s: string): string => {
  let lines = String.split_on_char('\n', s);
  let stripped = List.map(String.trim, lines);
  String.concat("\n", stripped);
};

let format_hazel = (implicit_hole: string, width, path) => {
  let program = read_input(path) |> strip_leading_whitespace;
  /* Parse to a zipper (not just a segment) so we recover manual refractors
     produced by `^^probe(...)` / `^^statics(...)` triggers in the input.
     The refractors are then passed to Printer.of_segment so they round-trip
     back to their trigger syntax. Both convex and concave Grout are rendered
     with `implicit_hole` so the marker survives a decode|format|encode pipe. */
  switch (parse_to_zipper(program)) {
  | None => failwith("Failed to parse: " ++ path)
  | Some(zipper) =>
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);
    let pretty_seg = Haz3lcore.PrettySegment.prettify(~width, segment);
    let output =
      Haz3lcore.Printer.of_segment(
        ~holes=implicit_hole,
        ~concave_holes=implicit_hole,
        ~indent=" ",
        ~refractors=zipper.refractors.manuals,
        pretty_seg,
      );
    print_endline(output);
  };
};

let analyze_hazel =
    (show_warnings: bool, path: string)
    : [>
        | `Error(bool, string)
        | `Ok(unit)
      ] => {
  let program = read_input(path);
  /* Parse to zipper to preserve structure for measurement */
  switch (parse_to_zipper(program)) {
  | None =>
    prerr_endline("Failed to parse program");
    `Error((false, "Parse error"));
  | Some(zipper) =>
    open Language;
    open Util;
    /* Get segment and term */
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;

    /* Compute measured positions */
    let measured =
      Haz3lcore.Measured.of_segment(
        segment,
        Haz3lcore.ProjectorCore.Shape.Map.empty,
        Id.Map.empty,
      );

    /* Run static analysis */
    let (static_map, _) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      );

    /* Get errors with their infos for line numbers */
    let formatted_errors =
      Id.Map.fold(
        (_id, info, acc) =>
          switch (
            Diagnostic.format_error_with_location(
              ~source=program,
              ~path,
              measured,
              info,
            )
          ) {
          | None => acc
          | Some(err) => [err, ...acc]
          },
        static_map,
        [],
      )
      |> List.sort_uniq(compare);

    let formatted_warnings =
      if (show_warnings) {
        Id.Map.fold(
          (_id, info, acc) =>
            switch (
              Diagnostic.format_warning_with_location(
                ~source=program,
                ~path,
                measured,
                info,
              )
            ) {
            | None => acc
            | Some(w) => [w, ...acc]
            },
          static_map,
          [],
        )
        |> List.sort_uniq(compare);
      } else {
        [];
      };

    let print_diagnostics = (label, items) =>
      switch (items) {
      | [] => ()
      | _ =>
        let count = List.length(items);
        prerr_endline(
          "Found "
          ++ string_of_int(count)
          ++ " "
          ++ label
          ++ (count > 1 ? "s" : "")
          ++ ":",
        );
        prerr_endline("");
        List.iter(
          item => {
            prerr_endline(item);
            prerr_endline("");
          },
          items,
        );
      };

    switch (formatted_errors, formatted_warnings) {
    | ([], []) =>
      let msg =
        show_warnings
          ? "No static errors or warnings found." : "No static errors found.";
      print_endline(msg);
      `Ok();
    | (errors, warnings) =>
      print_diagnostics("static error", errors);
      print_diagnostics("warning", warnings);
      /* Warnings alone do not fail the run; only errors set a non-zero exit
         code. This matches `cargo check` / `gcc -Wall` semantics and keeps
         `analyze -W` usable in CI without forcing every warning to be fatal. */
      if (errors == []) {
        `Ok();
      } else {
        `Error((false, "Static errors found"));
      };
    };
  };
};

/* Extract source text for a given ID from the source file */
let extract_source_text =
    (~source: string, ~measured: Haz3lcore.Measured.t, id: Util_web.Id.t)
    : option(string) => {
  switch (Haz3lcore.Measured.find_by_id(id, measured)) {
  | Some({origin, last}) =>
    let lines = Diagnostic.lines_of_string(source);
    if (origin.row >= 0 && origin.row < Array.length(lines)) {
      if (origin.row == last.row) {
        /* Single line - extract substring */
        Some(
          Diagnostic.slice_columns(lines[origin.row], origin.col, last.col),
        );
      } else {
        /* Multi-line - extract from origin to end of first line */
        Some(
          Diagnostic.suffix_from_column(lines[origin.row], origin.col)
          ++ "...",
        );
      };
    } else {
      None;
    };
  | None => None
  };
};

/* Format a single test result for display */
let format_test_result =
    (
      ~source: string,
      ~measured: Haz3lcore.Measured.t,
      ~verbose: bool,
      id: Util_web.Id.t,
      reports: list(Language.TestMap.instance_report),
    )
    : option(string) => {
  open Language;
  let status = TestMap.joint_status(reports);
  let hint =
    switch (reports) {
    | [{hint, _}, ..._] when hint != "No hint available." => Some(hint)
    | _ => None
    };

  /* Skip passing tests unless verbose */
  if (!verbose && status == TestStatus.Pass) {
    None;
  } else {
    let status_str =
      switch (status) {
      | Pass => "PASS"
      | Fail => "FAIL"
      | Indet => "INDET"
      };

    /* Get line number */
    let location =
      switch (Haz3lcore.Measured.find_by_id(id, measured)) {
      | Some({origin, _}) => "line " ++ string_of_int(origin.row + 1)
      | None => "unknown location"
      };

    /* Format hint if present */
    let hint_str =
      switch (hint) {
      | Some(h) => ", \"" ++ h ++ "\""
      | None => ""
      };

    /* Get source text */
    let source_text =
      switch (extract_source_text(~source, ~measured, id)) {
      | Some(text) => text
      | None => "<source unavailable>"
      };

    Some(status_str ++ " [" ++ location ++ hint_str ++ "]: " ++ source_text);
  };
};

/* Run tests in a Hazel program and report results */
let test_hazel =
    (verbose: bool, path: string)
    : [>
        | `Error(bool, string)
        | `Ok(unit)
      ] => {
  let program = read_input(path);
  switch (parse_to_zipper(program)) {
  | None =>
    prerr_endline("Failed to parse program");
    `Error((false, "Parse error"));
  | Some(zipper) =>
    open Language;
    open Util;
    /* Get segment and term */
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;

    /* Compute measured positions for source text extraction */
    let measured =
      Haz3lcore.Measured.of_segment(
        segment,
        Haz3lcore.ProjectorCore.Shape.Map.empty,
        Id.Map.empty,
      );

    /* Evaluate and get test results */
    let (_, test_results) = Run.evaluate_with_tests(term);

    /* Print summary */
    print_endline(
      "Test Results: " ++ TestResults.test_summary_str(test_results),
    );
    print_endline("");

    /* Format individual test results */
    let formatted_tests =
      List.filter_map(
        ((id, reports)) =>
          format_test_result(
            ~source=program,
            ~measured,
            ~verbose,
            id,
            reports,
          ),
        test_results.test_map,
      );

    /* Print test results */
    List.iter(line => print_endline(line), formatted_tests);

    /* Return appropriate exit code */
    if (test_results.failing > 0) {
      `Error((false, "Tests failed"));
    } else {
      `Ok();
    };
  };
};

/* Run program with probes and display results inline */
let probe_hazel = (auto: bool, many: bool, path: string): unit => {
  let program = read_input(path);
  switch (parse_to_zipper(program)) {
  | None => prerr_endline("Failed to parse program")
  | Some(zipper) =>
    /* Get the segment */
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);

    /* Get term for evaluation */
    let make_term_result =
      Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp);
    let term = make_term_result.term;
    open Language;

    /* Probe ids up front — statics needs them too, since instrumentation
       (e.g. the livelit view fold-in) happens during elaboration */
    let base_probe_ids = Haz3lcore.CachedStatics.probe_ids_of_zipper(zipper);

    /* Run statics to get info_map and the (instrumented) elaboration */
    let (info_map, elaborated) =
      Statics.mk(
        ~probe_ids=base_probe_ids,
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      );

    /* If --auto, compute auto-probe IDs */
    let auto_ids =
      if (auto) {
        /* Build syntax cache for MultiProbe */
        let syntax =
          Haz3lcore.CachedSyntax.mk(zipper, ~info_map, ~dyn_map=Id.Map.empty);
        let root_id =
          Haz3lcore.Segment.root_id(
            Haz3lcore.Segment.skel(segment),
            segment,
          );

        switch (
          Haz3lcore.MultiProbe.ids_to_multiprobe(
            root_id,
            syntax.term_data,
            syntax.terms,
            syntax.measured,
            info_map,
          )
        ) {
        | Some(ids) =>
          List.fold_left(
            (acc, id_opt) =>
              switch (id_opt) {
              | Some(id) => Id.Map.add(id, (), acc)
              | None => acc
              },
            Id.Map.empty,
            ids,
          )
        | None => Id.Map.empty
        };
      } else {
        Id.Map.empty;
      };

    /* Combine base (manual + projector) and auto probes */
    let probe_ids =
      Id.Map.union((_, _, _) => Some(), base_probe_ids, auto_ids);

    /* Build probe_map - tells evaluator which expressions to record */
    let sample_map =
      Haz3lcore.CachedStatics.compute_targets(
        ~settings=CoreSettings.on,
        ~info_map,
        ~probe_ids,
      );

    /* Evaluate the elaboration from the same statics run (it may carry
       probe instrumentation) to collect probe samples */
    let sample_map = {
      let (_, state) =
        Evaluator.evaluate(
          ~eval_info=EvalInfo.of_targets(sample_map),
          ~env=Builtins.env_init,
          elaborated,
        );
      EvaluatorState.get_probes(state);
    };

    /* Format output with probe values */
    let window: Sample.Window.mode =
      many ? Sample.Window.Many : Sample.Window.Single;

    /* For auto-probe, we need to pass the auto IDs as refractors for rendering */
    let refractors =
      if (auto) {
        /* Build a refractor list that includes auto IDs */
        let auto_entries =
          Id.Map.fold(
            (id, (), acc) =>
              [(id, Haz3lcore.Refractors.mk_entry(Probe)), ...acc],
            auto_ids,
            [],
          );
        zipper.refractors.manuals @ auto_entries;
      } else {
        zipper.refractors.manuals;
      };

    let output =
      Haz3lcore.ProbeText.of_segment(
        ~window,
        ~probe_map=sample_map,
        ~refractors,
        segment,
      );
    print_endline(output);
  };
};

/* Benchmark evaluation performance: parse+statics once, evaluate N times.
 * "Plain" is the non-incremental path (prev/eval_info empty, as used by the
 * CLI `run` and MVU apps); "Incr" is a cache-seeding incremental run
 * (eval_info populated, prev empty, as used by the web editor's first run). */
let bench_eval = (iterations: int, paths: list(string)): unit => {
  let now = () =>
    Js_of_ocaml.Js.Unsafe.global##.performance##now()##valueOf
    |> Js_of_ocaml.Js.float_of_number;
  Printf.printf(
    "%-40s %8s %12s %12s\n",
    "File",
    "Iters",
    "Plain(ms)",
    "Incr(ms)",
  );
  Printf.printf("%s\n", String.make(76, '-'));
  List.iter(
    path => {
      let program = read_input(path);
      let parsed = parse_program(program);
      let (elab, eval_info) = Run.elab_and_eval_info(parsed);
      ignore(Run.evaluate_elab(elab));
      let t0 = now();
      for (_ in 1 to iterations) {
        ignore(Run.evaluate_elab(elab));
      };
      let t1 = now();
      let plain = (t1 -. t0) /. float_of_int(iterations);
      ignore(Run.evaluate_elab_incr(~eval_info, elab));
      let t2 = now();
      for (_ in 1 to iterations) {
        ignore(Run.evaluate_elab_incr(~eval_info, elab));
      };
      let t3 = now();
      let incr = (t3 -. t2) /. float_of_int(iterations);
      Printf.printf(
        "%-40s %8d %12.1f %12.1f\n",
        path,
        iterations,
        plain,
        incr,
      );
    },
    paths,
  );
};

/* Common arg: path or "-" for stdin */
let input_arg = {
  let doc = "Path to Hazel source file, or '-' to read from stdin.";
  Arg.(
    required & pos(0, some(string), None) & info([], ~docv="INPUT", ~doc)
  );
};

let run_cmd = {
  let doc = "Run a Hazel program.";
  let info = Cmd.info("run", ~doc);
  Cmd.v(info, Term.(const(run_hazel) $ input_arg));
};

let implicit_hole_arg = {
  let doc =
    "Character used to render implicit holes (Grout) in output. "
    ++ "Default is `¿` (U+00BF), a single non-identifier, non-operator "
    ++ "token that round-trips through `format` and is recognised by "
    ++ "`slide-encode` so Grout positions are recovered on re-parse.";
  Arg.(
    value
    & opt(string, Haz3lcore.MarkerParse.default_implicit_hole)
    & info(["implicit-hole"], ~docv="CHAR", ~doc)
  );
};

let format_cmd = {
  let doc = {|
    Pretty-prints Hazel code, inserting line breaks to fit within a target
    width. Preserves comments but replaces original whitespace with
    structured formatting.
  |};
  let width_arg = {
    let doc = "Target line width in columns (default: 60).";
    Arg.(value & opt(int, 60) & info(["w", "width"], ~doc));
  };
  let info = Cmd.info("format", ~doc);
  Cmd.v(
    info,
    Term.(const(format_hazel) $ implicit_hole_arg $ width_arg $ input_arg),
  );
};

let analyze_cmd = {
  let doc = "Perform static analysis on Hazel code.";
  let warnings_arg = {
    let doc = "Also report warnings (e.g. unused variables).";
    Arg.(value & flag & info(["W", "warnings"], ~doc));
  };
  let info = Cmd.info("analyze", ~doc);
  Cmd.v(
    info,
    Term.ret(Term.(const(analyze_hazel) $ warnings_arg $ input_arg)),
  );
};

let probe_cmd = {
  let doc = "Run a Hazel program and display probe values inline.";
  let auto_arg = {
    let doc = "Auto-probe all expressions (one per line).";
    Arg.(value & flag & info(["auto", "a"], ~doc));
  };
  let many_arg = {
    let doc = "Show multiple sample values per probe (many mode).";
    Arg.(value & flag & info(["many", "m"], ~doc));
  };
  let info = Cmd.info("probe", ~doc);
  Cmd.v(info, Term.(const(probe_hazel) $ auto_arg $ many_arg $ input_arg));
};

let test_cmd = {
  let doc = "Run tests in a Hazel program and report results.";
  let verbose_arg = {
    let doc = "Show all tests, not just failures.";
    Arg.(value & flag & info(["verbose", "v"], ~doc));
  };
  let info = Cmd.info("test", ~doc);
  Cmd.v(info, Term.ret(Term.(const(test_hazel) $ verbose_arg $ input_arg)));
};

let output_arg = {
  let doc = "Path to write output to. If omitted, output is written to stdout.";
  Arg.(value & opt(some(string), None) & info(["output", "o"], ~doc));
};

let submission_arg = {
  let doc = "Path to a submission JSON file (exported from Hazel).";
  Arg.(
    required
    & pos(0, some(string), None)
    & info([], ~docv="SUBMISSION", ~doc)
  );
};

let grade_json_cmd = {
  let doc =
    "Grade a submission and emit the raw grading output as JSON. "
    ++ "The submission file is the JSON export produced by Hazel's export "
    ++ "feature (matching the schema of the in-app exercise store).";
  let info = Cmd.info("grade-json", ~doc);
  Cmd.v(info, Term.(const(Grade.grade_json) $ submission_arg $ output_arg));
};

let grade_report_cmd = {
  let doc =
    "Grade a submission and emit a human-readable text report. "
    ++ "For each exercise, prints the title, score, and summary. "
    ++ "Ends with a total across all exercises.";
  let info = Cmd.info("grade-report", ~doc);
  Cmd.v(
    info,
    Term.(const(Grade.grade_report) $ submission_arg $ output_arg),
  );
};

let bench_eval_cmd = {
  let doc = "Benchmark evaluation performance on one or more .hz files.";
  let iterations_arg = {
    let doc = "Number of iterations per file (default: 10).";
    Arg.(value & opt(int, 10) & info(["n", "iterations"], ~doc));
  };
  let files_arg = {
    let doc = "Hazel source files to benchmark.";
    Arg.(non_empty & pos_all(string, []) & info([], ~docv="FILES", ~doc));
  };
  let info = Cmd.info("bench-eval", ~doc);
  Cmd.v(info, Term.(const(bench_eval) $ iterations_arg $ files_arg));
};

/* Default to help if no subcommand is given */
let default_cmd = {
  let doc = "CLI tool for running and analyzing Hazel programs.";
  let info = Cmd.info("hazel", ~doc);
  Cmd.group(
    info,
    [
      run_cmd,
      format_cmd,
      analyze_cmd,
      probe_cmd,
      test_cmd,
      grade_json_cmd,
      grade_report_cmd,
      bench_eval_cmd,
    ],
  );
};

let () = exit(Cmd.eval(default_cmd));

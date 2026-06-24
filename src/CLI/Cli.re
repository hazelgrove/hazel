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

let parse_program = (s: string) =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => failwith("Failed to parse expression: " ++ s)
  };

/* Build a consent callback for CSV access. By default the CLI prompts before
   reading each referenced file and lets the user substitute a different local
   path; `--yes` auto-allows the declared paths for non-interactive use. When
   the program itself was read from stdin we cannot also prompt on stdin, so
   consent must be pre-granted with `--yes`. */
let make_authorizer =
    (~assume_yes: bool, ~from_stdin: bool, ~base_dir: string)
    : (string => Csv.decision) =>
  if (assume_yes) {
    path => Csv.Allow(path);
  } else if (from_stdin) {
    _path => {
      prerr_endline(
        "Refusing to read a CSV without consent: the program was read from "
        ++ "stdin, so no interactive prompt is available. Re-run with --yes to "
        ++ "authorize, or pass the program as a file.",
      );
      Csv.Deny;
    };
  } else {
    path => {
      let resolved = Csv.resolve(~base_dir, path);
      prerr_string(
        "Hazel wants to read CSV file:\n  "
        ++ resolved
        ++ "\n  [Enter] allow  ·  type another path to use instead  ·  [n] deny: ",
      );
      flush(stderr);
      switch (
        try(Some(input_line(stdin))) {
        | End_of_file => None
        }
      ) {
      | Some("")
      | Some("y")
      | Some("Y") => Csv.Allow(path)
      | None
      | Some("n")
      | Some("N") => Csv.Deny
      | Some(other) => Csv.Allow(String.trim(other))
      };
    };
  };

/* Choose the seed for each `^^seed(N)` reference. Under `--yes` (or stdin, where we
   can't prompt) we keep the source default N for reproducible non-interactive runs.
   Otherwise we prompt, letting the caller keep N, type a different integer, or draw a
   fresh OS-random seed ("r") — entropy a pure program can't produce on its own. */
let make_seed_chooser = (~assume_yes: bool, ~from_stdin: bool): (int => int) =>
  if (assume_yes || from_stdin) {
    default => default;
  } else {
    default => {
      prerr_string(
        "Hazel will use seed: "
        ++ string_of_int(default)
        ++ "\n  [Enter] keep  ·  type an integer to use instead  ·  [r] fresh random: ",
      );
      flush(stderr);
      switch (
        try(Some(input_line(stdin))) {
        | End_of_file => None
        }
      ) {
      | None
      | Some("")
      | Some("y")
      | Some("Y") => default
      | Some("r")
      | Some("R") =>
        Random.self_init();
        Random.int(1000000000); /* any seed in [0, 1e9); Random.int bound must be < 2^30 */
      | Some(other) =>
        switch (int_of_string_opt(String.trim(other))) {
        | Some(v) => v
        | None => default
        }
      };
    };
  };

/* Read a .hz file, resolve `^^seed(...)` references to integer literals, and build
   the (base_dir, consent callback) for CSV access. */
let setup_csv =
    (~assume_yes: bool, ~data_dir: option(string), path: string)
    : (string, string, string => Csv.decision) => {
  let from_stdin = path == "-";
  let program =
    Seed.splice(
      ~choose=make_seed_chooser(~assume_yes, ~from_stdin),
      read_input(path),
    );
  let base_dir =
    switch (data_dir) {
    | Some(d) => d
    | None => Filename.dirname(path)
    };
  let authorize = make_authorizer(~assume_yes, ~from_stdin, ~base_dir);
  (program, base_dir, authorize);
};

/* Text expansion (used by `expand`): inline `^^csv(...)` into `^^table([...])`.
   Materializes a portable .hz; the inlined table is large and slow to re-parse. */
let read_and_expand =
    (~assume_yes: bool, ~data_dir: option(string), path: string): string => {
  let (program, base_dir, authorize) =
    setup_csv(~assume_yes, ~data_dir, path);
  Csv.expand(~base_dir, ~authorize, program);
};

/* Run a thunk that may touch the filesystem for CSV access, reporting a denied
   or unreadable file as a clean error + non-zero exit (not a stack trace). */
let die_on_csv_error = (f: unit => 'a): 'a =>
  try(f()) {
  | Failure(msg)
  | Sys_error(msg) =>
    prerr_endline("hazel: " ++ msg);
    exit(1);
  };

let run_hazel = (assume_yes, data_dir, path) => {
  let (program, base_dir, authorize) =
    setup_csv(~assume_yes, ~data_dir, path);
  /* Splice CSV tables as AST so the (large) table is never re-parsed. */
  let spliced =
    die_on_csv_error(() =>
      Csv.splice_tables(~base_dir, ~authorize, ~parse=parse_program, program)
    );
  let evaluated = Run.evaluate(spliced);
  print_endline(Print.print(evaluated));
};

let expand_hazel = (assume_yes, data_dir, output, path) => {
  let expanded =
    die_on_csv_error(() => read_and_expand(~assume_yes, ~data_dir, path));
  switch (output) {
  | None => print_string(expanded)
  | Some(out) =>
    let oc = open_out(out);
    output_string(oc, expanded);
    close_out(oc);
  };
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
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, program)) {
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

/* Parse program and return zipper (preserving projectors like probes) */
let parse_to_zipper = (s: string): option(Haz3lcore.Zipper.t) =>
  Haz3lcore.Parser.to_zipper(~root=Exp, s);

let analyze_hazel =
    (
      show_warnings: bool,
      assume_yes: bool,
      data_dir: option(string),
      path: string,
    )
    : [>
        | `Error(bool, string)
        | `Ok(unit)
      ] => {
  let (program, base_dir, authorize) =
    setup_csv(~assume_yes, ~data_dir, path);
  /* Splice CSV tables as AST: parse only the skeleton, then bind the tables.
     Diagnostics map against the skeleton (the table has no source positions). */
  let (skeleton, refs) = Csv.extract_refs(program);
  /* Parse to zipper to preserve structure for measurement */
  switch (parse_to_zipper(skeleton)) {
  | None =>
    prerr_endline("Failed to parse program");
    `Error((false, "Parse error"));
  | Some(zipper) =>
    open Language;
    open Util;
    /* Get segment and term */
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);
    let term =
      die_on_csv_error(() =>
        Csv.wrap_lets(
          ~base_dir,
          ~authorize,
          refs,
          Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term,
        )
      );

    /* Compute measured positions */
    let measured =
      Haz3lcore.Measured.of_segment(
        segment,
        Haz3lcore.ProjectorCore.Shape.Map.empty,
        Id.Map.empty,
      );

    /* Run static analysis */
    let (static_map, _) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);

    /* Get errors with their infos for line numbers */
    let formatted_errors =
      Id.Map.fold(
        (_id, info, acc) =>
          switch (
            Diagnostic.format_error_with_location(
              ~source=skeleton,
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
                ~source=skeleton,
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
    (~source: string, ~measured: Haz3lcore.Measured.t, id: Util.Id.t)
    : option(string) => {
  switch (Haz3lcore.Measured.find_by_id(id, measured)) {
  | Some({origin, last}) =>
    let lines = Diagnostic.lines_of_string(source);
    if (origin.row >= 0 && origin.row < Array.length(lines)) {
      if (origin.row == last.row) {
        /* Single line - extract substring */
        let line = lines[origin.row];
        let start_col = max(0, origin.col);
        let end_col = min(String.length(line), last.col);
        Some(String.sub(line, start_col, end_col - start_col));
      } else {
        /* Multi-line - extract from origin to end of first line */
        let first_line = lines[origin.row];
        let start_col = max(0, origin.col);
        Some(
          String.sub(
            first_line,
            start_col,
            String.length(first_line) - start_col,
          )
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
      id: Util.Id.t,
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
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
    open Language;

    /* Run statics to get info_map */
    let (info_map, _) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);

    /* Get manual probe IDs */
    let manual_ids =
      List.fold_left(
        (map, (id, _)) => Id.Map.add(id, (), map),
        Id.Map.empty,
        zipper.refractors.manuals,
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

    /* Combine manual and auto probes */
    let probe_ids = Id.Map.union((_, _, _) => Some(), manual_ids, auto_ids);

    /* Build probe_map - tells evaluator which expressions to record */
    let sample_map =
      Haz3lcore.CachedStatics.compute_targets(
        ~settings=CoreSettings.on,
        ~info_map,
        ~probe_ids,
      );

    /* Evaluate with probe_map to collect probe samples */
    let (_, sample_map) = Run.evaluate_with_probe_map(~sample_map, term);

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

/* Benchmark parsing performance */
let bench_parse = (iterations: int, paths: list(string)): unit => {
  let now = () =>
    Js_of_ocaml.Js.Unsafe.global##.performance##now()##valueOf
    |> Js_of_ocaml.Js.float_of_number;

  /* Measure baseline (empty string parse) */
  let baseline = {
    let t0 = now();
    for (_ in 1 to iterations) {
      ignore(Haz3lcore.Parser.to_zipper(~root=Exp, ""));
    };
    let t1 = now();
    (t1 -. t0) /. float_of_int(iterations);
  };

  Printf.printf(
    "Baseline (empty parse): %.3fms per iteration (%d iterations)\n\n",
    baseline,
    iterations,
  );
  Printf.printf(
    "%-50s %8s %8s %10s %10s %10s %10s %10s %10s\n",
    "File",
    "Chars",
    "Lines",
    "Orig(ms)",
    "Seg(ms)",
    "Speedup",
    "Paste(ms)",
    "Fast(ms)",
    "Speedup",
  );
  Printf.printf("%s\n", String.make(140, '-'));

  List.iter(
    path => {
      let program = read_input(path);
      let chars = String.length(program);
      let lines = List.length(String.split_on_char('\n', program));

      /* Warmup both */
      ignore(Haz3lcore.Parser.to_zipper(~root=Exp, program));
      ignore(Haz3lcore.Parser.to_segment(program, ~root=Exp));

      /* Time unsegmented (to_zipper) */
      let t0 = now();
      for (_ in 1 to iterations) {
        ignore(Haz3lcore.Parser.to_zipper(~root=Exp, program));
      };
      let t1 = now();
      let orig_avg = (t1 -. t0) /. float_of_int(iterations);

      /* Time segmented (to_segment) */
      let t2 = now();
      for (_ in 1 to iterations) {
        ignore(Haz3lcore.Parser.to_segment(program, ~root=Exp));
      };
      let t3 = now();
      let seg_avg = (t3 -. t2) /. float_of_int(iterations);

      /* Time paste: slow (char-by-char into empty zipper) */
      let t4 = now();
      for (_ in 1 to iterations) {
        ignore(
          Haz3lcore.Parser.to_zipper(
            ~root=Exp,
            ~zipper_init=Haz3lcore.Zipper.init(),
            program,
          ),
        );
      };
      let t5 = now();
      let paste_slow = (t5 -. t4) /. float_of_int(iterations);

      /* Time paste: fast (segment splice) */
      let z_init = Haz3lcore.Zipper.init();
      let t6 = now();
      for (_ in 1 to iterations) {
        ignore(Haz3lcore.Parser.fast_paste(program, z_init, ~root=Exp));
      };
      let t7 = now();
      let paste_fast = (t7 -. t6) /. float_of_int(iterations);

      let speedup = orig_avg /. seg_avg;
      let paste_speedup = paste_slow /. paste_fast;
      Printf.printf(
        "%-50s %8d %8d %10.1f %10.1f %9.2fx %10.1f %10.1f %9.2fx\n",
        path,
        chars,
        lines,
        orig_avg,
        seg_avg,
        speedup,
        paste_slow,
        paste_fast,
        paste_speedup,
      );
    },
    paths,
  );

  Printf.printf("\n");
};

/* Common arg: path or "-" for stdin */
let input_arg = {
  let doc = "Path to Hazel source file, or '-' to read from stdin.";
  Arg.(
    required & pos(0, some(string), None) & info([], ~docv="INPUT", ~doc)
  );
};

/* Resolve relative `^^csv("...")` paths against this directory (default: the
   input file's directory). */
let data_dir_arg = {
  let doc =
    "Directory that relative `^^csv(\"...\")` paths resolve against "
    ++ "(default: the input file's directory).";
  Arg.(
    value & opt(some(string), None) & info(["data-dir"], ~docv="DIR", ~doc)
  );
};

/* Pre-authorize CSV reads so no interactive prompt is shown. */
let assume_yes_arg = {
  let doc =
    "Authorize reading every `^^csv(\"...\")` file without prompting "
    ++ "(required for non-interactive use).";
  Arg.(value & flag & info(["yes", "y"], ~doc));
};

let run_cmd = {
  let doc = "Run a Hazel program.";
  let info = Cmd.info("run", ~doc);
  Cmd.v(
    info,
    Term.(const(run_hazel) $ assume_yes_arg $ data_dir_arg $ input_arg),
  );
};

let implicit_hole_arg = {
  let doc =
    "Character used to render implicit holes (Grout) in output. "
    ++ "Default is `¿` (U+00BF), a single non-identifier, non-operator "
    ++ "token that round-trips through `format` and is recognised by "
    ++ "`slide-encode` so Grout positions are recovered on re-parse.";
  Arg.(
    value
    & opt(string, Haz3lcore.TextRoundtrip.default_implicit_hole)
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
    Term.ret(
      Term.(
        const(analyze_hazel)
        $ warnings_arg
        $ assume_yes_arg
        $ data_dir_arg
        $ input_arg
      ),
    ),
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

let expand_cmd = {
  let doc =
    "Expand `^^csv(\"...\")` references into inline `^^table([...])` literals, "
    ++ "producing a self-contained Hazel program that needs no further file "
    ++ "access. Writes to --output or stdout.";
  let info = Cmd.info("expand", ~doc);
  Cmd.v(
    info,
    Term.(
      const(expand_hazel)
      $ assume_yes_arg
      $ data_dir_arg
      $ output_arg
      $ input_arg
    ),
  );
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

/* ---------------- Slide commands ---------------- */

/* Slides are addressed by name (their first-tuple-element title). The list
   of slides is the one statically linked into the binary by Web.Init, so
   no on-disk .ml parsing is needed.
     * slide-list   - print every available slide name
     * slide-decode - print one slide's plaintext (Grout rendered as the
                      `--implicit-hole` marker, default `¿`; refractors
                      and projectors as their `^^…(...)` trigger syntax)
     * slide-encode - rebuild a slide .ml from a title + plaintext;
                      `--implicit-hole` markers are stripped via Destruct
                      and Grout is reinserted by remold/regrout
   Every other transformation (prettify, suppress warnings, analyze) is done
   generically on plaintext via `hazel format` / `hazel analyze` / etc. */

let slide_name_arg = {
  let doc = "Slide name (run `hazel slide-list` to see available names).";
  Arg.(
    required & pos(0, some(string), None) & info([], ~docv="NAME", ~doc)
  );
};

let lookup_slide_or_die = (name: string): Slide.slide =>
  switch (Slide.find(name)) {
  | Some(s) => s
  | None =>
    prerr_endline("slide: no slide named \"" ++ name ++ "\"");
    prerr_endline("Available names:");
    List.iter(n => prerr_endline("  " ++ n), Slide.list_names);
    exit(2);
  };

let slide_list = (): unit => List.iter(print_endline, Slide.list_names);

let slide_list_cmd = {
  let doc = "List the names of every slide linked into the binary.";
  let info = Cmd.info("slide-list", ~doc);
  Cmd.v(info, Term.(const(slide_list) $ const()));
};

let slide_decode = (implicit_hole: string, name: string): unit => {
  let slide = lookup_slide_or_die(name);
  /* `print_string`, not `print_endline`: the slide text already preserves
   * its trailing newlines; an extra `\n` would re-enter on re-parse as a
   * Secondary whitespace piece, breaking the CLI round-trip fixed-point
   * even though the core TextRoundtrip is fixed-point. */
  print_string(Slide.slide_to_text(~implicit_hole, slide));
};

let slide_decode_cmd = {
  let doc =
    "Print a named slide's program as plaintext. Manual refractors are "
    ++ "rendered with `^^probe(...)` / `^^statics(...)` trigger syntax so the "
    ++ "output is reparseable. Implicit holes (Grout) are rendered with "
    ++ "`--implicit-hole` (default `¿`) so `slide-encode` can identify and "
    ++ "strip them when round-tripping.";
  let info = Cmd.info("slide-decode", ~doc);
  Cmd.v(
    info,
    Term.(const(slide_decode) $ implicit_hole_arg $ slide_name_arg),
  );
};

let slide_encode =
    (
      implicit_hole: string,
      title: string,
      text_path: string,
      output: option(string),
    )
    : unit => {
  let text = read_input(text_path);
  let slide = Slide.text_to_slide(~implicit_hole, ~title, text);
  let rendered = Slide.render_slide_file(slide);
  switch (output) {
  | None => print_string(rendered)
  | Some(path) =>
    let oc = open_out(path);
    output_string(oc, rendered);
    close_out(oc);
  };
};

let slide_encode_cmd = {
  let doc =
    "Build a slide .ml file from a title and a Hazel plaintext program. "
    ++ "Refractors written as `^^probe(...)` / `^^statics(...)` in the input "
    ++ "are rebuilt by the parser's trigger module on insertion. "
    ++ "Markers matching `--implicit-hole` (default `¿`) are removed via "
    ++ "Destruct, letting the parser's remold/regrout pass reinsert Grout "
    ++ "in the canonical position.";
  let title_arg = {
    let doc = "Slide title (the first element of the persisted tuple).";
    Arg.(
      required
      & opt(some(string), None)
      & info(["title"], ~docv="TITLE", ~doc)
    );
  };
  let text_arg = {
    let doc = "Path to the program text, or '-' for stdin.";
    Arg.(
      required & pos(0, some(string), None) & info([], ~docv="TEXT", ~doc)
    );
  };
  let info = Cmd.info("slide-encode", ~doc);
  Cmd.v(
    info,
    Term.(
      const(slide_encode)
      $ implicit_hole_arg
      $ title_arg
      $ text_arg
      $ output_arg
    ),
  );
};

let bench_parse_cmd = {
  let doc = "Benchmark parsing performance on one or more .hz files.";
  let iterations_arg = {
    let doc = "Number of iterations per file (default: 5).";
    Arg.(value & opt(int, 5) & info(["n", "iterations"], ~doc));
  };
  let files_arg = {
    let doc = "Hazel source files to benchmark.";
    Arg.(non_empty & pos_all(string, []) & info([], ~docv="FILES", ~doc));
  };
  let info = Cmd.info("bench-parse", ~doc);
  Cmd.v(info, Term.(const(bench_parse) $ iterations_arg $ files_arg));
};

/* Default to help if no subcommand is given */
let default_cmd = {
  let doc = "CLI tool for running and analyzing Hazel programs.";
  let info = Cmd.info("hazel", ~doc);
  Cmd.group(
    info,
    [
      run_cmd,
      expand_cmd,
      format_cmd,
      analyze_cmd,
      probe_cmd,
      test_cmd,
      grade_json_cmd,
      grade_report_cmd,
      bench_parse_cmd,
      slide_list_cmd,
      slide_decode_cmd,
      slide_encode_cmd,
    ],
  );
};

let () = exit(Cmd.eval(default_cmd));

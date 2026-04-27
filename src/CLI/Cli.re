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
  switch (Haz3lcore.Parser.to_term(s)) {
  | Some(e) => e
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

let format_hazel = (width, path) => {
  let program = read_input(path) |> strip_leading_whitespace;
  /* Use segment-based path (like the editor's Cmd+S) to preserve comments.
     The AST round-trip (parse_program + segmentize) loses comments because
     MakeTerm drops Secondary pieces and ExpToSegment reconstructs from AST. */
  switch (Haz3lcore.Parser.to_segment(program)) {
  | None => failwith("Failed to parse: " ++ path)
  | Some(segment) =>
    let pretty_seg = Haz3lcore.PrettySegment.prettify(~width, segment);
    let output =
      Haz3lcore.Printer.of_segment(~holes="?", ~indent=" ", pretty_seg);
    print_endline(output);
  };
};

/* Parse program and return zipper (preserving projectors like probes) */
let parse_to_zipper = (s: string): option(Haz3lcore.Zipper.t) =>
  Haz3lcore.Parser.to_zipper(s);

/* Split string into lines */
let lines_of_string = (s: string): array(string) => {
  /* Handle both \n and \r\n line endings */
  let s = Core.String.substr_replace_all(s, ~pattern="\r\n", ~with_="\n");
  Array.of_list(String.split_on_char('\n', s));
};

/* Create a caret line pointing to error position */
let make_caret_line = (col: int, len: int): string => {
  let spaces = String.make(col, ' ');
  let carets = String.make(max(1, len), '^');
  spaces ++ carets;
};

/* Format error in Rust-style with source context */
let format_error_with_location =
    (
      ~source: string,
      ~path: string,
      measured: Haz3lcore.Measured.t,
      info: Language.Info.t,
    )
    : option(string) => {
  Language.(
    switch (Info.marks_of(info)) {
    | [] => None
    | marks =>
      let id = Info.id_of(info);
      let error_str = Haz3lcore.ErrorPrint.string_of_marks(info, marks);

      switch (Haz3lcore.Measured.find_by_id(id, measured)) {
      | Some({origin, last}) =>
        let lines = lines_of_string(source);
        let row = origin.row;
        let col = origin.col;
        /* Calculate length for single-line errors */
        let len =
          if (origin.row == last.row) {
            last.col - origin.col;
          } else {
            1;
          };

        /* Line numbers are 1-indexed for display */
        let line_num = row + 1;
        let line_num_str = string_of_int(line_num);
        let line_num_width = String.length(line_num_str);
        let padding = String.make(line_num_width, ' ');

        /* Get source line if available */
        let source_line =
          if (row >= 0 && row < Array.length(lines)) {
            lines[row];
          } else {
            "<source unavailable>";
          };

        /* Build Rust-style error output */
        let header = "error: " ++ error_str;
        let location =
          padding
          ++ " --> "
          ++ path
          ++ ":"
          ++ line_num_str
          ++ ":"
          ++ string_of_int(col + 1);
        let separator = padding ++ " |";
        let code_line = line_num_str ++ " | " ++ source_line;
        let caret_line = padding ++ " | " ++ make_caret_line(col, len);

        Some(
          String.concat(
            "\n",
            [header, location, separator, code_line, caret_line],
          ),
        );

      | None =>
        /* Fallback if no position found */
        let term_str = Haz3lcore.ErrorPrint.term_string_of(info);
        Some("error: " ++ error_str ++ "\n  in term: " ++ term_str);
      };
    }
  );
};

let analyze_hazel =
    (path: string)
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
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper).term;

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
            format_error_with_location(~source=program, ~path, measured, info)
          ) {
          | None => acc
          | Some(err) => [err, ...acc]
          },
        static_map,
        [],
      )
      |> List.sort_uniq(compare);

    switch (formatted_errors) {
    | [] =>
      print_endline("No static errors found.");
      `Ok();
    | _ =>
      let count = List.length(formatted_errors);
      prerr_endline(
        "Found "
        ++ string_of_int(count)
        ++ " static error"
        ++ (count > 1 ? "s" : "")
        ++ ":",
      );
      prerr_endline("");
      List.iter(
        error => {
          prerr_endline(error);
          prerr_endline("");
        },
        formatted_errors,
      );
      `Error((false, "Static errors found"));
    };
  };
};

/* Extract source text for a given ID from the source file */
let extract_source_text =
    (~source: string, ~measured: Haz3lcore.Measured.t, id: Util.Id.t)
    : option(string) => {
  switch (Haz3lcore.Measured.find_by_id(id, measured)) {
  | Some({origin, last}) =>
    let lines = lines_of_string(source);
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
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper).term;

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
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper).term;
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
      ignore(Haz3lcore.Parser.to_zipper(""));
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
      ignore(Haz3lcore.Parser.to_zipper(program));
      ignore(Haz3lcore.Parser.to_segment(program));

      /* Time unsegmented (to_zipper) */
      let t0 = now();
      for (_ in 1 to iterations) {
        ignore(Haz3lcore.Parser.to_zipper(program));
      };
      let t1 = now();
      let orig_avg = (t1 -. t0) /. float_of_int(iterations);

      /* Time segmented (to_segment) */
      let t2 = now();
      for (_ in 1 to iterations) {
        ignore(Haz3lcore.Parser.to_segment(program));
      };
      let t3 = now();
      let seg_avg = (t3 -. t2) /. float_of_int(iterations);

      /* Time paste: slow (char-by-char into empty zipper) */
      let t4 = now();
      for (_ in 1 to iterations) {
        ignore(
          Haz3lcore.Parser.to_zipper(
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
        ignore(Haz3lcore.Parser.fast_paste(program, z_init));
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

let run_cmd = {
  let doc = "Run a Hazel program.";
  let info = Cmd.info("run", ~doc);
  Cmd.v(info, Term.(const(run_hazel) $ input_arg));
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
  Cmd.v(info, Term.(const(format_hazel) $ width_arg $ input_arg));
};

let analyze_cmd = {
  let doc = "Perform static analysis on Hazel code.";
  let info = Cmd.info("analyze", ~doc);
  Cmd.v(info, Term.ret(Term.(const(analyze_hazel) $ input_arg)));
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

let gen_slides_cmd = {
  let doc = "Generate ML slide files from .hz programs. Optionally pass path prefixes to only rebuild matching files (e.g., ./hazel gen-slides tutorial debugging/grove-plotter).";
  let filter_arg = {
    let doc = "Path prefix filters (relative to hazel-programs/study/). Only .hz files matching these prefixes will be processed.";
    Arg.(value & pos_all(string, []) & info([], ~doc));
  };
  let info = Cmd.info("gen-slides", ~doc);
  Cmd.v(info, Term.(const(GenSlides.generate) $ filter_arg));
};

let gen_slides_clean_cmd = {
  let doc = "Remove generated slide files and restore stub.";
  let info = Cmd.info("gen-slides-clean", ~doc);
  Cmd.v(info, Term.(const(GenSlides.clean) $ const()));
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
      format_cmd,
      analyze_cmd,
      probe_cmd,
      test_cmd,
      gen_slides_cmd,
      gen_slides_clean_cmd,
      bench_parse_cmd,
    ],
  );
};

let () = exit(Cmd.eval(default_cmd));

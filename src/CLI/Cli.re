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

let format_hazel = path => {
  let program = read_input(path);
  let parsed = parse_program(program);
  print_endline(Print.print(parsed));
};

/* Parse program and return zipper (preserving projectors like probes) */
let parse_to_zipper = (s: string): option(Haz3lcore.Zipper.t) =>
  Haz3lcore.Parser.to_zipper(s);

/* Split string into lines */
let lines_of_string = (s: string): array(string) => {
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
    switch (Info.error_of(info)) {
    | None => None
    | Some(error) =>
      let id = Info.id_of(info);
      let error_str = Info.show_error(error);

      switch (Haz3lcore.Measured.find_by_id(id, measured)) {
      | Some({origin, last}) =>
        let lines = lines_of_string(source);
        let row = origin.row;
        let col = origin.col;
        let len =
          if (origin.row == last.row) {
            last.col - origin.col;
          } else {
            1;
          };

        let line_num = row + 1;
        let line_num_str = string_of_int(line_num);
        let line_num_width = String.length(line_num_str);
        let padding = String.make(line_num_width, ' ');

        let source_line =
          if (row >= 0 && row < Array.length(lines)) {
            lines[row];
          } else {
            "<source unavailable>";
          };

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

      | None => Some("error: " ++ error_str)
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
  switch (parse_to_zipper(program)) {
  | None =>
    prerr_endline("Failed to parse program");
    `Error((false, "Parse error"));
  | Some(zipper) =>
    open Language;
    open Util;
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper).term;

    let measured =
      Haz3lcore.Measured.of_segment(
        segment,
        Haz3lcore.ProjectorCore.Shape.Map.empty,
        Id.Map.empty,
      );

    let static_map =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);

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
        let line = lines[origin.row];
        let start_col = max(0, origin.col);
        let end_col = min(String.length(line), last.col);
        Some(String.sub(line, start_col, end_col - start_col));
      } else {
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

  if (!verbose && status == TestStatus.Pass) {
    None;
  } else {
    let status_str =
      switch (status) {
      | Pass => "PASS"
      | Fail => "FAIL"
      | Indet => "INDET"
      };

    let location =
      switch (Haz3lcore.Measured.find_by_id(id, measured)) {
      | Some({origin, _}) => "line " ++ string_of_int(origin.row + 1)
      | None => "unknown location"
      };

    let hint_str =
      switch (hint) {
      | Some(h) => ", \"" ++ h ++ "\""
      | None => ""
      };

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
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper).term;

    let measured =
      Haz3lcore.Measured.of_segment(
        segment,
        Haz3lcore.ProjectorCore.Shape.Map.empty,
        Id.Map.empty,
      );

    let (_, test_results) = Run.evaluate_with_tests(term);

    print_endline(
      "Test Results: " ++ TestResults.test_summary_str(test_results),
    );
    print_endline("");

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

    List.iter(line => print_endline(line), formatted_tests);

    if (test_results.failing > 0) {
      `Error((false, "Tests failed"));
    } else {
      `Ok();
    };
  };
};

/* Run program with probes and display results inline */
let probe_hazel = (many: bool, path: string): unit => {
  let program = read_input(path);
  switch (parse_to_zipper(program)) {
  | None => prerr_endline("Failed to parse program")
  | Some(zipper) =>
    /* Get the segment */
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);

    /* Get refractors (where probes are stored) */
    let refractors = zipper.refractors.manuals;

    /* Get term for evaluation */
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper).term;

    /* Evaluate and collect probe samples */
    let (_, probe_map) = Run.evaluate_with_probes(term);

    /* Format output with probe values */
    let window: Language.Sample.Window.mode =
      many ? Language.Sample.Window.Many : Language.Sample.Window.Single;
    let output =
      Haz3lcore.ProbeText.of_segment(
        ~window,
        ~probe_map,
        ~refractors,
        segment,
      );
    print_endline(output);
  };
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
    Reconstructs Hazel code from its abstract syntax tree (AST), producing
    concrete syntax without preserving original whitespace or comments.
    This process uses a recovering parser and automatically inserts holes
    where necessary to ensure syntactic correctness.
  |};

  let info = Cmd.info("format", ~doc);
  Cmd.v(info, Term.(const(format_hazel) $ input_arg));
};

let analyze_cmd = {
  let doc = "Perform static analysis on Hazel code.";
  let info = Cmd.info("analyze", ~doc);
  Cmd.v(info, Term.ret(Term.(const(analyze_hazel) $ input_arg)));
};

let probe_cmd = {
  let doc = "Run a Hazel program and display probe values inline.";
  let many_arg = {
    let doc = "Show multiple sample values per probe (many mode).";
    Arg.(value & flag & info(["many", "m"], ~doc));
  };
  let info = Cmd.info("probe", ~doc);
  Cmd.v(info, Term.(const(probe_hazel) $ many_arg $ input_arg));
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

/* Default to help if no subcommand is given */
let default_cmd = {
  let doc = "CLI tool for running and analyzing Hazel programs.";
  let info = Cmd.info("hazel", ~doc);
  Cmd.group(info, [run_cmd, format_cmd, analyze_cmd, probe_cmd, test_cmd]);
};

let () = exit(Cmd.eval(default_cmd));

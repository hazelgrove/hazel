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

/* Parse user program text without injecting extra stdlib terms. */
let parse_program_raw = (s: string) =>
  switch (Haz3lcore.Parser.to_term(s)) {
  | Some(e) => e
  | None => failwith("Failed to parse expression: " ++ s)
  };

/* Parse user program text and prepend Petri stdlib definitions semantically. */
let parse_program_with_petri_stdlib = (s: string) =>
  parse_program_raw(s) |> Haz3lcore.PetriStdlib.with_prelude_term;

let run_hazel = path => {
  let program = read_input(path);
  let parsed = parse_program_with_petri_stdlib(program);
  let evaluated = Run.evaluate(parsed);

  print_endline(Print.print(evaluated));
};

let format_hazel = path => {
  let program = read_input(path);
  let parsed = parse_program_raw(program);
  print_endline(Print.print(parsed));
};

let analyze_hazel =
    (path: string)
    : [>
        | `Error(bool, string)
        | `Ok(unit)
      ] => {
  let program = read_input(path);
  let parsed = parse_program_with_petri_stdlib(program);
  open Language;
  let static_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), parsed);
  let errors = List.map(snd, Statics.Map.errors(static_map));
  switch (errors) {
  | [] =>
    print_endline("No static errors found.");
    `Ok();
  | _ =>
    prerr_endline("Static errors:");
    List.iter(error => print_endline(Info.show_error(error)), errors);
    prerr_endline("");
    `Error((false, "Static errors found"));
  };
};

/* Parse program and return zipper (preserving projectors like probes) */
let parse_to_zipper = (s: string): option(Haz3lcore.Zipper.t) =>
  Haz3lcore.Parser.to_zipper(s);

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
    let term =
      Haz3lcore.MakeTerm.from_zip_for_sem(zipper).term
      |> Haz3lcore.PetriStdlib.with_prelude_term;

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

/* Run tests in a Hazel program and report results */
let test_hazel =
    (verbose: bool, path: string)
    : [>
        | `Error(bool, string)
        | `Ok(unit)
      ] => {
  let program = read_input(path);
  let parsed = parse_program_with_petri_stdlib(program);
  open Language;
  let (_, test_results) = Run.evaluate_with_tests(parsed);

  print_endline(
    "Test Results: " ++ TestResults.test_summary_str(test_results),
  );
  print_endline("");

  List.iter(
    ((id, reports)) => {
      let status = TestMap.joint_status(reports);
      let should_show = verbose || status != Pass;
      if (should_show) {
        let status_str =
          String.uppercase_ascii(TestStatus.to_string(status));
        let hint =
          switch (reports) {
          | [{hint, _}, ..._] when hint != "No hint available." => hint
          | _ => ""
          };
        let id_str = Util.Id.to_string(id);
        let hint_suffix = hint == "" ? "" : " \"" ++ hint ++ "\"";
        print_endline(status_str ++ " [" ++ id_str ++ "]" ++ hint_suffix);
      };
    },
    test_results.test_map,
  );

  if (test_results.failing > 0) {
    `Error((false, "Tests failed"));
  } else {
    `Ok();
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

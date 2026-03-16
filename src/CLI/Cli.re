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

let analyze_hazel =
    (path: string)
    : [>
        | `Error(bool, string)
        | `Ok(unit)
      ] => {
  let program = read_input(path);
  let parsed = parse_program(program);
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

/* Sanitize a slide name into a valid filename */
let sanitize_filename = (name: string): string => {
  name
  |> String.split_on_char(' ')
  |> String.concat("_")
  |> String.to_seq
  |> Seq.filter(c =>
       c >= 'a'
       && c <= 'z'
       || c >= 'A'
       && c <= 'Z'
       || c >= '0'
       && c <= '9'
       || c == '_'
       || c == '-'
     )
  |> String.of_seq;
};

let strip_projectors = (segment: Haz3lcore.Segment.t): Haz3lcore.Segment.t =>
  Haz3lcore.ZipperBase.MapPiece.of_segment(
    fun
    | Projector(pr) => [pr.syntax]
    | x => [x],
    segment,
  );

/* Export documentation slides as .hz files */
let export_hazel = (format, width, strip, filter, output_dir) => {
  // TODO figure out a way to reference init slides without depending on the entire Init module
  let all_slides =
    [
      BasicReference.out,
      Projectors.out,
      ADTs.out,
      Tuples.out,
      Modules.out,
      Tables.out,
      Polymorphism.out,
      Cards.out,
      Probes.out,
      Livelits.out,
    ]
    @ B2t2.Slides.all_slides;

  let slides =
    switch (filter) {
    | None => all_slides
    | Some(f) =>
      List.filter(
        ((name, _)) => {
          let name_lower = String.lowercase_ascii(name);
          let f_lower = String.lowercase_ascii(f);
          Core.String.is_substring(name_lower, ~substring=f_lower);
        },
        all_slides,
      )
    };

  if (List.length(slides) == 0) {
    prerr_endline("No slides matched the filter.");
    `Error((false, "No matching slides"));
  } else {
    /* Create output directory if it doesn't exist */
    if (!Sys.file_exists(output_dir)) {
      ignore(Sys.command("mkdir -p " ++ Filename.quote(output_dir)));
    };

    let count = ref(0);
    List.iter(
      ((name, persistent_segment: Haz3lcore.PersistentSegment.t)) => {
        let zipper = Haz3lcore.PersistentSegment.restore(persistent_segment);
        let segment =
          Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);
        let segment = strip ? strip_projectors(segment) : segment;
        let segment =
          format
            ? Haz3lcore.PrettySegment.prettify(~width, segment) : segment;
        let output =
          Haz3lcore.Printer.of_segment(
            ~holes="?",
            ~indent=" ",
            ~refractors=strip ? [] : zipper.refractors.manuals,
            segment,
          );
        let filename = sanitize_filename(name) ++ ".hz";
        let path = Filename.concat(output_dir, filename);
        Core.Out_channel.write_all(path, ~data=output ++ "\n");
        print_endline("Exported: " ++ filename);
        incr(count);
      },
      slides,
    );

    print_endline(
      "\nExported " ++ string_of_int(count^) ++ " slide(s) to " ++ output_dir,
    );
    `Ok();
  };
};

/* Extract slide name from an existing .ml slide file.
   Looks for the pattern: ( "slide name", on a line */
let extract_slide_name = (ml_path: string): option(string) =>
  if (Sys.file_exists(ml_path)) {
    let content = Core.In_channel.read_all(ml_path);
    let lines = String.split_on_char('\n', content);
    List.fold_left(
      (acc, line) =>
        switch (acc) {
        | Some(_) => acc
        | None =>
          let trimmed = String.trim(line);
          if (Core.String.is_prefix(trimmed, ~prefix="( \"")) {
            /* Extract between first " and next " */
            let after_quote =
              String.sub(trimmed, 3, String.length(trimmed) - 3);
            switch (String.index_opt(after_quote, '"')) {
            | Some(end_idx) => Some(String.sub(after_quote, 0, end_idx))
            | None => None
            };
          } else {
            None;
          };
        },
      None,
      lines,
    );
  } else {
    None;
  };

/* Import a .hz file into a .ml slide file */
let import_hazel =
    (name_opt: option(string), input_path: string, output_path: string)
    : [>
        | `Error(bool, string)
        | `Ok(unit)
      ] => {
  let program = read_input(input_path);

  /* Determine slide name */
  let name =
    switch (name_opt) {
    | Some(n) => n
    | None =>
      switch (extract_slide_name(output_path)) {
      | Some(n) => n
      | None =>
        /* Fall back to filename without extension */
        Filename.basename(input_path) |> Filename.remove_extension
      }
    };

  switch (parse_to_zipper(program)) {
  | None =>
    prerr_endline("Failed to parse: " ++ input_path);
    `Error((false, "Parse error"));
  | Some(zipper) =>
    let persisted = Haz3lcore.PersistentSegment.persist(zipper);

    /* Generate the .ml file content */
    let ml_content =
      "let out : string * Haz3lcore.PersistentSegment.t =\n"
      ++ "  ( \""
      ++ String.escaped(name)
      ++ "\",\n"
      ++ "    {\n"
      ++ "      segment =\n"
      ++ "        \""
      ++ String.escaped(persisted.segment)
      ++ "\";\n"
      ++ "      backup_text =\n"
      ++ "        \""
      ++ String.escaped(persisted.backup_text)
      ++ "\";\n"
      ++ "      refractors =\n"
      ++ "        \""
      ++ String.escaped(persisted.refractors)
      ++ "\";\n"
      ++ "    } )\n";

    Core.Out_channel.write_all(output_path, ~data=ml_content);
    print_endline("Imported: " ++ input_path ++ " -> " ++ output_path);
    print_endline("Slide name: " ++ name);
    `Ok();
  };
};

/* Check a Hazel program for parse incompleteness */
let parse_check_hazel =
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
    let segment =
      Haz3lcore.Zipper.unselect_and_zip(~erase_buffer=true, zipper);

    /* Check 1: Non-empty backpack (missing shards) */
    let missing_shards = Haz3lcore.Zipper.local_backpack(zipper);

    /* Check 2: Concave grout in segment (missing operands/operators) */
    let all_grout = Haz3lcore.Segment.holes(segment);
    let concave_grout =
      List.filter((g: Haz3lcore.Grout.t) => g.shape == Concave, all_grout);

    let has_missing = List.length(missing_shards) > 0;
    let has_concave = List.length(concave_grout) > 0;

    if (!has_missing && !has_concave) {
      print_endline("Parse complete.");
      `Ok();
    } else {
      if (has_missing) {
        prerr_endline(
          "Incomplete tiles (missing shards): "
          ++ string_of_int(List.length(missing_shards)),
        );
      };
      if (has_concave) {
        prerr_endline(
          "Concave grout found: "
          ++ string_of_int(List.length(concave_grout)),
        );
      };
      `Error((false, "Parse incomplete"));
    };
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

let export_cmd = {
  let doc = "Export documentation slides as .hz files.";
  let format_arg = {
    let doc = "Pretty-print exported code.";
    Arg.(value & flag & info(["format", "F"], ~doc));
  };
  let width_arg = {
    let doc = "Target line width for formatting (default: 60).";
    Arg.(value & opt(int, 60) & info(["w", "width"], ~doc));
  };
  let strip_arg = {
    let doc = "Remove projectors, keeping only their inner syntax.";
    Arg.(value & flag & info(["strip-projectors", "s"], ~doc));
  };
  let filter_arg = {
    let doc = "Only export slides whose name contains this string.";
    Arg.(value & opt(some(string), None) & info(["filter", "f"], ~doc));
  };
  let output_arg = {
    let doc = "Output directory for .hz files.";
    Arg.(
      required & pos(0, some(string), None) & info([], ~docv="DIR", ~doc)
    );
  };
  let info = Cmd.info("export", ~doc);
  Cmd.v(
    info,
    Term.ret(
      Term.(
        const(export_hazel)
        $ format_arg
        $ width_arg
        $ strip_arg
        $ filter_arg
        $ output_arg
      ),
    ),
  );
};

let parse_check_cmd = {
  let doc = "Check a Hazel program for parse incompleteness.";
  let info = Cmd.info("parse-check", ~doc);
  Cmd.v(info, Term.ret(Term.(const(parse_check_hazel) $ input_arg)));
};

let import_cmd = {
  let doc = "Import a .hz file into a .ml slide file.";
  let name_arg = {
    let doc = "Slide name. If omitted, extracted from existing .ml file or input filename.";
    Arg.(value & opt(some(string), None) & info(["name", "n"], ~doc));
  };
  let input_arg = {
    let doc = "Path to .hz input file.";
    Arg.(
      required & pos(0, some(string), None) & info([], ~docv="INPUT", ~doc)
    );
  };
  let output_arg = {
    let doc = "Path to .ml output file.";
    Arg.(
      required & pos(1, some(string), None) & info([], ~docv="OUTPUT", ~doc)
    );
  };
  let info = Cmd.info("import", ~doc);
  Cmd.v(
    info,
    Term.ret(Term.(const(import_hazel) $ name_arg $ input_arg $ output_arg)),
  );
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
      export_cmd,
      import_cmd,
      parse_check_cmd,
    ],
  );
};

let () = exit(Cmd.eval(default_cmd));

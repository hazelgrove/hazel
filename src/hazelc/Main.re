/*
   Entry point for the command line tool.

   To test easily, run `dune exec src/hazelc/main.exe -- <ARGS>`. Use `--help`.
 */
open Sexplib.Std;
open Cmdliner;
open LetOpen.Syntax;

[@deriving sexp]
type action =
  | DHExp
  | Hir
  | Anf
  | Grain
  | Wasm
  | Wat;

/* Temporary directory prefix. */
let prefix = "hazelc";

[@deriving sexp]
type error =
  | ParseError(string)
  | ElaborateError
  | GrainError;

let mk_opts = (action, _verbose, optimize, debug) => {
  let indet_analysis_level =
    switch (optimize) {
    | Some(l) when l >= 1 => IndetAnalysis.LocalAnalysis
    | None => IndetAnalysis.LocalAnalysis
    | Some(_) => IndetAnalysis.NoAnalysis
    };

  let opts: Compile.opts = {
    optimize: {
      indet_analysis: {
        level: indet_analysis_level,
      },
    },
    codegen: {
      print_final_expr: true,
    },
  };
  let grain_opts: Compile.grain_opts = {
    grain: None,
    includes: None,
    wat: Some(action == Wat),
    optimize,
    debug: Some(debug),
  };

  (opts, grain_opts);
};

let hazelc =
    (action, source_filenames, output_filename, verbose, optimize, debug) => {
  // Open the source file.
  let source_filename = List.hd(source_filenames);
  let&i source_file = open_in(source_filename);
  let source = Compile.Source(File(source_file));

  // Use a temporary file for Grain output during compilation.
  let grain_output = Filename.temp_file(prefix, "a.gr");

  // Initialize options.
  let (opts, grain_opts) = mk_opts(action, verbose, optimize, debug);

  // Use the given output filename, or use "a.{ext}" where {ext} depends on
  // output kind.
  let output_filename =
    Option.value(
      output_filename,
      ~default={
        let ext =
          switch (action) {
          | DHExp => "hz.dhexp"
          | Hir => "hz.ihexp"
          | Anf => "hz.anf"
          | Grain => "gr"
          | Wasm => "wasm"
          | Wat => "wat"
          };
        "a." ++ ext;
      },
    );

  // Helper to write to output file.
  let write_output = output => {
    let&o output_file = open_out(output_filename);
    Printf.fprintf(output_file, "%s", output);
  };
  let write_sexp_output = (x_to_sexp, x) =>
    x |> x_to_sexp |> Sexplib.Sexp.to_string_hum |> write_output;

  // Helper to convert from Compile.next_error.
  let convert_error = (err: Compile.next_error) =>
    switch (err) {
    | ParseError(err) => ParseError(err)
    | ElaborateError => ElaborateError
    };

  let res =
    switch (action) {
    | DHExp =>
      Compile.resume_until_elaborated(~opts, source)
      |> Result.map(write_sexp_output(DHExp.sexp_of_t))
      |> Result.map_error(convert_error)

    | Hir =>
      Compile.resume_until_transformed(~opts, source)
      |> Result.map(write_sexp_output(Hir.sexp_of_expr))
      |> Result.map_error(convert_error)

    | Anf =>
      Compile.resume_until_optimized(~opts, source)
      |> Result.map(write_sexp_output(Anf.sexp_of_prog))
      |> Result.map_error(convert_error)

    | Grain =>
      Compile.resume_until_printed(~opts, source)
      |> Result.map(write_output)
      |> Result.map_error(convert_error)

    | Wasm
    | Wat =>
      let g = Compile.resume_until_printed(~opts, source);
      switch (g) {
      | Ok(g) =>
        Compile.wasmize(~opts=grain_opts, grain_output, output_filename, g)
        |> Result.map_error(() => GrainError)
      | Error(err) => Error(convert_error(err))
      };
    };

  // Print error, if any.
  switch (res) {
  | Ok () => ()
  | Error(err) =>
    switch (err) {
    | ParseError(err) => print_endline(err)
    | ElaborateError => print_endline("elaboration error")
    | GrainError => ()
    }
  };
};

/* Source file argument. */
let source_filenames = {
  let doc = "Source code file.";
  let docv = "SOURCE";
  Arg.(non_empty & pos_all(non_dir_file, []) & info([], ~docv, ~doc));
};

/* Output file argument. */
let output_filename_flag = {
  let doc = "Output file.";
  Arg.(
    value
    & opt(some(string), None)
    & info(["o", "output"], ~docv="OUT", ~doc)
  );
};

/* Action flag. */
let action_flag = {
  let dhexp = {
    let doc = "Emit DHExp sexp.";
    (DHExp, Arg.info(["dhexp"], ~doc));
  };
  let ihexp = {
    let doc = "Emit Hir sexp.";
    (Hir, Arg.info(["hir"], ~doc));
  };
  let anf = {
    let doc = "Emit Anf sexp.";
    (Anf, Arg.info(["anf"], ~doc));
  };
  let grain = {
    let doc = "Emit Grain code.";
    (Grain, Arg.info(["grain"], ~doc));
  };
  let wasm = {
    let doc = "Emit WebAssembly.";
    (Wasm, Arg.info(["wasm"], ~doc));
  };
  let wat = {
    let doc = "Emit WebAssembly text.";
    (Wat, Arg.info(["wat"], ~doc));
  };
  Arg.(last & vflag_all([Wasm], [dhexp, ihexp, anf, grain, wasm, wat]));
};

/* Verbosity flag. */
let verbose_flag = {
  let doc = "Enable verbose output.";
  Arg.(value & flag & info(["v", "verbose"], ~doc));
};

/* Optimization level flag: any integer [0, 3) */
let optimize_flag = {
  let optimize_arg = {
    let parse = s =>
      try({
        let n = int_of_string(s);
        if (n >= 0 && n < 3) {
          Ok(n);
        } else {
          Error(`Msg("invalid optimization level"));
        };
      }) {
      | Failure(_) => Error(`Msg("unable to parse integer"))
      };
    let print = (ppf, p) => Format.fprintf(ppf, "%s", string_of_int(p));
    Arg.conv(~docv="LEVEL", (parse, print));
  };

  let doc = "Set optimization level";
  Arg.(
    value
    & opt(some(optimize_arg), None)
    & info(["O"], ~docv="LEVEL", ~doc)
  );
};

/* Debug flag. */
let debug_flag = {
  let doc = "Enable debug information.";
  Arg.(value & flag & info(["g", "debug"], ~doc));
};

let cmd = {
  let doc = "Hazel compiler.";
  let man = [
    `S(Manpage.s_bugs),
    `P("Report bugs at <https://github.com/hazelgrove/hazel>"),
  ];
  let info = Cmd.info("hazelc", ~version="%%VERSION%%", ~doc, ~man);
  Cmd.v(
    info,
    Term.(
      const(hazelc)
      $ action_flag
      $ source_filenames
      $ output_filename_flag
      $ verbose_flag
      $ optimize_flag
      $ debug_flag
    ),
  );
};

let main = () => exit(Cmd.eval(cmd));
let () = main();

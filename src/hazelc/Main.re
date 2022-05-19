open Sexplib.Std;

[@deriving sexp]
type action =
  | DHExp
  | Hir
  | Anf
  | Grain
  | Wasm
  | Wat;

let prefix = "hazelc";

[@deriving sexp]
type error =
  | ParseError(string)
  | ElaborateError
  | GrainError;

let error_from_next_error = (err: Compile.next_error) =>
  switch (err) {
  | ParseError(err) => ParseError(err)
  | ElaborateError => ElaborateError
  };

let hazelc = (action, sources, out, _verbose, optimize, debug) => {
  let out =
    switch (out) {
    | Some(out) => out
    | None =>
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
    };

  let source = List.hd(sources);
  let source_file = open_in(source);
  let opts: Compile.opts = {
    exp_only: false,
    grain: {
      grain: None,
      includes: None,
      wat: Some(action == Wat),
      optimize,
      debug: Some(debug),
    },
  };

  let grain_output = Filename.temp_file(prefix, "a.gr");

  let write_output = output => {
    let out_file = open_out(out);
    Printf.fprintf(out_file, "%s", output);
    close_out(out_file);
    Ok();
  };

  let res =
    switch (action) {
    | DHExp =>
      let res =
        Compile.resume_until_dhexp(
          ~opts,
          Source(SourceChannel(source_file)),
        );
      switch (res) {
      | Ok(d) =>
        let output = d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string_hum;
        write_output(output);
      | Error(err) => Error(error_from_next_error(err))
      };

    | Hir =>
      let res =
        Compile.resume_until_hir(~opts, Source(SourceChannel(source_file)));
      switch (res) {
      | Ok(d) =>
        let output = d |> Hir.sexp_of_expr |> Sexplib.Sexp.to_string_hum;
        write_output(output);
      | Error(err) => Error(error_from_next_error(err))
      };

    | Anf =>
      let res =
        Compile.resume_until_anf(~opts, Source(SourceChannel(source_file)));
      switch (res) {
      | Ok(d) =>
        let output = d |> Anf.sexp_of_prog |> Sexplib.Sexp.to_string_hum;
        write_output(output);
      | Error(err) => Error(error_from_next_error(err))
      };

    | Grain =>
      let res =
        Compile.resume_until_grain_text(
          ~opts,
          Source(SourceChannel(source_file)),
        );
      switch (res) {
      | Ok(output) => write_output(output)
      | Error(err) => Error(error_from_next_error(err))
      };

    | Wasm
    | Wat =>
      let g =
        Compile.resume_until_grain_text(
          ~opts,
          Source(SourceChannel(source_file)),
        );
      switch (g) {
      | Ok(g) =>
        Compile.wasmize_next(grain_output, out, g)
        |> Result.map_error(() => GrainError)
      | Error(err) => Error(error_from_next_error(err))
      };
    };

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

open Cmdliner;

let sources = {
  let doc = "Source code file.";
  let docv = "SOURCE";
  Arg.(non_empty & pos_all(non_dir_file, []) & info([], ~docv, ~doc));
};

let out = {
  let doc = "Output file.";
  Arg.(
    value
    & opt(some(string), None)
    & info(["o", "output"], ~docv="OUT", ~doc)
  );
};

let action = {
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

let verbose = {
  let doc = "Enable verbose output.";
  Arg.(value & flag & info(["v", "verbose"], ~doc));
};

let optimize = {
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

let debug = {
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
      const(hazelc) $ action $ sources $ out $ verbose $ optimize $ debug
    ),
  );
};

let main = () => exit(Cmd.eval(cmd));
let () = main();

[@deriving sexp]
type action =
  | Grain
  | Wasm
  | Wat;

let hazelc = (action, sources, out, _verbose, optimize, debug) => {
  let out =
    switch (out) {
    | Some(out) => out
    | None =>
      switch (action) {
      | Grain => "a.gr"
      | Wasm => "a.wasm"
      | Wat => "a.wat"
      }
    };

  let source = List.hd(sources);
  let source_file = open_in(source);
  let opts: Compile.opts = {
    exp_only: false,
    grain: {
      grain: None,
      includes: None,
      wat: Some(action == Wat),
      optimize: Some(optimize),
      debug: Some(debug),
    },
  };

  let res =
    switch (action) {
    | Grain =>
      let res =
        Compile.compile_grain(~opts, out, SourceChannel(source_file));
      switch (res) {
      | Ok(output) =>
        let out_file = open_out(out);
        Printf.fprintf(out_file, "%s", output);
        close_out(out_file);
        Ok();
      | Error(err) => Error(err)
      };
    | Wasm
    | Wat =>
      let res = Compile.compile(~opts, out, SourceChannel(source_file));
      switch (res) {
      | Ok(_) => Ok()
      | Error(err) => Error(err)
      };
    };

  switch (res) {
  | Ok () => ()
  | Error(err) =>
    switch (err) {
    | ParseError(err) => print_endline(err)
    | ElaborateError => print_endline("elaboration error")
    | GrainError => print_endline("grain error")
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
  Arg.(last & vflag_all([Wasm], [grain, wasm, wat]));
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
        if (n < 3) {
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
  Arg.(value & opt(optimize_arg, 0) & info(["O"], ~docv="LEVEL", ~doc));
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

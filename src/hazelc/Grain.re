open Sexplib.Std;
open SexpResult;

module Opts = {
  [@deriving sexp]
  type t = {
    grain: option(string),
    includes: option(list(string)),
    optimize: option(int),
    debug: option(bool),
    wat: option(bool),
  };

  let identity = cmd => cmd;

  let use_grain = opts =>
    Option.fold(~none="grain", ~some=v => v, opts.grain);
  let use_subcmd = (opts, subcmd) =>
    Printf.sprintf("%s %s", use_grain(opts), subcmd);

  let use_arg = (arg, cmd) => Printf.sprintf("%s %s", cmd, arg);
  let use_flag = (flag, arg, cmd) => cmd |> use_arg(flag) |> use_arg(arg);
  let use_flags = (opts_list, cmd) =>
    List.fold_left((cmd, use) => use(cmd), cmd, opts_list);

  let use_optimize = opts =>
    switch (opts.optimize) {
    | Some(level) => use_flag("-O", string_of_int(level))
    | None => identity
    };
  let use_debug = opts =>
    switch (opts.debug) {
    | Some(true) => use_flag("--debug", "")
    | _ => identity
    };
  let use_wat = opts =>
    switch (opts.wat) {
    | Some(true) => use_flag("--wat", "")
    | _ => identity
    };
};

module Compile = {
  [@deriving sexp]
  type opts = {
    file: string,
    output: option(string),
  };

  [@deriving sexp]
  type compile_result = result(unit, int);

  let use_file = copts => Opts.use_arg(copts.file);
  let use_output = copts =>
    switch (copts.output) {
    | Some(output) => Opts.use_flag("-o", output)
    | None => Opts.identity
    };

  let opts_list = (opts, copts) => [
    Opts.use_optimize(opts),
    Opts.use_debug(opts),
    Opts.use_wat(opts),
    use_file(copts),
    use_output(copts),
  ];

  let compile = (~opts, copts) => {
    let cmd =
      Opts.use_subcmd(opts, "compile")
      |> Opts.use_flags(opts_list(opts, copts));

    let code = Sys.command(cmd);
    switch (code) {
    | 0 => Ok()
    | _ => Error(code)
    };
  };
};

module Run = {
  [@deriving sexp]
  type opts = {wasm: string};

  [@deriving sexp]
  type run_result = result(unit, int);

  let opts_list = (_, ropts) => [Opts.use_arg(ropts.wasm)];

  let run = (~opts, ropts) => {
    let cmd =
      Opts.use_subcmd(opts, "compile")
      |> Opts.use_flags(opts_list(opts, ropts));

    let code = Sys.command(cmd);
    switch (code) {
    | 0 => Ok()
    | _ => Error(code)
    };
  };
};

[@deriving sexp]
type opts = Opts.t;

[@deriving sexp]
type compile_opts = Compile.opts;
[@deriving sexp]
type compile_result = Compile.compile_result;
let compile = Compile.compile;

[@deriving sexp]
type run_opts = Run.opts;
[@deriving sexp]
type run_result = Run.run_result;
let run = Run.run;

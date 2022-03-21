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
    opts.grain |> Option.fold(~none="grain", ~some=v => v);
  let use_subcmd = (opts, subcmd) =>
    Printf.sprintf("%s %s", use_grain(opts), subcmd);

  let use_arg = (arg, cmd) => Printf.sprintf("%s %s", cmd, arg);
  let use_flag = (flag, arg, cmd) => cmd |> use_arg(flag) |> use_arg(arg);
  let use_args = (args_list, cmd) =>
    List.fold_left((cmd, use) => use(cmd), cmd, args_list);
};

module Compile = {
  [@deriving sexp]
  type opts = {
    file: string,
    output: option(string),
  };

  [@deriving sexp]
  type compile_result = result(unit, int);

  let use_includes = (opts: Opts.t) =>
    switch (opts.includes) {
    | Some(includes) =>
      Opts.use_args(List.map(Opts.use_flag("-I"), includes))
    | None => Opts.identity
    };
  let use_optimize = (opts: Opts.t) =>
    switch (opts.optimize) {
    | Some(level) => Opts.use_flag("-O", string_of_int(level))
    | None => Opts.identity
    };
  let use_debug = (opts: Opts.t) =>
    switch (opts.debug) {
    | Some(true) => Opts.use_flag("--debug", "")
    | _ => Opts.identity
    };
  let use_wat = (opts: Opts.t) =>
    switch (opts.wat) {
    | Some(true) => Opts.use_flag("--wat", "")
    | _ => Opts.identity
    };

  let use_file = copts => Opts.use_arg(copts.file);
  let use_output = copts =>
    switch (copts.output) {
    | Some(output) => Opts.use_flag("-o", output)
    | None => Opts.identity
    };

  let args_list = (opts, copts) => [
    use_includes(opts),
    use_optimize(opts),
    use_debug(opts),
    use_wat(opts),
    use_file(copts),
    use_output(copts),
  ];

  let compile = (~opts, copts) => {
    let cmd =
      Opts.use_subcmd(opts, "compile")
      |> Opts.use_args(args_list(opts, copts));

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
  type run_result = result(string, int);

  let args_list = (_, ropts) => [Opts.use_arg(ropts.wasm)];

  let input_lines = channel => {
    let lines = ref([]);
    try(
      {
        while (true) {
          let line = input_line(channel);
          lines := [line, ...lines^];
        };
        ();
      }
    ) {
    | End_of_file => ()
    };
    List.rev(lines^);
  };

  let run = (~opts, ropts) => {
    let cmd =
      Opts.use_subcmd(opts, "compile")
      |> Opts.use_args(args_list(opts, ropts));

    let stdout = Unix.open_process_in(cmd);
    let stdout_lines = input_lines(stdout);

    let status = Unix.close_process_in(stdout);
    switch (status) {
    | WEXITED(0) => Ok(String.concat("\n", stdout_lines))
    | WEXITED(code)
    | WSIGNALED(code)
    | WSTOPPED(code) => Error(code)
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

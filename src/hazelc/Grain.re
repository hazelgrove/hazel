open Sexplib.Std;

[@deriving sexp]
type opts = {
  grain: option(string),
  includes: option(list(string)),
  optimize: option(int),
  debug: option(bool),
  wat: option(bool),
};

[@deriving sexp]
type inner_opts = {
  grain: string,
  includes: list(string),
  optimize: option(int),
  debug: bool,
  wat: bool,
};

[@deriving sexp]
type compile_opts = {
  file: string,
  output: option(string),
};

type compile_result = result(unit, int);

let build_inner_opts = (opts: opts) => {
  {
    grain: Option.fold(~none="grain", ~some=v => v, opts.grain),
    includes: Option.fold(~none=[], ~some=v => v, opts.includes),
    optimize: opts.optimize,
    debug: Option.fold(~none=false, ~some=v => v, opts.debug),
    wat: Option.fold(~none=false, ~some=v => v, opts.wat),
  };
};

let use_opt = (cmd, flag) => cmd ++ " " ++ flag;
let use_output = (cmd, _, copts) =>
  switch (copts.output) {
  | Some(output) => use_opt(cmd, "-o " ++ output)
  | None => cmd
  };
let use_optimize = (cmd, opts, _) =>
  switch (opts.optimize) {
  | Some(level) => use_opt(cmd, "-O " ++ string_of_int(level))
  | None => cmd
  };
let use_debug = (cmd, opts, _) =>
  if (opts.debug) {
    use_opt(cmd, "--debug");
  } else {
    cmd;
  };
let use_wat = (cmd, opts, _) =>
  if (opts.wat) {
    use_opt(cmd, "--wat");
  } else {
    cmd;
  };

let use_opts_list = [use_output, use_optimize, use_debug, use_wat];
let use_opts = (cmd, opts, copts) =>
  List.fold_left((cmd, use) => use(cmd, opts, copts), cmd, use_opts_list);

let build_cmd = (opts, copts) => {
  let iopts = build_inner_opts(opts);
  let cmd = iopts.grain ++ " compile " ++ copts.file;
  use_opts(cmd, iopts, copts);
};

let compile = (~opts, copts) => {
  let cmd = build_cmd(opts, copts);
  let code = Sys.command(cmd);

  switch (code) {
  | 0 => Ok()
  | _ => Error(code)
  };
};

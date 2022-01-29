open Sexplib.Std;

[@deriving sexp]
type opts = {grain: string};

[@deriving sexp]
type compile_opts = {
  file: string,
  includes: list(string),
  output: option(string),
  optimize: option(int),
  debug: bool,
  wat: bool,
};

type compile_result = result(unit, int);

let use_opt = (cmd, flag) => cmd ++ " " ++ flag;

let use_output = (cmd, co) =>
  switch (co.output) {
  | Some(output) => use_opt(cmd, "-o " ++ output)
  | None => cmd
  };

let use_optimize = (cmd, co) =>
  switch (co.optimize) {
  | Some(level) => use_opt(cmd, "-O " ++ string_of_int(level))
  | None => cmd
  };

let use_debug = (cmd, co) =>
  if (co.debug) {
    use_opt(cmd, "--debug");
  } else {
    cmd;
  };
let use_wat = (cmd, co) =>
  if (co.wat) {
    use_opt(cmd, "--wat");
  } else {
    cmd;
  };

let use_opts_list = [use_output, use_optimize, use_debug, use_wat];

let build_cmd = (o, co) => {
  let cmd = o.grain ++ " compile " ++ co.file;
  List.fold_left((cmd, use) => use(cmd, co), cmd, use_opts_list);
};

let compile = (o, co) => {
  let cmd = build_cmd(o, co);
  let code = Sys.command(cmd);

  switch (code) {
  | 0 => Ok()
  | _ => Error(code)
  };
};

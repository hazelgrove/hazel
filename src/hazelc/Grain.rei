[@deriving sexp]
type opts = {
  grain: option(string),
  includes: option(list(string)),
  optimize: option(int),
  debug: option(bool),
  wat: option(bool),
};

[@deriving sexp]
type compile_opts = {
  file: string,
  output: option(string),
};

type compile_result = result(unit, int);

let compile: (~opts: opts, compile_opts) => compile_result;

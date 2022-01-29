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

let compile: (opts, compile_opts) => compile_result;

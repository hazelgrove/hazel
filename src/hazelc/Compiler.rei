[@deriving sexp]
type grain_opts = Grain.opts;

[@deriving sexp]
type opts = {
  exp_only: bool,
  grain: grain_opts,
};

[@deriving sexp]
type err =
  | Parse(string)
  | Elab
  | Grain;

[@deriving sexp]
type compile_result = result(unit, err);

let compile_uhexp: (~opts: opts=?, UHExp.t, string) => compile_result;
let compile_dhexp: (~opts: opts=?, DHExp.t, string) => compile_result;
let compile_buf: (~opts: opts=?, Lexing.lexbuf, string) => compile_result;
let compile_string: (~opts: opts=?, string, string) => compile_result;
let compile_file: (~opts: opts=?, in_channel, string) => compile_result;

[@deriving sexp]
type grain_result = result(string, err);

let grain_compile_uhexp: (~opts: opts=?, UHExp.t) => grain_result;
let grain_compile_dhexp: (~opts: opts=?, DHExp.t) => grain_result;
let grain_compile_buf: (~opts: opts=?, Lexing.lexbuf) => grain_result;
let grain_compile_string: (~opts: opts=?, string) => grain_result;
let grain_compile_file: (~opts: opts=?, in_channel) => grain_result;

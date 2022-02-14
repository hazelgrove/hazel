[@deriving sexp]
type opts = {exp_only: bool};

[@deriving sexp]
type err =
  | Parse(string)
  | Elab;

[@deriving sexp]
type grain_result = result(string, err);

let grain_compile_uhexp: (~opts: opts=?, UHExp.t) => grain_result;
let grain_compile_dhexp: (~opts: opts=?, DHExp.t) => grain_result;
let grain_compile_buf: (~opts: opts=?, Lexing.lexbuf) => grain_result;
let grain_compile_string: (~opts: opts=?, string) => grain_result;
let grain_compile_file: (~opts: opts=?, in_channel) => grain_result;

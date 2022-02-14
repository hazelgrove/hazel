[@deriving sexp]
type opts = {exp_only: bool};

[@deriving sexp]
type err =
  | Parse(string)
  | Elab;

[@deriving sexp]
type compile_result = result(string, err);

let compile_uhexp: (~opts: opts=?, UHExp.t) => compile_result;
let compile_dhexp: (~opts: opts=?, DHExp.t) => compile_result;
let compile_buf: (~opts: opts=?, Lexing.lexbuf) => compile_result;
let compile_string: (~opts: opts=?, string) => compile_result;
let compile_file: (~opts: opts=?, in_channel) => compile_result;

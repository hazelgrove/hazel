type compile_opts = {expr_only: bool};

type compile_result = result(string, string);

let compile_uhexp: (~opts: compile_opts=?, UHExp.t) => compile_result;
let compile_dhexp: (~opts: compile_opts=?, DHExp.t) => compile_result;
let compile_buf: (~opts: compile_opts=?, Lexing.lexbuf) => compile_result;
let compile_string: (~opts: compile_opts=?, string) => compile_result;
let compile_file: (~opts: compile_opts=?, in_channel) => compile_result;

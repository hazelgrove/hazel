type compile_result = result(string, string);

let compile_buf: Lexing.lexbuf => compile_result;
let compile_string: string => compile_result;
let compile_file: in_channel => compile_result;
let compile_uhexp: UHExp.t => compile_result;
let compile_dhexp: DHExp.t => compile_result;

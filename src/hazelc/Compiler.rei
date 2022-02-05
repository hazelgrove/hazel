type compile_result = result(string, string);

let compile: Lexing.lexbuf => compile_result;
let compile_string: string => compile_result;
let compile_file: in_channel => compile_result;

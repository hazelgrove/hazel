
open Lexing;
let column_num = (pos : position) => {
  (pos.pos_cnum - pos.pos_bol) - 1
}


let string_of_pos = (pos : position) => {
  let l = string_of_int(pos.pos_lnum);
  let c = string_of_int(column_num(pos) + 1);
  "line " ++ l ++ ", column " ++ c;
}

let parse = (f, s) => {
  let lexbuf = Lexing.from_string(s);
  let result =
    try(f(Lexer.token, lexbuf)) {
    | Parser.Error => raise(Failure("Parse error at: " ++ string_of_pos(lexbuf.lex_curr_p)));
    };
  result;
};

let parse_program = s => parse(Parser.program, s);

let parse = (f, s) => {
  let lexbuf = Lexing.from_string(s);
  let result =
    try(f(Lexer.token, lexbuf)) {
    | Parser.Error => raise(Failure("Parse error"))
    };
  result;
};

let parse_program = s => parse(Parser.program, s);

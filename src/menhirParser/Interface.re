open Lexing;
let column_num = (pos: position) => {
  pos.pos_cnum - pos.pos_bol - 1;
};

let string_of_pos = (pos: position) => {
  let l = string_of_int(pos.pos_lnum);
  let c = string_of_int(column_num(pos) + 1);
  "line " ++ l ++ ", column " ++ c;
};

/* where a parse stopped: the token it couldn't take, its line and first
   column (1-based) */
type stop = {
  line: int,
  column: int,
};

let parse_or_stop = (f, s) => {
  Lexer.reset_delims();
  let lexbuf = Lexing.from_string(s);
  switch (f(Lexer.token, lexbuf)) {
  | ast => Ok(ast)
  | exception Parser.Error =>
    let start = lexbuf.lex_start_p;
    Error((
      {
        line: start.pos_lnum,
        column: column_num(start) + 2,
      },
      lexbuf.lex_curr_p,
    ));
  };
};

let parse = (f, s) =>
  switch (parse_or_stop(f, s)) {
  | Ok(ast) => ast
  | Error((_, curr)) =>
    raise(
      Failure(
        "Exception "
        ++ Printexc.to_string(Parser.Error)
        ++ " at: "
        ++ string_of_pos(curr),
      ),
    )
  };

let parse_program = s => parse(Parser.program, s);
let program_or_stop = s =>
  Result.map_error(fst, parse_or_stop(Parser.program, s));

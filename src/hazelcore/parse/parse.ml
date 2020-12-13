open Lexing

exception SyntaxError of ((int * int) option * string)

module I = Hazel_parser.MenhirInterpreter

let rec parse lexbuf c =
  match c with
  | I.InputNeeded _ ->
      let token = Hazel_lexer.read lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer c (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume c in parse lexbuf checkpoint
  | I.HandlingError _ ->
      let startp = lexbuf.lex_start_p in
      let line = startp.pos_lnum in
      let col = startp.pos_cnum - startp.pos_bol + 1 in
      let test = Lexing.lexeme lexbuf in
      let _ = print_endline test in
      raise (SyntaxError (Some (line, col), test))
  | I.Accepted v -> v
  | I.Rejected -> raise (SyntaxError (None, "Rejected"))

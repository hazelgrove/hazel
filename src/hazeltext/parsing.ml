open Lexing

(* Intended to store the line, column and character of error *)
exception SyntaxError of ((int * int) option * string option)

module I = Parse.MenhirInterpreter

let rec parse lexbuf c =
  match c with
  | I.InputNeeded _ ->
      let token = Lex.read lexbuf in
      let startp = lexbuf.lex_start_p in
      let endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer c (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume c in
      parse lexbuf checkpoint
  | I.HandlingError _ ->
      let startp = lexbuf.lex_start_p in
      let line = startp.pos_lnum in
      let col = startp.pos_cnum - startp.pos_bol + 1 in
      let lexeme =
        match startp.pos_cnum < lexbuf.lex_buffer_len with
        | true ->
            let c = Bytes.get lexbuf.lex_buffer startp.pos_cnum in
            Some (String.make 1 c)
        | false -> None
      in
      raise (SyntaxError (Some (line, col), lexeme))
  | I.Accepted v -> v
  | I.Rejected -> raise (SyntaxError (None, Some "Rejected"))

let ast_of_lexbuf l =
  try
    Ok
      ( parse l (Parse.Incremental.main l.lex_curr_p)
      |> Statics_Exp.fix_and_renumber_holes Contexts.initial
      |> fun (ast, _, _) -> ast )
  with
  | SyntaxError (Some (line, col), tok) ->
      let tok_string =
        match tok with
        | Some c -> Printf.sprintf "Token: %s" c
        | None -> Printf.sprintf "End of File"
      in
      Error
        (Printf.sprintf "ERROR on line %d, column %d. %s" line col tok_string)
  | SyntaxError (None, _) -> Error "Unknown Error"

let ast_of_string str = Lexing.from_string str |> ast_of_lexbuf

open Lexing

(* Intended to store the line, column and character of error *)
exception SyntaxError of ((int * int) option * string option)

module I = Parse.MenhirInterpreter

(*
let old_pos : int ref = ref 0
let cur_ws : int ref = ref 0
let get_whitespace () = !cur_ws

let set_whitespace l =
  let count = lexeme_start l - !old_pos in
  old_pos := lexeme_end l;
  cur_ws := count

let print_whitespace l =
  set_whitespace l;
  let count = get_whitespace () in
  print_endline ("considering: " ^ lexeme l);
  print_endline ("ws should be: " ^ Int.to_string count)
  *)

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
  try Ok (parse l (Parse.Incremental.main l.lex_curr_p)) with
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

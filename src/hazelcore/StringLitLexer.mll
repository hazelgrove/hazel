{
open Sexplib.Std

type error =
  | InvalidEscape of {
    start: int;
    length: int;
  } [@@deriving sexp]

let buffer : Buffer.t = Buffer.create 256
let errors: error Queue.t = Queue.create ()

let add_char = Buffer.add_char buffer
let add_string = Buffer.add_string buffer

let invalid_escape lexbuf = 
  let start = Lexing.lexeme_start lexbuf in
  let length = Lexing.lexeme_end lexbuf - start in
  Queue.add (InvalidEscape {
    start = Lexing.lexeme_start lexbuf;
    length = length
  }) errors

let escapechar c =
  match c with
  | '\\' | '\'' | '\"' | ' ' -> c
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | _ -> assert false
}

rule stringlit_body = parse
  | '\\' (['\\' '\'' '\"' 'n' 't' 'r' ' '] as c)
    {
      add_char (escapechar c);
      stringlit_body lexbuf
    }
  | '\\' _
    {
      add_char '\\';
      add_string (Lexing.lexeme lexbuf);
      invalid_escape lexbuf;
      stringlit_body lexbuf
    }
  | eof
    {
      let errors_list = Queue.fold (fun a x -> x::a) [] errors in
      let r = (Buffer.contents buffer, errors_list) in
      Buffer.clear buffer;
      Queue.clear errors;
      r
    }
  | (_ as c)
    {
      add_char c;
      stringlit_body lexbuf
    }

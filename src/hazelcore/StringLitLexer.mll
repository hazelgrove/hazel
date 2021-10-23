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

let charcode_0 = Char.code '0'
let int_of_char c = 
  Char.code c - charcode_0

let charcode_a = Char.code 'a'
let charcode_A = Char.code 'A'
let int_of_hex_char c = 
  match c with 
  | '0' .. '9' -> int_of_char c
  | 'a' .. 'f' -> 10 + Char.code c - charcode_a
  | 'A' .. 'F' -> 10 + Char.code c - charcode_A
  | _ -> assert false

let int_of_decimal a b c = 
  a * 100 + b * 10 + c

let int_of_decimal_escape a b c = 
  int_of_decimal (int_of_char a) (int_of_char b) (int_of_char c)

let int_of_octal a b c = 
  a * 64 + b * 8 + c

let int_of_octal_escape a b c = 
  int_of_octal (int_of_char a) (int_of_char b) (int_of_char c)

let int_of_hex a b = 
  a * 16 + b

let int_of_hex_escape a b = 
  int_of_hex (int_of_hex_char a) (int_of_hex_char b)

let add_decimal_code lexbuf i = 
  begin if (i > 0 && i < 256) then 
    add_char (Char.chr i)
  else 
    add_char '\\';
    add_string (Lexing.lexeme lexbuf);
    invalid_escape lexbuf
  end

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
  | '\\' (['0'-'9'] as a) (['0'-'9'] as b) (['0'-'9'] as c)
    {
      let i = int_of_decimal_escape a b c in 
      add_decimal_code lexbuf i;
      stringlit_body lexbuf
    }
  | '\\' 'o' (['0'-'7'] as a) (['0'-'7'] as b) (['0'-'7'] as c)
    {
      let i = int_of_octal_escape a b c in 
      add_decimal_code lexbuf i;
      stringlit_body lexbuf
    }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as a) (['0'-'9' 'a'-'f' 'A'-'F'] as b)
    {
      let i = int_of_hex_escape a b in 
      add_decimal_code lexbuf i;
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

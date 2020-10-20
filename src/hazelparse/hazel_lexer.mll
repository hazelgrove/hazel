{
open Hazel_parser

let keyword_table = Hashtbl.create 4
let _ =
  List.iter
    (fun (keyword, token) -> Hashtbl.add keyword_table keyword token)
    [ "let", LET;
      "in", IN ]
}

let white = [' ']+
let digit = ['0'-'9']
let numlit = digit+
let lowercase_ident = ['a'-'z' '_']+

rule read =
  parse
  | white { read lexbuf }
  | lowercase_ident as id {
    try
      Hashtbl.find keyword_table id
    with Not_found ->
      IDENT id
  }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "/" { DIV }
  | "=" { EQUAL }
  | ":" { COLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "\\" { LAMBDA }
  | numlit { INT (Lexing.lexeme lexbuf) }
  | eof { EOF }

{
open Hazel_parser

let keyword_table = Hashtbl.create 8
let _ =
  List.iter
    (fun (keyword, token) -> Hashtbl.add keyword_table keyword token)
    [ "let", LET;
      "in", IN;
      "case", CASE;
      "end", END ]
}

let white = [' ']+
let newline = ['\n']
let digit = ['0'-'9']
let numlit = digit+
let lowercase_ident = ['a'-'z' '_']+

rule read =
  parse
  newline {
    let curr_p = lexbuf.lex_curr_p in
    let count = curr_p.pos_cnum - curr_p.pos_bol in
    Lexing.new_line lexbuf;
    (* FIXME: This is to handle empty lines,
     * but does not work with whitespace in the line *)
    if count = 1 then
      EMPTY
    else
      read lexbuf
  }
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
  | "." { PERIOD }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "\\" { LAMBDA }
  | "|" { BAR }
  | "=>" { ARROW }
  | "#" white* ( [^'\n']* as t) { COMMENT t }
  | numlit { INT (Lexing.lexeme lexbuf) }
  | eof { EOF }

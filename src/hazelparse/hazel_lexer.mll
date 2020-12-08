{
open Hazel_parser

let keyword_table = Hashtbl.create 12
let _ =
  List.iter
    (fun (keyword, token) -> Hashtbl.add keyword_table keyword token)
    [ "let", LET;
      "in", IN;
      "case", CASE;
      "end", END;
      "true", TRUE;
      "false", FALSE]
}

let white = [' ']
let newline = ['\n']
let digit = ['0'-'9']
let numlit = digit+
let floatlit = digit* '.' digit+ | digit+ '.' digit*
let alpha = ['A'-'Z' 'a'-'z' '_']
let ident = ['A'-'Z' 'a'-'z' '_']+ (alpha | digit)*

rule read =
  parse
  newline {
    let curr_p = lexbuf.lex_curr_p in
    let count = curr_p.pos_cnum - curr_p.pos_bol in
    Lexing.new_line lexbuf;
    if count = 1 then
      EMPTY
    else
      read lexbuf
  }
  | white* newline {
    EMPTY
  }
  | white+ { read lexbuf }
  | ident as id {
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
  | ">" { GREATER }
  | "<" { LESSER }
  | "." { PERIOD }
  | "," { COMMA }
  | ":" { COLON }
  | "::" { COLONCOLON }
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "\\" { LAMBDA }
  | "|" { BAR }
  | "=>" { ARROW }
  | "->" { TARROW }
  | "inj[L]" { INJL }
  | "inj[R]" { INJR }
  | "#" white* ( [^'\n']* as t) { COMMENT t }
  | numlit { INT (Lexing.lexeme lexbuf) }
  | floatlit { FLOAT (Lexing.lexeme lexbuf) }
  | eof { EOF }

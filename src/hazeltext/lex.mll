{
open Parse

let keyword_table = Hashtbl.create 12
let _ =
  List.iter
    (fun (keyword, token) -> Hashtbl.add keyword_table keyword token)
    [ "let", LET;
      "in", IN;
      "case", CASE;
      "end", END;
      "fun", FUN;
      "true", TRUE;
      "false", FALSE;
      "NaN", FLOAT ("NaN");
      "Inf", FLOAT "Inf";
      "NegInf", FLOAT "NegInf"]
}

(*
Definitions base on OCaml definitions
https://ocaml.org/manual/lex.html
*)
let decimal = ['0'-'9'] ['0'-'9' '_']*
let hex = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct = '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin = '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_lit = decimal | hex | oct | bin
let float =
  ['0'-'9'] ['0'-'9' '_']* ('.' ['0'-'9' '_']*)?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_'])?
let float_hex =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']*)?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
let float_lit = float | float_hex

let whitespace = [' ' '\160' '\194']
let newline = ('\r'* '\n')
let wild = ['_']
let ident = ['_' 'a'-'z' 'A'-'Z' '0'-'9' '\'']+

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
  | whitespace+ { read lexbuf }
  | int_lit { INT (Lexing.lexeme lexbuf) }
  | float_lit { FLOAT (Lexing.lexeme lexbuf) }
  | wild { WILD }
  | ident as id {
    try
      Hashtbl.find keyword_table id
    with Not_found ->
      IDENT id
  }
  | "+" { PLUS }
  | "+." { FPLUS }
  | "-" { MINUS }
  | "-." { FMINUS }
  | "*" { MULT }
  | "*." { FMULT }
  | "/" { DIV }
  | "/." { FDIV }
  | "=" { EQUAL }
  | "==" { EQUALEQUAL }
  | "==." { FEQUALEQUAL }
  | ">" { GREATER }
  | ">." { FGREATER }
  | "<" { LESSER }
  | "<." { FLESSER }
  | "&&" { AND }
  | "||" { OR }
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
  | "?" { EMPTY_HOLE }
  | "|" { BAR }
  | "=>" { ARROW }
  | "->" { TARROW }
  | "→" { TARROW }
  | "inj[L]" { INJL }
  | "inj[R]" { INJR }
  | "#" whitespace* ( [^'\n']* as t) { COMMENT t }
  | eof { EOF }

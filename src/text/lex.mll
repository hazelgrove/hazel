{
open Parse

let keyword_table = Hashtbl.create 12
let _ =
  List.iter
    (fun (keyword, token) -> Hashtbl.add keyword_table keyword token)
    [ "let", LET;
      "type", TYPE;
      "in", IN;
      "triv", TRIV;
      "test", TEST;
      "case", CASE;
      "of", OF;
      "end", END;
      "if", IF;
      "then", THEN;
      "else", ELSE;
      "nil", NIL;
      "fun", FUN;
      "true", TRUE;
      "false", FALSE;
      "NaN", FLOAT ("NaN");
      "Inf", FLOAT "Inf";
      "NegInf", FLOAT "NegInf"]
}

(*
Definitions based on OCaml definitions
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
  newline { read lexbuf }
  | whitespace+ { read lexbuf }
  | int_lit { INT (int_of_string (Lexing.lexeme lexbuf)) }
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
  | ">=" { GREATEREQUAL }
  | ">." { FGREATER }
  | ">=." { FGREATEREQUAL }
  | "<" { LESSER }
  | "<=" { LESSEREQUAL }
  | "<." { FLESSER }
  | "<=." { FLESSEREQUAL }
  | "&&" { AND }
  | "||" { OR }
  | "," { COMMA }
  | ":" { COLON }
  | "::" { COLONCOLON }
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "?" { EMPTY_HOLE }
  | "|" { BAR }
  | "=>" { ARROW }
  | "->" { TARROW }
  | "→" { TARROW }
  | eof { EOF }

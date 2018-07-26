{
  open HazelParse
}

let space = [' ' '\t' '\r' '\n']
let whitespace = space+
let digit = ['0'-'9']
let natural = digit+
(* should be equivalent to the OCaml rules: "[_a-z][_a-zA-Z0-9']*" *)
let id = ['_' 'a'-'z'] (['_' 'a'-'z' 'A'-'Z' '''] | digit)*

rule read = 
  parse
  | whitespace { read lexbuf }
  | "num" { NUM_TYPE }
  | "let" { LET }
  | "in" { IN }
  | "inj" { INJECT }
  | "lambda" { LAMBDA }
  | "case" { CASE }
  | "L" { LEFT }
  | "R" { RIGHT }
  | ":" { COLON }
  | "=" { EQUAL }
  | "." { DOT }
  | "+" { PLUS }
  | "*" { TIMES }
  | "|" { BAR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LCBRACE }
  | "}" { RCBRACE }
  | "=>" { CASE_ARROW }
  | "->" { TYPE_ARROW }
  | natural { NATURAL (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ {raise (LangUtil.InvalidSyntax ("Unexpected char: " ^ Lexing.lexeme lexbuf))}

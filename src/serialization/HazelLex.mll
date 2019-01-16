{
  open HazelParse
}

let space = [' ' '\t' '\r' '\n']
let whitespace = space+
let digit = ['0'-'9']
let natural = digit+
(* should be equivalent to the OCaml rules: "[_a-z][_a-zA-Z0-9']*" *)
let id = ['_' 'a'-'z'] (['_' 'a'-'z' 'A'-'Z' '''] | digit)*
let palette_name = '$' id 
let palette_model = '"' ([^ '\\' '"'] | '\\' '"' | '\\' '\\')* '"' 

rule read = 
  parse
  | whitespace { read lexbuf }
  | "num" { NUM_TYPE }
  | "bool" { BOOL_TYPE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "_" { WILDCARD }
  | "let" { LET }
  | "in" { IN }
  | "inj" { INJECT }
  | "lambda" { LAMBDA }
  | "case" { CASE }
  | "L" { LEFT }
  | "R" { RIGHT }
  | ":" { COLON }
  | "::" { DOUBLE_COLON }
  | ";" { SEMICOLON }
  | "=" { EQUAL }
  | "," { COMMA }
  | "." { DOT }
  | "+" { PLUS }
  | "*" { TIMES }
  | "|" { BAR }
  | "&" { AMP }
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
  | palette_name { PALETTE_NAME (Lexing.lexeme lexbuf) }
  | palette_model { PALETTE_MODEL (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ {raise (LangUtil.InvalidSyntax ("Unexpected char: " ^ Lexing.lexeme lexbuf))}


{
open Lexing
open Parser

let advance_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  let pos' = { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  } in
  lexbuf.lex_curr_p <- pos'
}


let float = '-'? ['0'-'9']* '.' ['0'-'9']*
let int = '-'? ['0'-'9'] ['0'-'9']*

let string = '"' ([^ '"' '\\'] | '\\' ['"' '\\'])* '"'

let newline = '\r' | '\n' | "\r\n"

let whitespace = [' ' '\t']+

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = 
    parse 
    | whitespace {token lexbuf }
    | newline { advance_line lexbuf; token lexbuf}
    | int as i { INT (int_of_string i) }
    | float as f { FLOAT (float_of_string f )}
    | string as s { STRING (String.sub s 1 (String.length s - 2)) }
    | "true" { TRUE }
    | "false" { FALSE }
    | "let" { LET }
    | "in" { IN }
    | "fun" { FUN }
    | "case" { CASE }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "{" { OPEN_BRACKET }
    | "}" { CLOSE_BRACKET }
    | "[" { OPEN_SQUARE_BRACKET }
    | "]" { CLOSE_SQUARE_BRACKET }
    | "(" { OPEN_PAREN }
    | ")" { CLOSE_PAREN }
    | "->" { DASH_ARROW }
    | "=>" { EQUAL_ARROW }
    | "=" { SINGLE_EQUAL }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "/" { DIVIDE }
    | "**" {POWER}
    | "==" { DOUBLE_EQUAL }
    | "!=" { NOT_EQUAL }
    | "<" { LESS_THAN}
    | "<=" { LESS_THAN_EQUAL }
    | ">" { GREATER_THAN }
    | ">=" { GREATER_THAN_EQUAL }
    | "&&" { L_AND }
    | "||" { L_OR }
    | "!" { L_NOT }
    | "&" { B_AND }
    | "|" { TURNSTILE }
    | "," { COMMA }
    | ":" { COLON }
    | "Int" { INT_TYPE }
    | "Float" { FLOAT_TYPE }
    | "Bool" { BOOL_TYPE }
    | "String" { STRING_TYPE }
    | "()" { UNIT }
    | identifier as i { IDENT(i) }
    | eof { EOF }
    | _ { raise (Failure ("Lex error: unknown char: '" ^ Lexing.lexeme lexbuf ^ "'")) }

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

let identifier = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let constructor_ident = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* let builtins = ["infinity", "neg_infinity", "nan", "epsilon_float", "pi", "max_int", "min_int", "is_finite", "is_infinite", "int_of_float", "float_of_int", "string_of_int", "string_of_float", "string_of_bool", "int_of_string", "float_of_string", "bool_of_string", "abs", "abs_float", "ceil", "floor", "exp", "log", "log10", "sqrt", "sin", "cos", "tan", "asin", "acos", "atan", "mod", "string_length", "string_compare", "string_trim", "string_concat", "string_sub"] *)

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
    | "end" { END }
    | "fun" { FUN }
    | "case" { CASE }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "as" { AS }
    | "{" { OPEN_BRACKET }
    | "}" { CLOSE_BRACKET }
    | "[" { OPEN_SQUARE_BRACKET }
    | "]" { CLOSE_SQUARE_BRACKET }
    | "(" { OPEN_PAREN }
    | ")" { CLOSE_PAREN }
    | "->" { DASH_ARROW }
    | "=>" { EQUAL_ARROW }
    | "=" { SINGLE_EQUAL }
    (* Int ops*)
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
    (* Float ops *)
    | "+." { PLUS_FLOAT }
    | "-." { MINUS_FLOAT }
    | "*." { TIMES_FLOAT }
    | "/." { DIVIDE_FLOAT }
    | "**." {POWER_FLOAT}
    | "==." { DOUBLE_EQUAL_FLOAT }
    | "!=." { NOT_EQUAL_FLOAT }
    | "<." { LESS_THAN_FLOAT}
    | "<=." { LESS_THAN_EQUAL_FLOAT }
    | ">." { GREATER_THAN_FLOAT }
    | ">=." { GREATER_THAN_EQUAL_FLOAT }
    (* Bool ops *)
    | "&&" { L_AND }
    | "||" { L_OR }
    | "!" { L_NOT }
    | "&" { B_AND }
    | "|" { TURNSTILE }
    | "," { COMMA }
    | ":" { COLON }
    (* Types *)
    | "Int" { INT_TYPE }
    | "Float" { FLOAT_TYPE }
    | "Bool" { BOOL_TYPE }
    | "String" { STRING_TYPE }
    | "Unknown" { UNKNOWN }
    | "Internal" { INTERNAL }
    (* DHExp Annotations *)
    | "()" { UNIT }
    (* Filters *)
    | "pause" {PAUSE}
    | "debug" {DEBUG}
    | "hide" {HIDE}
    | "eval" {EVAL}
    (* Other *)
    | ";" {SEMI_COLON}
    | "test" {TEST}
    | "::" { CONS }
    | "@" {AT_SYMBOL}
    | "?" {QUESTION}
    | "_" {WILD}
    | "_BAD" {BAD_CONSTRUCTOR}
    | "fix" {FIX}
    | "_InAp" {IN_AP}
    | "_OutAp" {OUT_AP}
    | "typfun" {TYP_FUN}
    | "type" {TYP}
    | "$" {DOLLAR_SIGN}
    (* | builtins as b {BUILIN(b)} *)
    | identifier as i { IDENT(i) }
    | constructor_ident as i { CONSTRUCTOR_IDENT(i)}
    | eof { EOF }
    | _ { raise (Failure ("Lex error: unknown char: '" ^ Lexing.lexeme lexbuf ^ "'")) }

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

let parse_float_string s = 
  try
    let f = float_of_string s in
    f
  with
    | Failure _ -> print_endline ("Parse Float String Lexing Error On: " ^ s); 0.0

}
(* TODO We don't yet support negative floats in MakeTerm *)
(* Require leading 0 to avoid clashing with dot *)
let float = ['0'-'9']+ '.' ['0'-'9']*
(* negative ints are done through unop *)
let int = ['0'-'9'] ['0'-'9']*

let string = '"' ([^ '"' '\\'] | '\\' ['"' '\\'])* '"'
let quoted_label = '`' ([^ '`' '\\'] | '\\' [''' '\\'])* '`'

let newline = '\r' | '\n' | "\r\n"

let whitespace = [' ' '\t']+

let identifier = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let constructor_ident = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let sexp_string = '`' [^'`']* '`'
let ints = ['0'-'9']+
let projector_invoke = "^^" ['a'-'z' 'A'-'Z' '0'-'9' '_']+

rule token = 
    parse 
    | "undef" { UNDEF}
    | whitespace {token lexbuf }
    | newline { advance_line lexbuf; token lexbuf}
    | ints as i { INT (int_of_string i) }
    | float as f { FLOAT (parse_float_string f )}
    | string as s { STRING (String.sub s 1 (String.length s - 2)) }
    | quoted_label as l { QUOTED_LABEL (String.sub l 1 (String.length l - 2)) }
    | projector_invoke as p { PROJECTOR_INVOKE p }
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
    | "[" { OPEN_SQUARE_BRACKET }
    | "]" { CLOSE_SQUARE_BRACKET }
    | "(" { OPEN_PAREN }
    | ")" { CLOSE_PAREN }
    | "{{{" { OPEN_TRIPLE_CURLY }
    | "}}}" { CLOSE_TRIPLE_CURLY }
    | "->" { DASH_ARROW }
    | "=>" { EQUAL_ARROW }
    | "=" { SINGLE_EQUAL }
    | "..." { TUPLE_EXTENSION }
    | "." { DOT }
    (* Poly ops*)
    | "==" { DOUBLE_EQUAL }
    | "!=" { NOT_EQUAL }
    (* Int ops*)
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "/" { DIVIDE }
    | "**" {POWER}
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
    | "<." { LESS_THAN_FLOAT}
    | "<=." { LESS_THAN_EQUAL_FLOAT }
    | ">." { GREATER_THAN_FLOAT }
    | ">=." { GREATER_THAN_EQUAL_FLOAT }
    | "==." { DOUBLE_EQUAL_FLOAT }
    | "!=." { NOT_EQUAL_FLOAT }
    (* String Ops *)
    | "++" { STRING_CONCAT }
    | "$==" { STRING_EQUAL }
    (* Bool ops *)
    | "&&" { L_AND }
    | "||" { L_OR }
    | "!" { L_NOT }
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
    | "@<" {TYP_AP_SYMBOL}
    | "@" {AT_SYMBOL}
    | "?" {QUESTION}
    | "_" {WILD}
    | "fix" {FIX}
    | "typfun" {TYP_FUN}
    | "type" {TYP}
    | "$" {DOLLAR_SIGN}
    | "~" {TILDE}
    | "/~" {SLASH_TILDE}
    | "?t" {T_TYP}
    | "?p" {P_PAT}
    | "?tp" {TP_TPAT}
    | "?e" {E_EXP}
    | "named_fun" {NAMED_FUN}
    | "forall" {FORALL}
    | "rec" {REC}
    | identifier as i { IDENT(i) }
    | constructor_ident as i { CONSTRUCTOR_IDENT(i)}
    | eof { EOF }
    | _ { raise (Failure ("Lex error: unknown char: '" ^ Lexing.lexeme lexbuf ^ "'")) }

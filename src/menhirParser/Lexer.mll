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

let named_token fallback name =
  if name = Language.DerivativeOperator.expression_surface_prefix then
    EXPRESSION_DERIVATIVE
  else if name = Language.DerivativeOperator.expression_surface_separator then
    DERIVATIVE_BY
  else
    fallback name

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let rec next_non_whitespace lexbuf position =
  if position >= lexbuf.lex_buffer_len then None
  else
    let ch = Bytes.get lexbuf.lex_buffer position in
    if is_whitespace ch then next_non_whitespace lexbuf (position + 1)
    else Some (position, ch)

let following_word lexbuf start =
  let rec finish position =
    if position >= lexbuf.lex_buffer_len then position
    else
      match Bytes.get lexbuf.lex_buffer position with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> finish (position + 1)
      | _ -> position
  in
  let stop = finish start in
  Bytes.sub_string lexbuf.lex_buffer start (stop - start)

let function_operand_follows lexbuf =
  let start = lexbuf.lex_curr_pos in
  start < lexbuf.lex_buffer_len
  && is_whitespace (Bytes.get lexbuf.lex_buffer start)
  &&
  match next_non_whitespace lexbuf start with
  | None -> false
  | Some (position, ('a' .. 'z' | 'A' .. 'Z' | '_')) ->
      not
        (List.mem
           (following_word lexbuf position)
           ["in"; "then"; "else"; "end"; "by"])
  | Some (_, ('0' .. '9' | '"' | '(' | '[' | '?' | '{' | '!')) -> true
  | Some _ -> false

let constructor_token lexbuf name =
  if name = Language.DerivativeOperator.function_surface
     && function_operand_follows lexbuf then
    FUNCTION_DERIVATIVE
  else
    CONSTRUCTOR_IDENT name

}
(* TODO We don't yet support negative floats in MakeTerm *)
(* Require leading digits before dot *)
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
    | float as f { FLOAT f }
    | string as s { STRING (String.sub s 1 (String.length s - 2)) }
    | quoted_label as l { QUOTED_LABEL (String.sub l 1 (String.length l - 2)) }
    | projector_invoke as p { PROJECTOR_INVOKE p }
    | "true" { TRUE }
    | "false" { FALSE }
    | "module" { MODULE }
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
    | "{" { OPEN_CURLY }
    | "}" { CLOSE_CURLY }
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
    | "Void" { VOID_TYPE }
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
    | "~" {TILDE}
    | "/~" {SLASH_TILDE}
    | "?t" {T_TYP}
    | "?p" {P_PAT}
    | "?tp" {TP_TPAT}
    | "?e" {E_EXP}
    | "named_fun" {NAMED_FUN}
    | "poly" {POLY}
    | "rec" {REC}
    | identifier as i { named_token (fun name -> IDENT name) i }
    | constructor_ident as i { constructor_token lexbuf i }
    | eof { EOF }
    | _ { raise (Failure ("Lex error: unknown char: '" ^ Lexing.lexeme lexbuf ^ "'")) }

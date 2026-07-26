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
(* Require leading digits before dot *)
let float = ['0'-'9']+ '.' ['0'-'9']*
(* negative ints are done through unop *)
let int = ['0'-'9'] ['0'-'9']*

let string = '"' ([^ '"' '\\'] | '\\' ['"' '\\'])* '"'
let quoted_label = '`' ([^ '`' '\\'] | '\\' [''' '\\'])* '`'

let newline = '\r' | '\n' | "\r\n"

let whitespace = [' ' '\t']+

(* Names may contain Unicode: every non-ASCII character is a name character
   except the operator whitelist (Token.unicode_operator_chars in haz3lcore).
   ocamllex is byte-oriented with no Unicode property support, but the rule is
   now a fixed list rather than a property test, so we can spell it out in
   UTF-8: accept well-formed multi-byte sequences and subtract the five
   whitelisted operators, all of which happen to lead with 0xE2.

     e2 88 88  U+2208 IN
     e2 89 a0  U+2260 NOT EQUAL TO
     e2 89 ae  U+226E NOT LESS-THAN
     e2 89 af  U+226F NOT GREATER-THAN
     e2 8a 86  U+2286 SUBSET OF OR EQUAL TO

   Those five therefore fall through to the error rule here, matching the
   editor, which treats them as operators with no form rather than as names.

   Spelling out UTF-8 also narrows names from "any byte >= 0x80" to
   well-formed sequences, so malformed input now raises a lex error instead
   of lexing as a name. The editor's regexes work on decoded codepoints, so
   malformed bytes were never a name there either.

   Known divergences, both pre-existing and both in the safe direction (this
   lexer accepts a superset of the editor's names, so every name the editor
   accepts round-trips): a name starting with a non-ASCII UPPERCASE letter
   (`Ćtr`) is a constructor in the editor but lexes as an identifier here,
   because `constructor_ident` cannot test the case of a multi-byte character;
   and the characters the editor puts in NEITHER class -- Unicode whitespace
   like U+00A0, and the implicit-hole marker `¿` -- lex as name characters
   here. *)
let cont = ['\128'-'\191']
let utf8_2 = ['\194'-'\223'] cont
let utf8_4 = ['\240'-'\244'] cont cont cont
let utf8_3 =
    (['\224'-'\239'] # ['\226']) cont cont
  | '\226' (cont # ['\136' '\137' '\138']) cont
  | '\226' '\136' (cont # ['\136'])
  | '\226' '\137' (cont # ['\160' '\174' '\175'])
  | '\226' '\138' (cont # ['\134'])
let nonascii = utf8_2 | utf8_3 | utf8_4
let name_start = ['a'-'z' '_'] | nonascii
let name_rest = ['a'-'z' 'A'-'Z' '0'-'9' '_'] | nonascii
let identifier = name_start name_rest*
let constructor_ident = ['A'-'Z'] name_rest*
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
    | identifier as i { IDENT(i) }
    | constructor_ident as i { CONSTRUCTOR_IDENT(i)}
    | eof { EOF }
    | _ { raise (Failure ("Lex error: unknown char: '" ^ Lexing.lexeme lexbuf ^ "'")) }

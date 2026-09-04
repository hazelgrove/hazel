{
open Lexing
open Parser

(* Innermost-open-delimiter stack: Hazel disambiguates `;` by sort —
   directly inside module/signature braces it is the item separator,
   anywhere else (incl. inside parens/brackets nested in braces) it is
   the Seq operator. Track it here so the parser sees two tokens.
   NB stateful across lexbufs is fine: each parse starts fresh via
   reset_delims from Interface. *)
let delim_stack : char list ref = ref []
let reset_delims () = delim_stack := []
let push_delim c = delim_stack := c :: !delim_stack
let pop_delim () =
  match !delim_stack with [] -> () | _ :: tl -> delim_stack := tl
let semi_token () =
  match !delim_stack with '{' :: _ -> MOD_SEMI | _ -> SEMI_COLON

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

let string = '"' ([^ '"' '\\'] | '\\' _)* '"'
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

   The implicit-hole marker `¿` (c2 bf) is subtracted for the same reason:
   the editor puts it in neither class, so it is always its own token there,
   and the QUESTION rule below must win over `identifier` even when the
   marker abuts a name (`x¿`). Without the subtraction the fast path would
   lex that as one IDENT and silently disagree with the editor.

   Known divergences, all rooted in the same limitation: ocamllex cannot
   test the case of a multi-byte character, so a name starting with a
   non-ASCII UPPERCASE letter is a constructor in the editor but lexes as an
   identifier here (`Ćtr`), and `^Ć` lexes as a livelit here though the
   editor requires a non-uppercase start. Unicode whitespace like U+00A0
   likewise lexes as a name character here, where the editor puts it in
   neither class. *)
let cont = ['\128'-'\191']
let utf8_2 = ['\195'-'\223'] cont | '\194' (cont # ['\191'])
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
(* `'` continues a variable but not a constructor, as in the editor. *)
let var_rest = name_rest | '\''
let identifier = name_start var_rest*
let constructor_ident = ['A'-'Z'] name_rest*
let sexp_string = '`' [^'`']* '`'
let ints = ['0'-'9']+
let projector_invoke = "^^" ['a'-'z' 'A'-'Z' '0'-'9' '_']+
(* Same name alphabet as `identifier`, minus `'`, matching Token.is_livelit:
   `^é` is a livelit in the editor, so it must be one token here too. *)
let livelit_ident = '^' name_start name_rest*
let comment = '#' [^ '#' '\n']* '#'

rule token = 
    parse 
    | "undef" { UNDEF}
    | whitespace {token lexbuf }
    | comment { token lexbuf }
    | newline { advance_line lexbuf; token lexbuf}
    | ints as i { INT (Util.Bigint.of_string i) }
    | float as f { FLOAT (parse_float_string f )}
    | string as s { STRING (String.sub s 1 (String.length s - 2)) }
    | quoted_label as l { QUOTED_LABEL (String.sub l 1 (String.length l - 2)) }
    | projector_invoke as p { PROJECTOR_INVOKE p }
    | livelit_ident as l { LIVELIT_IDENT l }
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
    | "[" { push_delim '['; OPEN_SQUARE_BRACKET }
    | "]" { pop_delim (); CLOSE_SQUARE_BRACKET }
    | "(" { push_delim '('; OPEN_PAREN }
    | ")" { pop_delim (); CLOSE_PAREN }
    | "{{{" { push_delim 't'; OPEN_TRIPLE_CURLY }
    | "}}}" { pop_delim (); CLOSE_TRIPLE_CURLY }
    | "{" { push_delim '{'; OPEN_CURLY }
    | "}" { pop_delim (); CLOSE_CURLY }
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
    | "SInt" { SINT_TYPE }
    | "Nat" { NAT_TYPE }
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
    | ";" { semi_token () }
    | "test" {TEST}
    | "hint" {HINT}
    | "|>" { PIPELINE }
    | "::" { CONS }
    | "@<" {TYP_AP_SYMBOL}
    | "@" {AT_SYMBOL}
    | "?" {QUESTION}
    | "\xc2\xbf" {QUESTION} (* ¿ implicit-hole marker (TextRoundtrip) *)
    | "_" {WILD}
    | "use" {USE}
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

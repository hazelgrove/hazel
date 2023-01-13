{
  let mk_tile = fun s -> Spiece.P (T (Tile.mk ","))
}

let digit = ['0'-'9']

let int_lit = (digit | '_')+
let float_lit = digit+ '.' digit* | digit* '.' digit+

let alpha = ['a'-'z' 'A'-'Z']
let alpha_lower = ['a'-'z']
let alpha_upper = ['A'-'Z']

let id_lower = alpha_lower (alpha | digit | '_')*
let id_upper = alpha_upper (alpha | digit | '_')*

let newline = '\r' | '\n' | "\r\n"
let whitespace = (' ' | '\t' | newline)+

let op_int =
  '+' | '-' | '*' | '/' | "**" | '>' | ">=" | '<' | "<=" | "=="
let op_float =
  "+." | "-." | "*." | "/." | "**." | ">." | ">=." | "<." | "<=." | "==."
let op_bool =
  "&&" | "||"

let op =
  op_int | op_float | op_bool |
  "::" | ',' | ':' | "->"

let token = op | id_lower | id_upper | int_lit | float_lit

rule next_spiece = parse
| whitespace { Spiece.S (Space.mk (Lexing.lexeme lexbuf)) }
| token { Spiece.P (T (Tile.mk (Lexing.lexeme lexbuf))) }

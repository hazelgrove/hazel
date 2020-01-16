{
  open SkelExprParser
}

let white = [' ']+
let digit = ['0'-'9']
let placeholder = digit+

rule read =
  parse
  | white { read lexbuf }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "_" { SPACEOP }
  | "," { COMMA }
  | "::" { CONS }
  | "<" { LT }
  | ">" { GT }
  | "=" { EQ }
  | "|" { OR }
  | "&" { AND }
  | placeholder { PLACEHOLDER (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }


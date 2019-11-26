{
  open SkelTypParser
}

let white = [' ']+
let digit = ['0'-'9']
let placeholder = digit+

rule read =
  parse
  | white { read lexbuf }
  | "|" { VBAR }
  | "->" { ARROW }
  | "," { COMMA }
  | placeholder { PLACEHOLDER (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }


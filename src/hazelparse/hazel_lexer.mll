{
        open Hazel_parser
}

let white = [' ']+
let digit = ['0'-'9']
let numlit = digit+

rule read =
  parse
  | white { read lexbuf }
  | "+" { PLUS }
  | "-" { MINUS }
  | numlit { INT (Lexing.lexeme lexbuf) }
  | eof { EOF }

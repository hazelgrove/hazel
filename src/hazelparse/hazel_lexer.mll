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
  | "*" { MULT }
  | "/" { DIV }
  | numlit { INT (Lexing.lexeme lexbuf) }
  | eof { EOF }

{
  open Skel
  open Skelparser
}

let white = [' ']+
let digit = ['0'-'9']
let placeholder = digit+

rule read = 
  parse
  | white { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "_" { SPACEOP }
  | placeholder { PLACEHOLDER (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }


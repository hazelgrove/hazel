{
        open TestParse
}

let white = [' ']+
let digit = ['0'-'9']
let numlit = digit+

rule read =
        parse
        | white { read lexbuf }
        | "+" { PLUS }
        (*
        | "-" { MINUS }
        | ">" { GT }
        | "<" { LT }
        | "=" { EQ }
        *)
        | numlit { INT (Lexing.lexeme lexbuf) }
        | eof { EOF }

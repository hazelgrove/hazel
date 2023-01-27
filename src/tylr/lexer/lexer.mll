let digit = ['0'-'9']

let int_lit = (digit | '_')+
let float_lit = digit+ '.' digit* | digit* '.' digit+

let alpha_lower = ['a'-'z']
let alpha_upper = ['A'-'Z']
let alpha = alpha_lower | alpha_upper

(* todo: allow leading underscore *)
let id_lower = alpha_lower (alpha | digit | '_')*
let id_upper = alpha_upper (alpha | digit | '_')*

let newline = '\r' | '\n' | "\r\n"
let space = ' ' | '\t'

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

rule next_lexeme = parse
| newline { Some (Lexeme.S (Space.mk(Newline))) }
| space   { Some (Lexeme.S (Space.mk(Space))) }
| token   {
    (* todo: use dummy id and have client handle regen *)
    let token = Lexing.lexeme lexbuf in
    let mold =
      match String.get token 0 with
      | '_' -> Mold.default_operand
      | c when Util.CharUtil.is_alphanum c -> Mold.default_operand
      | _ -> Mold.default_infix
    in
    Some (Lexeme.T (Tile.mk mold token))
  }
| eof { None }

{
  let lex = fun s ->
    let buf = Lexing.from_string s in
    let rev = ref [] in
    let rec go () =
      match next_lexeme buf with
      | None -> ()
      | Some lx -> rev := lx::!rev; go ()
    in
    go (); List.rev(!rev)
}
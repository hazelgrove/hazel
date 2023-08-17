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
let space = (' ' | '\t' | newline)+

let op_int =
  '+' | '-' | '*' | '/' | "**" | '>' | ">=" | '<' | "<=" | "=="
let op_float =
  "+." | "-." | "*." | "/." | "**." | ">." | ">=." | "<." | "<=." | "==."
let op_bool =
  "&&" | "||"

let op =
  op_int | op_float | op_bool |
  "::" | ',' | ':' | "->"

let paren = "(" | ")"
let brack = "[" | "]"

let const = paren | brack

rule next_lexeme = parse
| space {
    Some (Lexeme.S (Space.of_string (Lexing.lexeme lexbuf)))
  }
| id_lower {
    Some (Lexeme.T (Label.Id_lower, Lexing.lexeme lexbuf))
  }
| id_upper {
    Some (Lexeme.T (Label.Id_upper, Lexing.lexeme lexbuf))
  }
| int_lit {
    Some (Lexeme.T (Label.Int_lit, Lexing.lexeme lexbuf))
  }
| float_lit {
    Some (Lexeme.T (Label.Float_lit, Lexing.lexeme lexbuf))
  }
| const {
    let t = Lexing.lexeme lexbuf in
    Some (Lexeme.T (Label.Const t, t))
  }
| eof { None }

{
  let lex = fun s ->
    let buf = Lexing.from_string s in
    let rev = ref [] in
    let rec go () =
      match next_lexeme buf with
      | None -> print_endline "unrecognized token"; ()
      | Some lx -> rev := lx::!rev; go ()
    in
    go (); List.rev(!rev)

  (* returns label of s if lexed as single token *)
  let label s =
    match lex s with
    | [T (lbl, _)] -> Some lbl
    | _ -> None
}
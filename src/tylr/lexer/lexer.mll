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
    Some (Label.Id_lower, Lexeme.T (Lexing.lexeme lexbuf))
  }
| id_upper {
    Some (Label.Id_upper, Lexeme.T (Lexing.lexeme lexbuf))
  }
| int_lit {
    Some (Label.Int_lit, Lexeme.T (Lexing.lexeme lexbuf))
  }
| float_lit {
    Some (Label.Float_lit, Lexeme.T (Lexing.lexeme lexbuf))
  }
| const {
    let t = Lexing.lexeme lexbuf in
    Some (Label.Const t, Lexeme.T t)
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

  (* returns label of s if lexed as single token *)
  let label = fun s ->
    match lex s with
    | [(lbl, T t)] -> Some lbl
    | _ -> None

  module Relexed = struct
    open Sexplib.Std

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (Lexeme.s, int)

    let empty = ([], 0)
  end

  let relex_post_delete = fun well ->
    match Stepwell.uncons_opt_lexemes(well) with
    | ((None | Some (S _ | T {matter: Grout, token: "", _}), _), _)
    | ((_, None | Some (S _ | T {matter: Grout, token: "", _})), _) ->
      (Relexed.empty, well)
    | ((Some (T l), Some (T r)), well') ->
      begin match lex (l.token ^ r.token) with
      | T l'::T r'::[] when l' = l.token && r' = r.token ->
        (Relexed.empty, well)
      | ls -> ((ls, Token.length r.token), well')
      end

  let relex_post_insert =

}
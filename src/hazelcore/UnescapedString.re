open Sexplib.Std;

[@deriving sexp]
type t = string;

let to_string = s => s;

// FIXME: Lexing is repeating escape sequences literally ("\090" -> "Z\\090")
let from_string = s =>
  s |> Lexing.from_string |> StringLitLexer.stringlit_body;

let equal = String.equal;

let concat = (s1, s2) => s1 ++ s2;

[@deriving sexp]
type t =
  | Let
  | Case
  | Define;

let is_Let = String.equal("let");
let is_Case = String.equal("case");
let is_Define = String.equal("define");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Case) {
    Some(Case);
  } else if (text |> is_Define) {
    Some(Define);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | Case => "case"
  | Define => "define";

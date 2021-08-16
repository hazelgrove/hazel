[@deriving (sexp, show)]
type t =
  | Let
  | Case;

let is_Let = String.equal("let");
let is_Case = String.equal("case");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Case) {
    Some(Case);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | Case => "case";

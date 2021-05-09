[@deriving sexp]
type t =
  | Let
  | And
  | Case;

let is_Let = String.equal("let");
let is_And = String.equal("and");
let is_Case = String.equal("case");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_And) {
    Some(And);
  } else if (text |> is_Case) {
    Some(Case);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | And => "and"
  | Case => "case";

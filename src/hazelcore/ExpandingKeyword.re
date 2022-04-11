[@deriving sexp]
type t =
  | Let
  | And
  | Case
  | Fun;

let is_Let = String.equal("let");
let is_And = String.equal("and");
let is_Case = String.equal("case");
let is_Fun = String.equal("fun");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_And) {
    Some(And);
  } else if (text |> is_Case) {
    Some(Case);
  } else if (text |> is_Fun) {
    Some(Fun);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | And => "and"
  | Case => "case"
  | Fun => "fun";

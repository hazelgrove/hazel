[@deriving sexp]
type t =
  | Let
  | Abbrev
  | Case;

let is_Let = String.equal("let");
let is_Abbrev = String.equal("abbrev");
let is_Case = String.equal("case");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Abbrev) {
    Some(Abbrev);
  } else if (text |> is_Case) {
    Some(Case);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | Abbrev => "abbrev"
  | Case => "case";

[@deriving sexp]
type t =
  | Let
  | Case
  | Lam;

let is_Let = String.equal("let");
let is_Case = String.equal("case");
let is_Lam = String.equal("fun");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Case) {
    Some(Case);
  } else if (text |> is_Lam) {
    Some(Lam);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | Case => "case"
  | Lam => "fun";

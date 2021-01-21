[@deriving sexp]
type t =
  | Let
  | Case
  | If;

let is_Let = String.equal("let");
let is_Case = String.equal("case");
let is_If = String.equal("if");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Case) {
    Some(Case);
  } else if (text |> is_If) {
    Some(If);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | Case => "case"
  | If => "if";

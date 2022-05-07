[@deriving sexp]
type t =
  | Fun
  | Let
  | Case
  | TyAlias;

let of_string: string => option(t) =
  fun
  | "fun" => Some(Fun)
  | "let" => Some(Let)
  | "case" => Some(Case)
  | "type" => Some(TyAlias)
  | _ => None;

let is_Fun = String.equal("fun");
let is_Let = String.equal("let");
let is_Case = String.equal("case");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Case) {
    Some(Case);
  } else if (text |> is_Fun) {
    Some(Fun);
  } else {
    None;
  };

let to_string =
  fun
  | Fun => "fun"
  | Let => "let"
  | Case => "case"
  | TyAlias => "type";

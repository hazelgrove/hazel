[@deriving sexp]
type t =
  | Let
  | Match
  | Fun;

let is_Let = String.equal("let");
let is_Match = String.equal("match");
let is_Fun = String.equal("fun");

let mk = (text: string): option(t) =>
  if (text |> is_Let) {
    Some(Let);
  } else if (text |> is_Match) {
    Some(Match);
  } else if (text |> is_Fun) {
    Some(Fun);
  } else {
    None;
  };

let to_string =
  fun
  | Let => "let"
  | Match => "match"
  | Fun => "fun";

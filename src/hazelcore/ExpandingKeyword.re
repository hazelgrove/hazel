[@deriving sexp]
type t =
  | Let
  | Case
  | TyAlias;

let of_string: string => option(t) =
  fun
  | "let" => Some(Let)
  | "case" => Some(Case)
  | "type" => Some(TyAlias)
  | _ => None;

let to_string =
  fun
  | Let => "let"
  | Case => "case"
  | TyAlias => "type";

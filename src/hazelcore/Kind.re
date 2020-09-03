[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(HTyp.t);

let to_string: t => string =
  fun
  | KHole => "KHole"
  | Type => "Type"
  | _ => "Singleton";

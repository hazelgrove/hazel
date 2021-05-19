open Sexplib;

[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(HTyp.t);

let to_string: t => string =
  fun
  | KHole => "KHole"
  | Type => "Type"
  | Singleton(typ) =>
    "Singleton(" ++ (HTyp.sexp_of_t(typ) |> Sexp.to_string) ++ ")";

let is_singleton: t => bool =
  fun
  | Singleton(_) => true
  | KHole
  | Type => false;

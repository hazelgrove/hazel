open Sexplib;

[@deriving sexp]
type t =
  | KHole
  | Type
  | Singleton(t, HTyp.t);

let rec to_string: t => string =
  fun
  | KHole => "KHole"
  | Type => "Type"
  | Singleton(t, typ) =>
    "Singleton("
    ++ to_string(t)
    ++ ", "
    ++ (HTyp.sexp_of_t(typ) |> Sexp.to_string)
    ++ ")";

let is_singleton: t => bool =
  fun
  | Singleton(_) => true
  | KHole
  | Type => false;

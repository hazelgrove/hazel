[@deriving sexp]
type kw =
  | Assert
  | Same;

let type_of_kw: kw => HTyp.t =
  fun
  | Assert => Arrow(Bool, Prod([]))
  | Same => Arrow(Hole, Arrow(Hole, Prod([])));

let string_of_kw: kw => string =
  fun
  | Assert => "assert"
  | Same => "same";

let kw_of_string: string => option(kw) =
  fun
  | "assert" => Some(Assert)
  | "same" => Some(Same)
  | _ => None;

[@deriving sexp]
type t =
  | Typed(kw, ErrStatus.t, KeywordID.t);

let to_string: t => string =
  fun
  | Typed(kw, _, _) => string_of_kw(kw);

let length: t => int = kw => String.length(to_string(kw));

[@deriving sexp]
type kw =
  | Test;

let type_of_kw =
  fun
  | Test => HTyp.Arrow(Bool, Prod([]));

let string_of_kw =
  fun
  | Test => "test";

let kw_of_string =
  fun
  | "test" => Some(Test)
  | _ => None;

[@deriving sexp]
type t =
  | Typed(kw, ErrStatus.t, KeywordID.t);

let to_string =
  fun
  | Typed(kw, _, _) => string_of_kw(kw);

let length = kw => String.length(to_string(kw));

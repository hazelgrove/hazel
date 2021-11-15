[@deriving sexp]
type kw =
  | Test;

let type_of_kw: kw => HTyp.t =
  fun
  | Test => Arrow(Bool, Prod([]));

let string_of_kw: kw => string =
  fun
  | Test => "test";

let kw_of_string: string => option(kw) =
  fun
  | "test" => Some(Test)
  | _ => None;

[@deriving sexp]
type t =
  | Typed(kw, ErrStatus.t, KeywordID.t);

let to_string: t => string =
  fun
  | Typed(kw, _, _) => string_of_kw(kw);

let length: t => int = kw => String.length(to_string(kw));

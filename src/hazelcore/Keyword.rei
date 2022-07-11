[@deriving sexp]
type kw =
  | Test;

let type_of_kw: kw => HTyp.t;
let string_of_kw: kw => string;
let kw_of_string: string => option(kw);

[@deriving sexp]
type t =
  | Typed(kw, ErrStatus.t, KeywordID.t);

let to_string: t => string;
let length: t => int;

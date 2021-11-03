[@deriving sexp]
type kw =
  | Assert;

[@deriving sexp]
type t =
  | Typed(kw, ErrStatus.t, KeywordID.t);

let string_of_kw: kw => string =
  fun
  | Assert => AssertStatus.name;

let to_string: t => string =
  fun
  | Typed(kw, _, _) => string_of_kw(kw);

let kw_of_string: string => option(kw) =
  fun
  | "assert" => Some(Assert)
  | _ => None;

let length: t => int = kw => String.length(to_string(kw));

//TODO(andrew)
let type_of_kw: kw => HTyp.t =
  fun
  | Assert => AssertStatus.assert_ty;

let type_of: t => HTyp.t =
  fun
  | Typed(kw, _, _) => type_of_kw(kw);

/* type variable errors */
module HoleReason = {
  [@deriving sexp]
  type t =
    | Unbound
    | Reserved
    | InvalidName;
};

/* type variable hole status */
module Status = {
  [@deriving sexp]
  type t =
    | NotInHole(Index.t)
    | InHole(HoleReason.t, MetaVar.t);
};
let valid_name: string => bool = {
  let re = Re.Str.regexp("^[_a-zA-Z][_a-zA-Z0-9']*$");
  s => Re.Str.string_match(re, s, 0);
};

let reserved_word = (s: string): bool =>
  // built-in types
  s == "Int"
  || s == "Float"
  || s == "Bool"
  // keywords
  || s == "let"
  || s == "case";

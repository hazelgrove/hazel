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

/* type variable names */
module Name = {
  open Sexplib.Std;

  [@deriving sexp]
  type t = string;

  let of_string: string => t = name => name;

  let to_string: t => string = name => name;

  let length: t => int = String.length;

  let equal: (t, t) => bool = String.equal;

  let valid = {
    let re = Re.Str.regexp("^[_a-zA-Z][_a-zA-Z0-9']*$");
    (s: string) => (Re.Str.string_match(re, s, 0): bool);
  };

  let reserved = (s: string): bool =>
    // built-in types
    s == "Int"
    || s == "Float"
    || s == "Bool"
    // keywords
    || s == "let"
    || s == "case";
};

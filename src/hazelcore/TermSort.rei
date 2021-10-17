[@deriving sexp]
type t =
  | Typ
  | Pat
  | Exp;

[@deriving sexp]
type syntax =
  | Typ(UHTyp.t)
  | Pat(UHPat.t)
  | Exp(UHExp.t);

let to_string: t => string;

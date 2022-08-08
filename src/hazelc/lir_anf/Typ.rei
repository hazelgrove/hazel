[@deriving (sexp, eq, ord)]
type t = (Complete.t, t_)

[@deriving (sexp, eq, ord)]
and t_ =
  | THole
  | TInt
  | TFloat
  | TBool
  | TArrow(t, t)
  | TSum(t_, t_)
  | TPair(t_, t_)
  | TUnit
  | TList(t_);

let nc: t_ => t;
let ii: t_ => t;
let ni: t_ => t;

let is_nc: t => option(t_);
let is_ii: t => option(t_);
let is_ni: t => option(t_);

let consistent: (t, t) => bool;
let consistent_: (t_, t_) => bool;

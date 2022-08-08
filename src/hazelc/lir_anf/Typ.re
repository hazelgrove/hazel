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

let nc = ty_ => (Complete.NecessarilyComplete, ty_);
let ii = ty_ => (Complete.IndeterminatelyIncomplete, ty_);
let ni = ty_ => (Complete.NecessarilyIncomplete, ty_);

let is_nc =
  fun
  | (Complete.NecessarilyComplete, ty_) => Some(ty_)
  | _ => None;

let is_ii =
  fun
  | (Complete.IndeterminatelyIncomplete, ty_) => Some(ty_)
  | _ => None;

let is_ni =
  fun
  | (Complete.NecessarilyIncomplete, ty_) => Some(ty_)
  | _ => None;

let rec consistent_ = (ty_, ty_') =>
  switch (ty_, ty_') {
  | (THole, _)
  | (_, THole) => true
  | (TInt, TInt) => true
  | (TInt, _) => false
  | (TFloat, TFloat) => true
  | (TFloat, _) => false
  | (TBool, TBool) => true
  | (TBool, _) => false
  | (TArrow(ty1, ty2), TArrow(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | (TSum(ty_1, ty_2), TSum(ty_1', ty_2')) =>
    consistent_(ty_1, ty_1') && consistent_(ty_2, ty_2')
  | (TArrow(_, _), _) => false
  | (TSum(_, _), _) => false
  | (TPair(ty_1, ty_2), TPair(ty_1', ty_2')) =>
    consistent_(ty_1, ty_1') && consistent_(ty_2, ty_2')
  | (TPair(_, _), _) => false
  | (TUnit, TUnit) => true
  | (TUnit, _) => false
  | (TList(ty_), TList(ty_')) => consistent_(ty_, ty_')
  | (TList(_), _) => false
  }

and consistent = ((cc, ty), (cc', ty')) =>
  if (Complete.equal(cc, cc')) {
    consistent_(ty, ty');
  } else {
    false;
  };

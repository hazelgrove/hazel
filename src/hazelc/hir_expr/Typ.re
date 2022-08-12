[@deriving (sexp, eq, ord)]
type t =
  | THole
  | TInt
  | TFloat
  | TBool
  | TArrow(t, t)
  | TSum(t, t)
  | TPair(t, t)
  | TUnit
  | TList(t);

let rec consistent = (x, y) =>
  switch (x, y) {
  | (THole, _)
  | (_, THole) => true
  | (TInt, TInt) => true
  | (TInt, _) => false
  | (TFloat, TFloat) => true
  | (TFloat, _) => false
  | (TBool, TBool) => true
  | (TBool, _) => false
  | (TArrow(ty1, ty2), TArrow(ty1', ty2'))
  | (TSum(ty1, ty2), TSum(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | (TArrow(_, _), _) => false
  | (TSum(_, _), _) => false
  | (TPair(ty1, ty2), TPair(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | (TPair(_, _), _) => false
  | (TUnit, TUnit) => true
  | (TUnit, _) => false
  | (TList(ty), TList(ty')) => consistent(ty, ty')
  | (TList(_), _) => false
  };

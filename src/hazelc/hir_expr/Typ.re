open Sexplib.Std;

[@deriving sexp]
type t =
  | THole
  | TInt
  | TFloat
  | TBool
  | TArrow(t, t)
  | TSum(t, t)
  | TProd(list(t))
  | TList(t);

let equal = (==);

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
  | (TProd(tys1), TProd(tys2)) =>
    Util.ListUtil.for_all2_opt(consistent, tys1, tys2)
    |> Option.value(~default=false)
  | (TProd(_), _) => false
  | (TList(ty), TList(ty')) => consistent(ty, ty')
  | (TList(_), _) => false
  };

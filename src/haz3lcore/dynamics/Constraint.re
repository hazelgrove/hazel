open Sexplib.Std;

[@deriving sexp]
type t =
  | Truth
  | Falsity
  | Hole
  | Int(int)
  | NotInt(int)
  | Float(float)
  | NotFloat(float)
  | String(string)
  | NotString(string)
  | And(t, t)
  | Or(t, t)
  | InjL(t)
  | InjR(t)
  | List(list(t));

// How to replace this function?
let rec constrains = (c: t, ty: HTyp.t): bool =>
  switch (c, ty) {
  // switch (c, HTyp.head_normalize(InitialContext.ctx, ty)) {
  | (Truth, _)
  | (Falsity, _)
  | (Hole, _) => true
  | (Int(_) | NotInt(_), Int) => true
  | (Int(_) | NotInt(_), _) => false
  | (Float(_) | NotFloat(_), Float) => true
  | (Float(_) | NotFloat(_), _) => false
  | (String(_) | NotString(_), String) => true
  | (String(_) | NotString(_), _) => false
  | (And(c1, c2), _) => constrains(c1, ty) && constrains(c2, ty)
  | (Or(c1, c2), _) => constrains(c1, ty) && constrains(c2, ty)
  | (InjL(c1), Sum(ty1, _)) => constrains(c1, ty1)
  | (InjL(_), _) => false
  | (InjR(c2), Sum(_, ty2)) => constrains(c2, ty2)
  | (InjR(_), _) => false
  | (List(c), ty) =>
    List.fold_left((last, x) => last && constrains(x, ty), true, c)
  };

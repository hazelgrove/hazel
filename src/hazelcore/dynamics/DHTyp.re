type t =
  | TyVar(TyVar.Ctx.t, TyVar.t)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

open Sexplib.Std;

/* types with holes */
[@deriving sexp]
type t =
  | TyVar(Index.t, string)
  | TyVarHole(TyVar.HoleReason.t, MetaVar.t, string)
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

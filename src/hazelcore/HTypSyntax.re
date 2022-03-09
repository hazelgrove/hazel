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

let rec increment_indices: t => t =
  fun
  | TyVar(i, name) => TyVar(Index.increment(i), name)
  | (TyVarHole(_) | Hole | Int | Float | Bool) as ty => ty
  | Arrow(t1, t2) => Arrow(increment_indices(t1), increment_indices(t2))
  | Sum(t1, t2) => Sum(increment_indices(t1), increment_indices(t2))
  | List(t) => List(increment_indices(t))
  | Prod(lst) => Prod(List.map(increment_indices, lst));

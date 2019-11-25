open Sexplib.Std;
open GeneralUtil;

[@deriving sexp]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | Wild
  | Keyword(MetaVar.t, MetaVarInst.t, Keyword.t)
  | Var(Var.t)
  | NumLit(int)
  | BoolLit(bool)
  | Inj(InjSide.t, t)
  | ListNil
  | Cons(t, t)
  | Pair(t, t)
  | Triv /* unit intro */
  | Ap(t, t);

let rec make_tuple = (ds: ListMinTwo.t(t)): t =>
  switch (ds) {
  | Pair(d1, d2) => Pair(d1, d2)
  | Cons(d1, ds) =>
    let d2 = make_tuple(ds);
    Pair(d1, d2);
  };

/**
   * Whether dp contains the variable x outside of a hole.
   */
let rec binds_var = (x: Var.t, dp: t): bool =>
  switch (dp) {
  | EmptyHole(_, _)
  | NonEmptyHole(_, _, _, _)
  | Wild
  | NumLit(_)
  | BoolLit(_)
  | Triv
  | ListNil
  | Keyword(_, _, _) => false
  | Var(y) => Var.eq(x, y)
  | Inj(_, dp1) => binds_var(x, dp1)
  | Pair(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | Ap(_, _) => false
  };

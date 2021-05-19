open Sexplib.Std;

[@deriving sexp]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | Wild
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, string)
  | Var(Var.t)
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | Inj(InjSide.t, t)
  | ListNil
  | Cons(t, t)
  | Tuple(list((option(Label.t), t)))
  | Ap(t, t)
  | ErrLabel(Label.t);

/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (x: Var.t, dp: t): bool =>
  switch (dp) {
  | EmptyHole(_, _)
  | NonEmptyHole(_, _, _, _)
  | Wild
  | InvalidText(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | Triv
  | ListNil
  | Keyword(_, _, _)
  | ErrLabel(_) => false
  | Var(y) => Var.eq(x, y)
  | Inj(_, dp1) => binds_var(x, dp1)
  | Tuple(tuple_elts) =>
    List.fold_left(
      (acc, (label, dp)) => {acc || binds_var(x, dp)},
      false,
      tuple_elts,
    )
  | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)

  | Ap(_, _) => false
  };

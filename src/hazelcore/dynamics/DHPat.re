open Sexplib.Std;

module UnIntOp = {
  [@deriving sexp]
  type t =
    | Negate;
  let of_op = (op: UHPat.unop): option((t, HTyp.t)) =>
    switch (op) {
    | Negate => Some((Negate, Int))
    | _ => None
    };
  let to_op = (op: t): UHPat.unop =>
    switch (op) {
    | Negate => Negate
    };
};

module UnFloatOp = {
  [@deriving sexp]
  type t =
    | FNegate;
  let of_op = (op: UHPat.unop): option((t, HTyp.t)) =>
    switch (op) {
    | FNegate => Some((FNegate, Float))
    | _ => None
    };
  let to_op = (op: t): UHPat.unop =>
    switch (op) {
    | FNegate => FNegate
    };
};

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
  | UnIntOp(UnIntOp.t, t)
  | UnFloatOp(UnFloatOp.t, t)
  | Inj(InjSide.t, t)
  | ListNil
  | Cons(t, t)
  | Pair(t, t)
  | Triv /* unit intro */
  | Ap(t, t);

let rec mk_tuple: list(t) => t =
  fun
  | [] => failwith("mk_tuple: expected at least 1 element")
  | [dp] => dp
  | [dp, ...dps] => Pair(dp, mk_tuple(dps));

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
  | Keyword(_, _, _) => false
  | Var(y) => Var.eq(x, y)
  | Inj(_, dp1) => binds_var(x, dp1)
  | UnIntOp(_, dp) => binds_var(x, dp)
  | UnFloatOp(_, dp) => binds_var(x, dp)
  | Pair(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | Ap(_, _) => false
  };

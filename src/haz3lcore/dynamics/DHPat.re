open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | Wild
  | ExpandingKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, string)
  | Var(Var.t)
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | StringLit(string)
  | Inj(InjSide.t, t)
  | ListLit(HTyp.t, list(t))
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
  | StringLit(_)
  | Triv
  | ExpandingKeyword(_, _, _) => false
  | Var(y) => Var.eq(x, y)
  | Inj(_, dp1) => binds_var(x, dp1)
  | Pair(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | ListLit(_, d_list) =>
    let new_list = List.map(binds_var(x), d_list);
    List.fold_left((||), false, new_list);
  | Ap(_, _) => false
  };

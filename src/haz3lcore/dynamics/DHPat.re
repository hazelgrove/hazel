open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | Wild
  | InvalidText(MetaVar.t, MetaVarInst.t, string)
  | BadConstructor(MetaVar.t, MetaVarInst.t, string)
  | Var(Var.t)
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | StringLit(string)
  | ListLit(Typ.t, list(t))
  | Cons(t, t)
  | Tuple(list(t))
  | Constructor(string, Typ.t)
  | Ap(t, t);

let mk_tuple: list(t) => t =
  fun
  | []
  | [_] => failwith("mk_tuple: expected at least 2 elements")
  | dps => Tuple(dps);

/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (x: Var.t, dp: t): bool =>
  switch (dp) {
  | EmptyHole(_, _)
  | NonEmptyHole(_, _, _, _)
  | Wild
  | InvalidText(_)
  | BadConstructor(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | StringLit(_)
  | Constructor(_) => false
  | Var(y) => Var.eq(x, y)
  | Tuple(dps) => dps |> List.exists(binds_var(x))
  | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | ListLit(_, d_list) =>
    let new_list = List.map(binds_var(x), d_list);
    List.fold_left((||), false, new_list);
  | Ap(_, _) => false
  };

let rec bound_vars = (dp: t): list(Var.t) =>
  switch (dp) {
  | EmptyHole(_, _)
  | NonEmptyHole(_, _, _, _)
  | Wild
  | InvalidText(_)
  | BadConstructor(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | StringLit(_)
  | Constructor(_) => []
  | Var(y) => [y]
  | Tuple(dps) => List.flatten(List.map(bound_vars, dps))
  | Cons(dp1, dp2) => bound_vars(dp1) @ bound_vars(dp2)
  | ListLit(_, dps) => List.flatten(List.map(bound_vars, dps))
  | Ap(_, dp1) => bound_vars(dp1)
  };

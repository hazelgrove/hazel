open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t)
  // TODO: Work out what to do with invalids
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | ExpandingKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  // Same
  | Wild
  | Int(int)
  | Float(float)
  | Bool(bool)
  | String(string)
  // TODO:
  | InvalidText(MetaVar.t, MetaVarInst.t, string)
  | BadConstructor(MetaVar.t, MetaVarInst.t, string)
  | Var(Var.t)
  | ListLit(Typ.t, list(t))
  | Cons(t, t)
  | Tuple(list(t))
  | Constructor(string)
  | Ap(t, t);

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
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Constructor(_)
  | ExpandingKeyword(_, _, _) => false
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
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Constructor(_)
  | ExpandingKeyword(_, _, _) => []
  | Var(y) => [y]
  | Tuple(dps) => List.flatten(List.map(bound_vars, dps))
  | Cons(dp1, dp2) => bound_vars(dp1) @ bound_vars(dp2)
  | ListLit(_, dps) => List.flatten(List.map(bound_vars, dps))
  | Ap(_, dp1) => bound_vars(dp1)
  };

let get_var = (pat: t) => {
  switch (pat) {
  | Var(x) => Some(x)
  | Wild
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | ListLit(_)
  | Cons(_, _)
  | Tuple(_)
  | Constructor(_)
  | EmptyHole(_)
  | NonEmptyHole(_)
  | ExpandingKeyword(_)
  | InvalidText(_)
  | BadConstructor(_)
  | Ap(_) => None
  };
};

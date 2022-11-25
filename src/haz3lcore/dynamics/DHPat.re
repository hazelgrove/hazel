include Term.UPat;
// open Sexplib.Std;
//
// [@deriving (show({with_path: false}), sexp, yojson)]
// type t =
//   | EmptyHole(MetaVar.t, MetaVarInst.t)
//   | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
//   | Wild
//   | ExpandingKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
//   | InvalidText(MetaVar.t, MetaVarInst.t, string)
//   | Var(Var.t)
//   | IntLit(int)
//   | FloatLit(float)
//   | BoolLit(bool)
//   | StringLit(string)
//   | Inj(InjSide.t, t)
//   | ListLit(Typ.t, list(t))
//   | Cons(t, t)
//   | Tuple(list(t))
//   | Tag(string)
//   | Ap(t, t);
//
let mk_tuple: list(t) => t =
  fun
  | []
  | [_] => failwith("mk_tuple: expected at least 2 elements")
  | dps => {
      ids: List.fold_left((acc, ts) => acc @ ts.ids, [], dps),
      term: Tuple(dps),
    };

/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (x: Var.t, dp: t): bool =>
  switch (dp.term) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Triv
  | Tag(_) => false
  | Var(y) => Var.eq(x, y)
  | Parens(d) => binds_var(x, d)
  | Inj(_, dp1) => binds_var(x, dp1)
  | Tuple(dps) => dps |> List.exists(binds_var(x))
  | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | ListLit(d_list) =>
    let new_list = List.map(binds_var(x), d_list);
    List.fold_left((||), false, new_list);
  | Ap(_, _) => false
  | TypeAnn(d, _) => binds_var(x, d)
  };

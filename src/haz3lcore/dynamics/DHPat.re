include CH.CPat;

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
  | Hole(_)
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
  | ListLit(d_list, _) =>
    let new_list = List.map(binds_var(x), d_list);
    List.fold_left((||), false, new_list);
  | Ap(_, _) => false
  | TypeAnn(d, _) => binds_var(x, d)
  };

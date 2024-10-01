include Pat;

/* A Dynamic Pattern (DHPat) is a pattern that is part of an expression
   that has been type-checked. Hence why these functions take both a
   pattern, dp, and an info map, m, with type information. */

/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (m: Statics.Map.t, x: Var.t, dp: t): bool =>
  switch (Statics.get_pat_error_at(m, rep_id(dp))) {
  | Some(_) => false
  | None =>
    switch (dp |> term_of) {
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Invalid(_)
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | Constructor(_) => false
    | Cast(y, _, _)
    | Parens(y) => binds_var(m, x, y)
    | Var(y) => Var.eq(x, y)
    | Tuple(dps) => dps |> List.exists(binds_var(m, x))
    | Cons(dp1, dp2) => binds_var(m, x, dp1) || binds_var(m, x, dp2)
    | ListLit(d_list) =>
      let new_list = List.map(binds_var(m, x), d_list);
      List.fold_left((||), false, new_list);
    | Ap(_, _) => false
    }
  };

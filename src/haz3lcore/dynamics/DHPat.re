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
    | EmptyHolePat
    | MultiHolePat(_)
    | Wild
    | InvalidPat(_)
    | IntPat(_)
    | FloatPat(_)
    | BoolPat(_)
    | StringPat(_)
    | ConstructorPat(_) => false
    | CastPat(y, _, _)
    | ParensPat(y) => binds_var(m, x, y)
    | VarPat(y) => Var.eq(x, y)
    | TuplePat(dps) => dps |> List.exists(binds_var(m, x))
    | ConsPat(dp1, dp2) => binds_var(m, x, dp1) || binds_var(m, x, dp2)
    | ListLitPat(d_list) =>
      let new_list = List.map(binds_var(m, x), d_list);
      List.fold_left((||), false, new_list);
    | ApPat(_, _) => false
    }
  };

let rec bound_vars = (dp: t): list(Var.t) =>
  switch (dp |> term_of) {
  | EmptyHolePat
  | MultiHolePat(_)
  | Wild
  | InvalidPat(_)
  | IntPat(_)
  | FloatPat(_)
  | BoolPat(_)
  | StringPat(_)
  | ConstructorPat(_) => []
  | CastPat(y, _, _)
  | ParensPat(y) => bound_vars(y)
  | VarPat(y) => [y]
  | TuplePat(dps) => List.flatten(List.map(bound_vars, dps))
  | ConsPat(dp1, dp2) => bound_vars(dp1) @ bound_vars(dp2)
  | ListLitPat(dps) => List.flatten(List.map(bound_vars, dps))
  | ApPat(_, dp1) => bound_vars(dp1)
  };

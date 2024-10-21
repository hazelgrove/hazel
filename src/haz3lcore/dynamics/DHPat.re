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
    | Label(_)
    | Constructor(_) => false
    | Cast(y, _, _)
    | Parens(y) => binds_var(m, x, y)
    | Var(y) => Var.eq(x, y)
    | TupLabel(_, dp) => binds_var(m, x, dp)
    | Tuple(dps) => dps |> List.exists(binds_var(m, x))
    | Cons(dp1, dp2) => binds_var(m, x, dp1) || binds_var(m, x, dp2)
    | ListLit(d_list) =>
      let new_list = List.map(binds_var(m, x), d_list);
      List.fold_left((||), false, new_list);
    | Ap(_, _) => false
    }
  };

let rec bound_vars = (dp: t): list(Var.t) =>
  switch (dp |> term_of) {
  | EmptyHole
  | MultiHole(_)
  | Wild
  | Invalid(_)
  | Int(_)
  | Float(_)
  | Bool(_)
  | String(_)
  | Label(_)
  | Constructor(_) => []
  | Cast(y, _, _)
  | Parens(y) => bound_vars(y)
  | Var(y) => [y]
  | TupLabel(_, dp) => bound_vars(dp)
  | Tuple(dps) => List.flatten(List.map(bound_vars, dps))
  | Cons(dp1, dp2) => bound_vars(dp1) @ bound_vars(dp2)
  | ListLit(dps) => List.flatten(List.map(bound_vars, dps))
  | Ap(_, dp1) => bound_vars(dp1)
  };

let rec get_label: t => option((LabeledTuple.label, t)) =
  dp =>
    switch (dp |> term_of) {
    | Parens(dp) => get_label(dp)
    | TupLabel({term: Label(name), _}, t') => Some((name, t'))
    | _ => None
    };

open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type term =
  | Invalid(string)
  | EmptyHole
  // TODO: Multihole
  | Wild
  | Int(int)
  | Float(float)
  | Bool(bool)
  | String(string)
  // TODO: Remove Triv from UPat
  | ListLit(list(t))
  | Constructor(string)
  | Cons(t, t)
  | Var(Var.t)
  | Tuple(list(t))
  // TODO: parens
  | Ap(t, t)
  // TODO: Add Type Annotations???
  // TODO: Work out what to do with invalids
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | ExpandingKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | BadConstructor(MetaVar.t, MetaVarInst.t, string)
and t = {
  ids: list(Id.t),
  copied: bool,
  term,
};

let rep_id = ({ids, _}) => List.hd(ids);
let term_of = ({term, _}) => term;
let fast_copy = (id, {term, _}) => {ids: [id], term, copied: true};
// All children of term must have expression-unique ids.
let unwrap = ({ids, term, copied}) => (term, term => {ids, term, copied});
let fresh = term => {
  {ids: [Id.mk()], copied: false, term};
};

/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (x: Var.t, dp: t): bool =>
  switch (dp |> term_of) {
  | EmptyHole
  | NonEmptyHole(_, _, _, _)
  | Wild
  | Invalid(_)
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
  | ListLit(d_list) =>
    let new_list = List.map(binds_var(x), d_list);
    List.fold_left((||), false, new_list);
  | Ap(_, _) => false
  };

let rec bound_vars = (dp: t): list(Var.t) =>
  switch (dp |> term_of) {
  | EmptyHole
  | NonEmptyHole(_, _, _, _)
  | Wild
  | Invalid(_)
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
  | ListLit(dps) => List.flatten(List.map(bound_vars, dps))
  | Ap(_, dp1) => bound_vars(dp1)
  };

let get_var = (pat: t) => {
  switch (pat |> term_of) {
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
  | EmptyHole
  | NonEmptyHole(_)
  | ExpandingKeyword(_)
  | Invalid(_)
  | BadConstructor(_)
  | Ap(_) => None
  };
};

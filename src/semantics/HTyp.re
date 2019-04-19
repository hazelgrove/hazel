open GeneralUtil;

type idx = int; /* we use de Bruijn indices */

/* types with holes */
[@deriving sexp]
type t =
  | TVar(idx, Var.t) /* bound type variables */ 
  | TVarHole(Var.t) /* free type variables */
  | Hole
  | Unit
  | Num
  | Bool
  | Arrow(t, t)
  | Prod(t, t)
  | Sum(t, t)
  | List(t)
  | Forall(Var.t, t)
  /* the following two don't cause the de Bruijn index to increment */
  | ForallBindingHole(MetaVar.t, t) /* forall _, ty */ 
  | ForallShadowErr(Var.t, t)
;

/* # Substitution */

let rec shift_free_vars' = (ty, shift_by, binders_seen) => 
  switch (ty) {
  | TVar(i, x) => 
    if (i >= binders_seen) {
      TVar(i + shift_by, x)
    } else {
      ty
    }
  | TVarHole(_)
  | Hole
  | Unit
  | Num
  | Bool => ty
  | Arrow(ty1, ty2) => 
    Arrow(
      shift_free_vars'(ty1, shift_by, binders_seen),
      shift_free_vars'(ty2, shift_by, binders_seen))
  | Prod(ty1, ty2) => 
    Prod(
      shift_free_vars'(ty1, shift_by, binders_seen),
      shift_free_vars'(ty2, shift_by, binders_seen))
  | Sum(ty1, ty2) => 
    Sum(
      shift_free_vars'(ty1, shift_by, binders_seen),
      shift_free_vars'(ty2, shift_by, binders_seen))
  | List(ty1) => 
    List(shift_free_vars'(ty1, shift_by, binders_seen))
  | Forall(a, ty1) => 
    Forall(a, shift_free_vars'(ty1, shift_by, binders_seen + 1))
  | ForallBindingHole(u, ty1) => 
    ForallBindingHole(u, shift_free_vars'(ty1, shift_by, binders_seen))
  | ForallShadowErr(x, ty1) => 
    ForallShadowErr(x, shift_free_vars'(ty1, shift_by, binders_seen))
  };
  
let shift_free_vars = (ty, shift_by) => shift_free_vars'(ty, shift_by, 0);

let rec subst' = (ty, binders_seen, ty') => 
  switch (ty') {
  | TVar(i, _) => 
    if (i == binders_seen) {
      shift_free_vars(ty, binders_seen)
    } else {
      ty'
    }
  | TVarHole(_) 
  | Hole
  | Unit
  | Num
  | Bool => ty'
  | Arrow(ty1, ty2) => 
    Arrow(subst'(ty, binders_seen, ty1),
          subst'(ty, binders_seen, ty2))
  | Prod(ty1, ty2) => 
    Prod(subst'(ty, binders_seen, ty1),
         subst'(ty, binders_seen, ty2))
  | Sum(ty1, ty2) => 
    Sum(subst'(ty, binders_seen, ty1),
         subst'(ty, binders_seen, ty2))
  | List(ty1) => List(subst'(ty, binders_seen, ty1))
  | Forall(a, ty1) => 
    Forall(a, subst'(ty, binders_seen + 1, ty1))  
  | ForallBindingHole(u, ty1) => 
    ForallBindingHole(u, subst'(ty, binders_seen, ty1))
  | ForallShadowErr(x, ty1) => 
    ForallShadowErr(x, subst'(ty, binders_seen, ty1))
  };

/* capture-avoiding substitution of ty' for variable 0 in ty */
let subst = (ty', ty) => subst'(ty', 0, ty);

/* # type equivalence */
let rec equiv = (ty1, ty2) =>
  switch (ty1, ty2) {
  | (TVar(i, _), TVar(j, _)) => i == j
  | (TVar(_, _), _) => false
  | (TVarHole(t1), TVarHole(t2)) => Var.eq(t1, t2)
  | (TVarHole(_), _) => false
  | (Hole, Hole) => true
  | (Hole, _) => false
  | (Unit, Unit) => true
  | (Unit, _) => false
  | (Num, Num) => true
  | (Num, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) => 
    equiv(ty1, ty1') && equiv(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Prod(ty1, ty2), Prod(ty1', ty2')) => 
    equiv(ty1, ty1') && equiv(ty2, ty2')
  | (Prod(_, _), _) => false
  | (Sum(ty1, ty2), Sum(ty1', ty2')) => 
    equiv(ty1, ty1') && equiv(ty2, ty2')
  | (Sum(_, _), _) => false
  | (List(ty), List(ty')) => equiv(ty, ty')
  | (List(_), _) => false
  | (Forall(_, ty1), Forall(_, ty2)) => equiv(ty1, ty2)  
  | (Forall(_, _), _) => false
  /* TODO: how should Forall interact with the error forms in terms of equivalence? */
  };

/* # type consistency */
let rec consistent = (x, y) =>
  switch (x, y) {
  | (Hole, _)
  | (_, Hole) => true
  | (Unit, Unit) => true
  | (Unit, _) => false
  | (Num, Num) => true
  | (Num, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Prod(ty1, ty2), Prod(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Prod(_, _), _) => false
  | (Sum(_, _), _) => false
  | (List(ty), List(ty')) => consistent(ty, ty')
  | (List(_), _) => false
  };

let inconsistent = (ty1, ty2) => !consistent(ty1, ty2);

/* matched arrow types */
let matched_arrow =
  fun
  | Hole => Some((Hole, Hole))
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None;

let has_matched_arrow =
  fun
  | Hole => true
  | Arrow(_, _) => true
  | _ => false;

/* matched product types */
let matched_prod =
  fun
  | Hole => Some((Hole, Hole))
  | Prod(ty1, ty2) => Some((ty1, ty2))
  | _ => None;

let has_matched_prod =
  fun
  | Hole => true
  | Prod(_, _) => true
  | _ => false;

let rec get_tuple = (ty1: t, ty2: t): ListMinTwo.t(t) =>
  switch (ty2) {
  | Prod(ty21, ty22) => ListMinTwo.Cons(ty1, get_tuple(ty21, ty22))
  | _ => ListMinTwo.Pair(ty1, ty2)
  };

let rec make_tuple = (tys: ListMinTwo.t(t)): t =>
  switch (tys) {
  | Pair(ty1, ty2) => Prod(ty1, ty2)
  | Cons(ty1, tys) => Prod(ty1, make_tuple(tys))
  };

let rec zip_with_skels =
        (skels: ListMinTwo.t('a), types: ListMinTwo.t(t))
        : (ListMinTwo.t(('a, t)), list('a)) =>
  switch (skels, types) {
  | (Pair(skel1, skel2), Pair(ty1, ty2)) => (
      Pair((skel1, ty1), (skel2, ty2)),
      [],
    )
  | (Cons(skel1, skels), Pair(ty1, ty2)) =>
    let (skel2, remainder) =
      switch (skels) {
      | Pair(s1, s2) => (s1, [s2])
      | Cons(s, ss) => (s, ListMinTwo.to_list(ss))
      };
    (Pair((skel1, ty1), (skel2, ty2)), remainder);
  | (Pair(skel1, skel2), Cons(ty1, tys)) =>
    let ty2 =
      switch (tys) {
      | Pair(t, _)
      | Cons(t, _) => t
      };
    (Pair((skel1, ty1), (skel2, ty2)), []);
  | (Cons(skel, skels), Cons(ty, tys)) =>
    let (tail, remainder) = zip_with_skels(skels, tys);
    (Cons((skel, ty), tail), remainder);
  };

/* matched sum types */
let matched_sum =
  fun
  | Hole => Some((Hole, Hole))
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None;

let has_matched_sum =
  fun
  | Hole => true
  | Sum(_, _) => true
  | _ => false;

/* matched list types */
let matched_list =
  fun
  | Hole => Some(Hole)
  | List(ty) => Some(ty)
  | _ => None;

let has_matched_list =
  fun
  | Hole => true
  | List(_) => true
  | _ => false;

/* complete (i.e. does not have any holes) */
let rec complete =
  fun
  | Hole => false
  | Unit => true
  | Num => true
  | Bool => true
  | Arrow(ty1, ty2) =>
    if (complete(ty1)) {
      complete(ty2);
    } else {
      false;
    }
  | Prod(ty1, ty2) =>
    if (complete(ty1)) {
      complete(ty2);
    } else {
      false;
    }
  | Sum(ty1, ty2) =>
    if (complete(ty1)) {
      complete(ty2);
    } else {
      false;
    }
  | List(ty) => complete(ty);

let rec join = (ty1, ty2) =>
  switch (ty1, ty2) {
  | (_, Hole) => Some(ty1)
  | (Hole, _) => Some(ty2)
  | (Unit, Unit) => Some(ty1)
  | (Unit, _) => None
  | (Num, Num) => Some(ty1)
  | (Num, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    switch (join(ty1, ty1'), join(ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_, _), _) => None
  | (Prod(ty1, ty2), Prod(ty1', ty2')) =>
    switch (join(ty1, ty1'), join(ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Prod(ty1, ty2))
    | _ => None
    }
  | (Prod(_, _), _) => None
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    switch (join(ty1, ty1'), join(ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Sum(ty1, ty2))
    | _ => None
    }
  | (Sum(_, _), _) => None
  | (List(ty), List(ty')) =>
    switch (join(ty, ty')) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };

open GeneralUtil;

/* types with holes */
[@deriving sexp]
type t =
  | Hole
  | Unit
  | Num
  | Bool
  | String
  | Arrow(t, t)
  | Prod(t, t)
  | Sum(t, t)
  | List(t);

let rec num_tms = (ty: t): int =>
  switch (ty) {
  | Hole
  | Unit
  | Num
  | Bool
  | String
  | List(_) => 1
  | Arrow(ty1, ty2)
  | Prod(ty1, ty2)
  | Sum(ty1, ty2) => num_tms(ty1) + num_tms(ty2)
  };

/* eqity */
let rec eq = (ty1, ty2) =>
  switch (ty1, ty2) {
  | (Hole, Hole) => true
  | (Hole, _) => false
  | (Unit, Unit) => true
  | (Unit, _) => false
  | (Num, Num) => true
  | (Num, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (String, String) => true
  | (String, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) => eq(ty1, ty1') && eq(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Prod(ty1, ty2), Prod(ty1', ty2')) => eq(ty1, ty1') && eq(ty2, ty2')
  | (Prod(_, _), _) => false
  | (Sum(ty1, ty2), Sum(ty1', ty2')) => eq(ty1, ty1') && eq(ty2, ty2')
  | (Sum(_, _), _) => false
  | (List(ty), List(ty')) => eq(ty, ty')
  | (List(_), _) => false
  };

/* type consistency */
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
  | (String, String) => true
  | (String, _) => false
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
  | String => true
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
  | (String, String) => Some(ty1)
  | (String, _) => None
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

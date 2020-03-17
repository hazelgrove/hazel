/* types with holes */
[@deriving sexp]
type t =
  | Hole
  | Unit
  | Num
  | Bool
  | Arrow(t, t)
  | Prod(t, t)
  | Sum(t, t)
  | List(t);

// Prod(Num, Num)  ==  (Num, Num)
// (Prod(Num, Num), Bool)  ==  (Num, Num, Bool)

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
  | Arrow(_) => true
  | _ => false;

let rec get_prod_elements: t => list(t) =
  fun
  | Prod(ty1, ty2) => get_prod_elements(ty1) @ get_prod_elements(ty2)
  | _ as ty => [ty];

let rec make_tuple: list(t) => t =
  fun
  | [] => failwith("make_tuple: expected at least 1 element")
  | [ty] => ty
  | [ty, ...tys] => Prod(ty, make_tuple(tys));

/* matched sum types */
let matched_sum =
  fun
  | Hole => Some((Hole, Hole))
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None;

let has_matched_sum =
  fun
  | Hole => true
  | Sum(_) => true
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

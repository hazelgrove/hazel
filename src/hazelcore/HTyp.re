open Sexplib.Std;

[@deriving sexp]
type unknown_type_provenance =
  | UserGenerated(MetaVar.t)
  | SynPatternVar
  | Internal(internal_provenance)
and internal_provenance =
  // enumerate other base cases here...
  | Matched_arrow_L(hole_provenance)
  | Matched_arrow_R(hole_provenance)
  | Matched_sum_L(hole_provenance)
  | Matched_sum_R(hole_provenance)
  | Matched_prod_L(hole_provenance)
  | Matched_prod_R(hole_provenance)
  | Matched_list(hole_provenance);

/* types with holes */
[@deriving sexp]
type t =
  | Unknown(unknown_type_provenance)
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(t, t)
  | Prod(list(t))
  | List(t);

[@deriving sexp]
type join =
  | GLB
  | LUB;

type inf_constraint = (t, t);

let precedence_Prod = Operators_Typ.precedence(Prod);
let precedence_Arrow = Operators_Typ.precedence(Arrow);
let precedence_Sum = Operators_Typ.precedence(Sum);
let precedence_const = Operators_Typ.precedence_const;
let precedence = (ty: t): int =>
  switch (ty) {
  | Int
  | Float
  | Bool
  | Unknown(_)
  | Prod([])
  | List(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
    ListUtil.for_all2_opt(consistent, tys1, tys2)
  | (Prod(_), _) => false
  | (List(_), _) => false
  };

let inconsistent = (ty1, ty2) => !consistent(ty1, ty2);

let rec consistent_all = (types: list(t)): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    if (List.exists(inconsistent(hd), tl)) {
      false;
    } else {
      consistent_all(tl);
    }
  };

/* matched arrow types */
// let matched_arrow =
//   fun
//   | Unknown(prov) => Some((Unknown(prov), Unknown(prov)))
//   | Arrow(ty1, ty2) => Some((ty1, ty2))
//   | _ => None;

let get_prod_elements: t => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [ty];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

let matched_arrow: t => (option(t, t)) =
  (typ: t) =>
  let pair, _ = matched_arrow_inf(typ);
  pair;

let matched_arrow_inf: t => (option(t, t), list(inf_constraint)) =
  (typ: t) =>
  switch (typ) {
    | Unknown(prov) =>
      let unknown_lt = Unknown(var, Matched_hole_L(prov));
      let unknown_rt = Unknown(var, Matched_hole_R(prov));
      let pair = (unknown_lt, unknown_rt);
      let constraints = [(typ, Arrow(unknown_lt, unknown_rt))];
      (Some(pair), constraints)
    | Arrow(ty1, ty2) => (Some((ty1, ty2)), [])
    | _ => (None, [])
  };

let matched_sum: t => (option(t, t)) =
  (typ: t) =>
  let pair, _ = matched_sum_inf(typ);
  pair;

let matched_sum_inf: t => (option(t, t), list(inf_constraint)) =
  (typ: t) =>
  switch (typ) {
    | Hole(base, provenances) =>
      let hole_left = Hole(base, provenances @ (Matched_sum(L)));
      let hole_right = Hole(base, provenances @ (Matched_sum(R)));
      let pair = (hole_left, hole_right);
      let constraints = [(typ, Sum(hole_left, hole_right))];
      (Some(pair), constraints);
    | Sum(ty1, ty2) => (Some((ty1, ty2)), [])
    | _ => (None, [])
  };

let matched_list: t => (option(t, t)) =
  (typ: t) =>
  let pair, _ = matched_list_inf(typ);
  pair;

let matched_list_inf: t => (option(t), list(inf_constraint)) =
  (typ: t) =>
  switch (typ) {
    | Hole(base, provenances) =>
      let hole_elt = Hole(base, provenances @ Matched_list);
      let constraint = [(typ, List(hole_elt))];
      (Some(hole_elt), constraint);
    | List(ty_ls) => (Some(ty_ls), [])
    | _ => (None, [])
  };

let rec load_type_variable = (typ: t) => {
  switch (typ) {
  | Hole(id) =>
    InfVar.type_variable := InfVar.recent(id + 1, InfVar.type_variable^)
  | Bool
  | Int
  | Float => ()
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) =>
    load_type_variable(ty1);
    load_type_variable(ty2);
  | Prod(tys) => List.iter(load_type_variable, tys)
  | List(ty) => load_type_variable(typ)
  };
};

/* complete (i.e. does not have any holes) */
let rec complete =
  fun
  | Hole(_) => false
  | Int => true
  | Float => true
  | Bool => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) => complete(ty1) && complete(ty2)
  | Prod(tys) => tys |> List.for_all(complete)
  | List(ty) => complete(ty);

let rec join = (j, ty1, ty2) =>
  switch (ty1, ty2) {
  | (_, Hole(_) as hole) =>
    switch (j) {
    | GLB => Some(hole)
    | LUB => Some(ty1)
    }
  | (Hole(_) as hole, _) =>
    switch (j) {
    | GLB => Some(hole)
/* matched sum types */
let matched_sum =
  fun
  | Unknown(prov) => Some((Unknown(prov), Unknown(prov)))
  | Sum(tyL, tyR) => Some((tyL, tyR))
  | _ => None;

/* matched list types */
let matched_list =
    | GLB => Some(Unknown(Internal))
    }
  | (Unknown(_), _) =>
    switch (j) {
    | GLB => Some(Unknown(Internal))
    | LUB => Some(ty2)
    }
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    switch (join(j, ty1, ty1'), join(j, ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    switch (join(j, ty1, ty1'), join(j, ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Sum(ty1, ty2))
    | _ => None
    }
  | (Sum(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.map2_opt(join(j), tys1, tys2)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    switch (join(j, ty, ty')) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };

let join_all = (j: join, types: list(t)): option(t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    if (!consistent_all(types)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => join(j, common_ty, ty)
          },
        Some(hd),
        tl,
      );
    }
  };
};

let is_unknown: t => bool =
  fun
  | Unknown(_) => true
  | _ => false;

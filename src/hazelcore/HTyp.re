open Sexplib.Std;

[@deriving sexp]
type unknown_type_provenance =
  | UserGenerated(MetaVar.t)
  | SynPatternVar
  | Internal(internal_provenance)
and internal_provenance =
  // enumerate other base cases here if applicable; try to avoid
  | Matched_arrow_L(unknown_type_provenance)
  | Matched_arrow_R(unknown_type_provenance)
  | Matched_sum_L(unknown_type_provenance)
  | Matched_sum_R(unknown_type_provenance)
  | Matched_prod_L(unknown_type_provenance)
  | Matched_prod_R(unknown_type_provenance)
  | Matched_list(unknown_type_provenance);

/**
   * option 1: use above and replace internals of andrew's code with usergen
   * option 2:
[@deriving sexp]
type unknown_type_provenance =
  | TypHole
  | SynPatternVar
  | Internal(internal_provenance)
and internal_provenance =
  // enumerate other base cases here if applicable; try to avoid
  | UserVisible(MetaVar.t)
  | Matched_arrow_L(unknown_type_provenance)
  | Matched_arrow_R(unknown_type_provenance)
  | Matched_sum_L(unknown_type_provenance)
  | Matched_sum_R(unknown_type_provenance)
  | Matched_prod_L(unknown_type_provenance)
  | Matched_prod_R(unknown_type_provenance)
  | Matched_list(unknown_type_provenance);
  **/

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
  };

let eq = (==);

let rec consistent = (x, y) =>
  switch (x, y) {
  //TODO(andrew): this throws immediately
  /*| (Unknown(SynPatternVar), _)
    | (_, Unknown(SynPatternVar)) =>
      failwith("HTyp.consistent: SynPatternVar")*/
  | (Unknown(_), _)
  | (_, Unknown(_)) => true
  | (Int, Int) => true
  | (Int, _) => false
  | (Float, Float) => true
  | (Float, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(_, _), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.for_all2_opt(consistent, tys1, tys2)
    |> Option.value(~default=false)
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => consistent(ty, ty')
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

let get_prod_elements: t => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [ty];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

// let rec load_type_variable = (typ: t) => {
//   switch (typ) {
//   | Unknown(id) =>
//     InfVar.type_variable := InfVar.recent(id + 1, InfVar.type_variable^)
//   | Bool
//   | Int
//   | Float => ()
//   | Arrow(ty1, ty2)
//   | Sum(ty1, ty2) =>
//     load_type_variable(ty1);
//     load_type_variable(ty2);
//   | Prod(tys) => List.iter(load_type_variable, tys)
//   | List(ty) => load_type_variable(typ)
//   };
// };

/* complete (i.e. does not have any holes) */
let rec complete =
  fun
  | Unknown(_) => false
  | Int => true
  | Float => true
  | Bool => true
  | Arrow(ty1, ty2)
  | Sum(ty1, ty2) => complete(ty1) && complete(ty2)
  | Prod(tys) => tys |> List.for_all(complete)
  | List(ty) => complete(ty);

let rec join = (j, ty1, ty2) =>
  switch (ty1, ty2) {
  | (_, Unknown(_) as hole) =>
    switch (j) {
    | GLB => Some(hole)
    | LUB => Some(ty1)
    }
  | (Unknown(_) as hole, _) =>
    switch (j) {
    | GLB => Some(hole)
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

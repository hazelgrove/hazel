open Sexplib.Std;

/* types with holes */
[@deriving sexp]
type t =
  | Hole(MetaVar.t, (list(MetaVar.t)), MetaVarGen.t) // UHHole [1] -> HHole [1.1.1.1.2.1], HHole [1.1.1.1.2.2]
  // UHHole [1] used in (HHole [1.1] , HHole [1.2]) ; 
  // UHHole [1] used in (HHole [1.3] , HHole [1.4]) ;
  // HHole [1.3] used in (HHole [1.3.1], HHole [1.3.2])
  // ^ Hole(1, [3], *0->2*) used in (Hole(1, [3, 1], 0), Hole(1, [3, 2], 0))
  // HHole [1.3] used in (HHole [1.3.3], HHole [1.3.4])
  // ^ Hole(1, [3], *2->4*) used in (Hole(1, [3, 3], 0), Hole(1, [3, 4], 0))
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
  | Hole(_)
  | Prod([])
  | List(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
  };

/* equality
   At the moment, this coincides with default equality,
   but this will change when polymorphic types are implemented */
let eq = (==);

/* type consistency */
let rec consistent = (x, y) =>
  switch (x, y) {
  | (Hole(_), _)
  | (_, Hole(_)) => true
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

let matched_arrow: t => (option(t, t)) =
  (typ: t) =>
  let pair, _ = matched_arrow_inf(typ);
  pair;

let matched_arrow_inf: t => (option(t, t), list(inf_constraint)) =
  (typ: t) =>
  switch (typ) {
    | Hole(base, tl, u_gen) =>
      let (id_in, u_gen) = MetaVarGen.next(u_gen);
      let (id_out, u_gen) = MetaVarGen.next(u_gen);
      let hole_in = Hole(base, tl @ id_in, MetaVarGen.init);
      let hole_out = Hole(base, tl @ id_out, MetaVarGen.init);
      let pair = (hole_in, hole_out);
      let constraint = [(typ, Arrow(hole_in, hole_out))];
      (Some(pair), constraint);
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
    | Hole(base, tl, u_gen) =>
      let (id_in, u_gen) = MetaVarGen.next(u_gen);
      let (id_out, u_gen) = MetaVarGen.next(u_gen);
      let hole_in = Hole(base, tl @ id_in, MetaVarGen.init);
      let hole_out = Hole(base, tl @ id_out, MetaVarGen.init);
      let pair = (hole_in, hole_out);
      let constraint = [(typ, Sum(hole_in, hole_out))];
      (Some(pair), constraint);
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
    | Hole(base, tl, u_gen) =>
      let (id_elt, u_gen) = MetaVarGen.next(u_gen);
      let hole_elt = Hole(base, tl @ id_in, MetaVarGen.init);
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

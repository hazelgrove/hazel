/** Types with holes */
include HTypCore;

type join =
  | GLB
  | LUB;

/** Kind equivalence

Kinds are equivalent if they are equal modulo singletons and singleton types
are all equivalent.
*/
let rec equivalent_kind = (k: Kind.t, k': Kind.t, ctx: TyCtx.t): bool =>
  switch (k, k') {
  | (KHole, KHole)
  | (Type, Type) => true
  | (KHole | Type, _) => false
  | (Singleton(Singleton(_, ty), _), Singleton(_, ty'))
  | (Singleton(_, ty), Singleton(Singleton(_, ty'), _))
  | (Singleton(_, ty), Singleton(_, ty')) => ctx |> equivalent(ty, ty')
  | (Singleton(_), _) => false
  }

/** Type equivalence */
and equivalent = (ty: t, ty': t, ctx: TyCtx.t): bool =>
  switch (ty, ty') {
  /* bound type variables are equivalent to themselves */
  | (TyVar(i, _), TyVar(i', _)) =>
    // need: symm, trans, singequiv, var
    Index.equal(i, i') && TyCtx.has_var_index(i, ctx)
  | (TyVar(_), _) => false
  /* type variable holes of known kind are equivalent to themselves */
  | (TyVarHole(reason, u, name), TyVarHole(reason', u', name')) =>
    reason == reason'
    && MetaVar.eq(u, u')
    && TyVar.Name.equal(name, name')
    && TyCtx.has_hole(u, ctx)
  | (TyVarHole(_), _) => false
  /* empty type holes of known kind are equivalent to themselves */
  | (Hole(u), Hole(u')) => u == u' && TyCtx.has_hole(u, ctx)
  | (Hole(_), _) => false
  /* base types are equivalent to themselves */
  | (Int | Float | Bool, _) => ty == ty'
  /* composite types are equivalent when they are componentwise equivalent */
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    ctx |> equivalent(ty1, ty1') && ctx |> equivalent(ty2, ty2')
  | (Arrow(_) | Sum(_), _) => false
  | (Prod(tys), Prod(tys')) =>
    List.for_all2((ty, ty') => ctx |> equivalent(ty, ty'), tys, tys')
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => ctx |> equivalent(ty, ty')
  | (List(_), _) => false
  };

/** Type consistency

Types are consistent if they are equivalent modulo holes.
*/
let rec consistent = (ty: t, ty': t, ctx: TyCtx.t) =>
  switch (ty, ty') {
  /* holes are consistent with anything */
  | (TyVarHole(_), _)
  | (_, TyVarHole(_))
  | (Hole(_), _)
  | (_, Hole(_)) => true
  /* type variables are consistent with equivalent types */
  | (TyVar(_), _)
  | (_, TyVar(_)) => ctx |> equivalent(ty, ty')
  | (Int | Float | Bool, _) => ty == ty'
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    ctx |> consistent(ty1, ty1') && ctx |> consistent(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(_, _), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    let check_component = (ty1, ty2) => ctx |> consistent(ty1, ty2);
    ListUtil.for_all2_opt(check_component, tys1, tys2)
    |> Option.value(~default=false);
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => ctx |> consistent(ty, ty')
  | (List(_), _) => false
  };

let inconsistent = (ty1: t, ty2: t, ctx: TyCtx.t) =>
  !(ctx |> consistent(ty1, ty2));

let rec consistent_all = (types: list(t), ctx: TyCtx.t): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    !List.exists(x => ctx |> inconsistent(hd, x), tl)
    || consistent_all(tl, ctx)
  };

let rec join = (j: join, ty1: t, ty2: t, ctx: TyCtx.t): option(t) => {
  switch (ty1, ty2) {
  | (TyVarHole(_, u, _), TyVarHole(_, u', _)) =>
    Some(Hole(MetaVar.join(u, u')))
  | (_, Hole(_))
  | (_, TyVarHole(_)) =>
    switch (j) {
    | GLB => Some(Hole(0))
    | LUB => Some(ty1)
    }
  | (Hole(_), _)
  | (TyVarHole(_), _) =>
    switch (j) {
    | GLB => Some(Hole(0))
    | LUB => Some(ty2)
    }
  | (TyVar(i, _), _) =>
    open OptUtil.Syntax;
    let* (_, k) = ctx |> TyCtx.var_binding(i);
    switch (k) {
    | Singleton(_, ty) => ctx |> join(j, ty, ty2)
    | KHole => ctx |> join(j, Hole(0), ty2)
    | Type => failwith("impossible for bounded type variables (currently) 1")
    };
  | (_, TyVar(i, _)) =>
    open OptUtil.Syntax;
    let* (_, k) = ctx |> TyCtx.var_binding(i);
    switch (k) {
    | Kind.Singleton(_, ty) => ctx |> join(j, ty1, ty)
    | KHole => ctx |> join(j, ty1, Hole(0))
    | Type => failwith("impossible for bounded type variables (currently) 2")
    };
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    open OptUtil.Syntax;
    let* ty1 = ctx |> join(j, ty1, ty1');
    let+ ty2 = ctx |> join(j, ty2, ty2');
    Arrow(ty1, ty2);
  | (Arrow(_), _) => None
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    open OptUtil.Syntax;
    let* ty1 = ctx |> join(j, ty1, ty1');
    let+ ty2 = ctx |> join(j, ty2, ty2');
    Sum(ty1, ty2);
  | (Sum(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.map2_opt((ty1, ty2) => ctx |> join(j, ty1, ty2), tys1, tys2)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    open OptUtil.Syntax;
    let+ ty = ctx |> join(j, ty, ty');
    List(ty);
  | (List(_), _) => None
  };
};

let join_all = (j: join, types: list(t), ctx: TyCtx.t): option(t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    if (!consistent_all(types, ctx)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => ctx |> join(j, common_ty, ty)
          },
        Some(hd),
        tl,
      );
    }
  };
};

let new_Hole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (Hole(u), u_gen);
};

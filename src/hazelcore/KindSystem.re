open Sexplib.Std;

module rec HTyp: {
  [@deriving sexp]
  type t('idx) =
    | Hole
    | Int
    | Float
    | Bool
    | Arrow(t('idx), t('idx))
    | Sum(t('idx), t('idx))
    | Prod(list(t('idx)))
    | List(t('idx))
    | TyVar(Index.t('idx), TyVar.t)
    | TyVarHole(TyVarErrStatus.HoleReason.t, MetaVar.t, TyVar.t);

  [@deriving sexp]
  type rel = t(Index.relative);

  [@deriving sexp]
  type abs = t(Index.absolute);

  let to_rel: (~offset: int=?, abs) => rel;
  let to_abs: (~offset: int=?, rel) => abs;

  let equal: (t('idx), t('idx)) => bool;
  let equivalent: (Context.t, abs, abs) => bool;

  let subst_tyvar: (t('idx), Index.t('idx), t('idx)) => t('idx);
  let subst_tyvars: (t('idx), list((Index.t('idx), t('idx)))) => t('idx);
} = {
  [@deriving sexp]
  type t('idx) =
    | Hole
    | Int
    | Float
    | Bool
    | Arrow(t('idx), t('idx))
    | Sum(t('idx), t('idx))
    | Prod(list(t('idx)))
    | List(t('idx))
    | TyVar(Index.t('idx), TyVar.t)
    | TyVarHole(TyVarErrStatus.HoleReason.t, MetaVar.t, TyVar.t);

  [@deriving sexp]
  type rel = t(Index.relative);

  [@deriving sexp]
  type abs = t(Index.absolute);

  let rec to_rel = (~offset: int=0, ty: abs): rel =>
    switch (ty) {
    | TyVar(i, name) => TyVar(Index.Abs.to_rel(~offset, i), name)
    | TyVarHole(reason, u, name) => TyVarHole(reason, u, name)
    | Hole => Hole
    | Int => Int
    | Float => Float
    | Bool => Bool
    | Arrow(ty1, ty2) => Arrow(to_rel(~offset, ty1), to_rel(~offset, ty2))
    | Sum(tyL, tyR) => Sum(to_rel(~offset, tyL), to_rel(~offset, tyR))
    | Prod(tys) => Prod(List.map(to_rel(~offset), tys))
    | List(ty) => List(to_rel(~offset, ty))
    };

  let rec to_abs = (~offset: int=0, ty: rel): abs =>
    switch (ty) {
    | TyVar(i, name) => TyVar(Index.Rel.to_abs(~offset, i), name)
    | TyVarHole(reason, u, name) => TyVarHole(reason, u, name)
    | Hole => Hole
    | Int => Int
    | Float => Float
    | Bool => Bool
    | Arrow(ty1, ty2) => Arrow(to_abs(~offset, ty1), to_abs(~offset, ty2))
    | Sum(tyL, tyR) => Sum(to_abs(~offset, tyL), to_abs(~offset, tyR))
    | Prod(tys) => Prod(List.map(to_abs(~offset), tys))
    | List(ty) => List(to_abs(~offset, ty))
    };

  let equal: (t('idx), t('idx)) => bool = (==);

  let rec equivalent = (ctx: Context.t, ty: abs, ty': abs): bool =>
    switch (ty, ty') {
    | (TyVar(i, _), TyVar(i', _)) =>
      switch (Context.tyvar_kind(ctx, i), Context.tyvar_kind(ctx, i')) {
      | (Some(k), Some(k')) =>
        switch (k, k') {
        | (Hole | Type, Hole | Type) => Index.equal(i, i')
        | (S(ty1), S(ty1')) => equivalent(ctx, ty1, ty1')
        | (S(_), _)
        | (_, S(_)) => false
        }
      | (None, _)
      | (_, None) => false
      }
    | (TyVar(_), _) => false
    | (TyVarHole(_, u, _), TyVarHole(_, u', _)) => MetaVar.eq(u, u')
    | (TyVarHole(_, _, _), _) => false
    | (Hole | Int | Float | Bool, _) => ty == ty'
    | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
    | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
      equivalent(ctx, ty1, ty1') && equivalent(ctx, ty2, ty2')
    | (Arrow(_, _), _) => false
    | (Sum(_, _), _) => false
    | (Prod(tys1), Prod(tys2)) =>
      List.for_all2(equivalent(ctx), tys1, tys2)
    | (Prod(_), _) => false
    | (List(ty), List(ty')) => equivalent(ctx, ty, ty')
    | (List(_), _) => false
    };

  let rec subst_tyvar =
          (ty: t('idx), idx: Index.t('idx), new_ty: t('idx)): t('idx) => {
    let go = ty1 => subst_tyvar(ty1, idx, new_ty);
    switch (ty) {
    | TyVar(idx', _) => Index.equal(idx, idx') ? new_ty : ty
    | TyVarHole(_)
    | Hole
    | Int
    | Float
    | Bool => ty
    | Arrow(ty1, ty2) => Arrow(go(ty1), go(ty2))
    | Sum(tyL, tyR) => Sum(go(tyL), go(tyR))
    | Prod(tys) => Prod(List.map(go, tys))
    | List(ty) => List(go(ty))
    };
  };

  let subst_tyvars =
      (ty: t('idx), tyvars: list((Index.t('idx), t('idx)))): t('idx) =>
    List.fold_left(
      (ty, (idx, ty_idx)) => subst_tyvar(ty, idx, ty_idx),
      ty,
      tyvars,
    );
}

and Kind: {
  [@deriving sexp]
  type t('idx) =
    | Hole
    | Type
    | S(HTyp.t('idx));

  [@deriving sexp]
  type rel = t(Index.relative);

  [@deriving sexp]
  type abs = t(Index.absolute);

  let to_rel: (~offset: int=?, abs) => rel;
  let to_abs: (~offset: int=?, rel) => abs;

  let to_htyp: t('idx) => HTyp.t('idx);
} = {
  [@deriving sexp]
  type t('idx) =
    | Hole
    | Type
    | S(HTyp.t('idx));

  [@deriving sexp]
  type rel = t(Index.relative);

  [@deriving sexp]
  type abs = t(Index.absolute);

  let to_rel = (~offset: int=0, k: abs): rel =>
    switch (k) {
    | Hole => Hole
    | Type => Type
    | S(ty) => S(HTyp.to_rel(~offset, ty))
    };

  let to_abs = (~offset: int=0, k: rel): abs =>
    switch (k) {
    | Hole => Hole
    | Type => Type
    | S(ty) => S(HTyp.to_abs(~offset, ty))
    };

  let to_htyp: t('idx) => HTyp.t('idx) =
    fun
    | Hole
    | Type => Hole
    | S(ty) => ty;
}

and Context: {
  [@deriving sexp]
  type binding =
    | VarBinding(Var.t, HTyp.rel)
    | TyVarBinding(TyVar.t, Kind.rel);

  [@deriving sexp]
  type t = list(binding);

  let nth_var_binding: (t, int) => option((int, Var.t, HTyp.rel));
  let nth_tyvar_binding: (t, int) => option((TyVar.t, Kind.rel));
  let first_tyvar_binding:
    (t, (TyVar.t, Kind.rel) => bool) => option((int, TyVar.t, Kind.rel));

  let tyvar_kind: (t, Index.Abs.t) => option(Kind.abs);
} = {
  [@deriving sexp]
  type binding =
    | VarBinding(Var.t, HTyp.rel)
    | TyVarBinding(TyVar.t, Kind.rel);

  [@deriving sexp]
  type t = list(binding);

  /* Returns the n-th expression variable bindings of the given context. */
  let nth_var_binding = (ctx: t, n: int): option((int, Var.t, HTyp.rel)) => {
    let rec go = (i, n, ctx) =>
      switch (n, ctx) {
      | (0, [VarBinding(x, ty), ..._]) => Some((i, x, ty))
      | (_, [VarBinding(_), ...ctx']) => go(i, n - 1, ctx')
      | (_, [TyVarBinding(_), ...ctx']) => go(i + 1, n, ctx')
      | (_, []) => None
      };
    go(0, n, ctx);
  };

  /* Returns the n-th type variable binding of the given context. */
  let rec nth_tyvar_binding = (ctx: t, n: int): option((TyVar.t, Kind.rel)) =>
    switch (n, ctx) {
    | (0, [TyVarBinding(t, k), ..._]) => Some((t, k))
    | (_, [TyVarBinding(_), ...ctx']) => nth_tyvar_binding(ctx', n - 1)
    | (_, [VarBinding(_), ...ctx']) => nth_tyvar_binding(ctx', n)
    | (_, []) => None
    };

  /* Returns the first type variable binding satisfying the given function. */
  let first_tyvar_binding =
      (ctx: t, f: (TyVar.t, Kind.t('idx)) => bool)
      : option((int, TyVar.t, Kind.t('idx))) => {
    let rec go = (n, ctx) =>
      switch (ctx) {
      | [TyVarBinding(t, k), ...ctx'] =>
        f(t, k) ? Some((n, t, k)) : go(n + 1, ctx')
      | [VarBinding(_), ...ctx'] => go(n, ctx')
      | [] => None
      };
    go(0, ctx);
  };

  let tyvar_kind = (ctx: t, idx: Index.Abs.t): option(Kind.abs) => {
    open OptUtil.Syntax;
    let i = Index.Abs.to_int(idx);
    let+ (_, k) = nth_tyvar_binding(ctx, i);
    Kind.to_abs(~offset=i, k);
  };
};

include TypBase.Kind;
module Ctx = TypBase.Ctx;
module Typ = TypBase.Typ;

let extend: (Ctx.entry, Ctx.t) => Ctx.t = List.cons;

let add_alias = (ctx: Ctx.t, name: Token.t, id: Id.t, ty: Typ.t): Ctx.t =>
  extend(TVarEntry({name, id, kind: Singleton(ty)}), ctx);

let add_abstract = (ctx: Ctx.t, name: Token.t, id: Id.t): Ctx.t =>
  extend(TVarEntry({name, id, kind: Abstract}), ctx);

let lookup_tvar = (ctx: Ctx.t, t: Token.t): option(t) =>
  List.find_map(
    fun
    | Ctx.TVarEntry({name, kind, _}) when name == t => Some(kind)
    | _ => None,
    ctx,
  );

let lookup_alias = (ctx: Ctx.t, t: Token.t): option(Typ.t) =>
  switch (lookup_tvar(ctx, t)) {
  | Some(Singleton(ty)) => Some(ty)
  | Some(Abstract)
  | None => None
  };

let is_alias = (ctx: Ctx.t, name: Token.t): bool =>
  switch (lookup_alias(ctx, name)) {
  | Some(_) => true
  | None => false
  };

let rec normalize_shallow = (ctx: Ctx.t, ty: Typ.t): Typ.t =>
  switch (ty) {
  | Var(x) =>
    switch (lookup_alias(ctx, x)) {
    | Some(ty) => normalize_shallow(ctx, ty)
    | None => ty
    }
  | _ => ty
  };

let rec normalize = (ctx: Ctx.t, ty: Typ.t): Typ.t => {
  switch (ty) {
  | Var(x) =>
    switch (lookup_alias(ctx, x)) {
    | Some(ty) => normalize(ctx, ty)
    | None => ty
    }
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String => ty
  | List(t) => List(normalize(ctx, t))
  | Arrow(t1, t2) => Arrow(normalize(ctx, t1), normalize(ctx, t2))
  | Prod(ts) => Prod(List.map(normalize(ctx), ts))
  | Sum(ts) => Sum(Util.TagMap.map(Option.map(normalize(ctx)), ts))
  | Rec(x, ty) =>
    /* NOTE: Fake -1 id below is a hack, but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec(x, normalize(add_abstract(ctx, x, -1), ty))
  };
};

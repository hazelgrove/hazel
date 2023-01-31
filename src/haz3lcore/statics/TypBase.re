open Sexplib.Std;

module rec Ctx: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Token.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | TVarEntry({
        name: Token.t,
        id: Id.t,
        kind: Kind.t,
      })
    | TagEntry(var_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_item = {
    id: Id.t,
    mode: Typ.mode,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_entry = list(co_item);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co = VarMap.t_(co_entry);

  let empty: t;
  let extend: (entry, t) => t;
  let get_id: entry => int;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Token.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | TVarEntry({
        name: Token.t,
        id: Id.t,
        kind: Kind.t,
      })
    | TagEntry(var_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_item = {
    id: Id.t,
    mode: Typ.mode,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_entry = list(co_item);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co = VarMap.t_(co_entry);

  let get_id: entry => int =
    fun
    | VarEntry({id, _})
    | TagEntry({id, _})
    | TVarEntry({id, _}) => id;
  let empty: t = VarMap.empty;
  let extend: (entry, t) => t = List.cons;
}
and Kind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;

  let lookup_alias: (Ctx.t, Token.t) => option(Typ.t);
  let is_alias: (Ctx.t, Token.t) => bool;
  let add_alias: (Ctx.t, Token.t, Id.t, Typ.t) => Ctx.t;
  let normalize: (Ctx.t, Typ.t) => Typ.t;
  let normalize_shallow: (Ctx.t, Typ.t) => Typ.t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;

  let lookup_tvar = (ctx: Ctx.t, t: Token.t) =>
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

  let add_alias = (ctx: Ctx.t, name: Token.t, id: Id.t, ty: Typ.t): Ctx.t =>
    Ctx.extend(TVarEntry({name, id, kind: Singleton(ty)}), ctx);

  let add_abstract = (ctx: Ctx.t, name: Token.t, id: Id.t): Ctx.t =>
    Ctx.extend(TVarEntry({name, id, kind: Abstract}), ctx);

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
};

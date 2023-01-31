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
    | Singleton(Typ.t);

  let lookup_tvar: (Ctx.t, Token.t) => option(t);
  let is_tvar: (Ctx.t, Token.t) => bool;
  let add_singleton: (Ctx.t, Token.t, Id.t, Typ.t) => Ctx.t;
  let normalize: (Ctx.t, Typ.t) => Typ.t;
  let normalize_shallow: (Ctx.t, Typ.t) => Typ.t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t);

  let lookup_tvar = (ctx: Ctx.t, t: Token.t) =>
    List.find_map(
      fun
      | Ctx.TVarEntry({name, kind, _}) when name == t => Some(kind)
      | _ => None,
      ctx,
    );

  let remove_tvar = (ctx: Ctx.t, t: Token.t): Ctx.t =>
    List.filter(
      fun
      | Ctx.TVarEntry({name, _}) => name != t
      | _ => true,
      ctx,
    );

  let is_tvar = (ctx: Ctx.t, name: Token.t) =>
    switch (lookup_tvar(ctx, name)) {
    | Some(_) => true
    | None => false
    };

  let add_singleton = (ctx: Ctx.t, name: Token.t, id: Id.t, ty: Typ.t): Ctx.t =>
    Ctx.extend(TVarEntry({name, id, kind: Singleton(ty)}), ctx);

  let rec normalize_shallow = (ctx: Ctx.t, ty: Typ.t): Typ.t =>
    switch (ty) {
    | Var(x) =>
      switch (lookup_tvar(ctx, x)) {
      | Some(Singleton(ty)) => normalize_shallow(ctx, ty)
      | _ => ty
      }
    | _ => ty
    };
  let rec normalize = (ctx: Ctx.t, ty: Typ.t): Typ.t => {
    switch (ty) {
    | Var(x) =>
      switch (lookup_tvar(ctx, x)) {
      | Some(Singleton(ty)) => normalize(ctx, ty)
      | None => ty
      }
    | Unknown(_)
    | Int
    | Float
    | Bool
    | String => ty
    | List(t) => List(normalize(ctx, t))
    | Arrow(t1, t2) => Arrow(normalize(ctx, t1), normalize(ctx, t2))
    | Sum(ts) => Sum(Util.TagMap.map(Option.map(normalize(ctx)), ts))
    | Rec(x, ty) => Rec(x, normalize(remove_tvar(ctx, x), ty))
    | Prod(ts) => Prod(List.map(normalize(ctx), ts))
    };
  };
};

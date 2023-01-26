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

  let lookup_tvar: (t, Token.t) => option(Kind.t);
  let is_tvar: (t, Token.t) => bool;
  let resolve_typ: (t, Typ.t) => option(Typ.t);
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

  let lookup_tvar = (ctx: t, t: Token.t) =>
    List.find_map(
      fun
      | TVarEntry({name, kind, _}) when name == t => Some(kind)
      | _ => None,
      ctx,
    );

  let rec resolve_typ = (ctx: t, ty: Typ.t): option(Typ.t) =>
    switch (ty) {
    | Var(x) =>
      switch (Ctx.lookup_tvar(ctx, x)) {
      | Some(Singleton(ty)) => resolve_typ(ctx, ty)
      | _ => Some(ty)
      }
    | _ => Some(ty)
    };

  let is_tvar = (ctx: t, name: Token.t) =>
    switch (Ctx.lookup_tvar(ctx, name)) {
    | Some(_) => true
    | None => false
    };
}
and Kind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t);
  let normalize: (Ctx.t, Typ.t) => Typ.t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t);
  let rec normalize = (ctx, typ) => {
    switch (typ) {
    | Typ.Var(x) =>
      switch (Ctx.lookup_tvar(ctx, x)) {
      | Some(Singleton(typ)) => normalize(ctx, typ)
      | None => typ
      }
    | Typ.Unknown(_)
    | Int
    | Float
    | Bool
    | String => typ
    | List(t) => List(normalize(ctx, t))
    | Arrow(t1, t2) => Arrow(normalize(ctx, t1), normalize(ctx, t2))
    | LabelSum(ts) =>
      LabelSum(
        List.map(
          (Typ.{typ, tag}) =>
            Typ.{typ: Option.map(normalize(ctx), typ), tag},
          ts,
        ),
      )
    | Rec(x, ty) => Rec(x, normalize(ctx, ty))
    | Sum(t1, t2) => Sum(normalize(ctx, t1), normalize(ctx, t2))
    | Prod(ts) => Prod(List.map(t => normalize(ctx, t), ts))
    };
  };
};

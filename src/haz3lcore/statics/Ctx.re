open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type var_entry = {
  name: Token.t,
  id: Id.t,
  typ: Typ.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tvar_entry = {
  name: Token.t,
  id: Id.t,
  kind: Kind.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type entry =
  | VarEntry(var_entry)
  | TagEntry(var_entry)
  | TVarEntry(tvar_entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type co_entry = {
  id: Id.t,
  mode: Typ.mode,
};
/* Each co-context entry is a list of the uses of a variable
   within some scope, including their type demands */
[@deriving (show({with_path: false}), sexp, yojson)]
type co = VarMap.t_(list(co_entry));

let extend = List.cons;

let lookup = (ctx: t, name: Token.t): option(entry) =>
  // TODO: will use lookup for everything cause a namespace collision?
  // NOTE: we may need separate lookup functions for each entry type
  List.find_map(
    fun
    | VarEntry(v) when v.name == name => Some(VarEntry(v))
    | TagEntry(v) when v.name == name => Some(TagEntry(v))
    | TVarEntry(v) when v.name == name => Some(TVarEntry(v))
    | _ => None,
    ctx,
  );

let add_abstract = (ctx: t, name: Token.t, id: Id.t): t =>
  extend(TVarEntry({name, id, kind: Abstract}), ctx);

let lookup_tvar = (ctx: t, name: Token.t): option(tvar_entry) =>
  // TODO: see comments in lookup
  switch (lookup(ctx, name)) {
  | Some(TVarEntry(t)) => Some(t)
  | _ => None
  };

let rec lookup_tvar_idx = (~i=0, ctx: t, x: Token.t): option(int) => {
  switch (ctx) {
  | [] => None
  | [TVarEntry({name, _}), ..._] when Token.compare(name, x) == 0 =>
    Some(i)
  | [TVarEntry(_), ...ctx] => lookup_tvar_idx(ctx, x, ~i=i + 1)
  | [_entry, ...ctx] => lookup_tvar_idx(ctx, x, ~i)
  };
};

let rec lookup_typ_by_idx = (~i, ctx: t): option(Typ.t) => {
  switch (ctx) {
  | [] => None
  | [TVarEntry({kind, _}), ..._] when i == 0 =>
    switch (kind) {
    | Singleton(typ) => Some(typ)
    | Abstract => None
    }
  | [TVarEntry(_), ...ctx] => lookup_typ_by_idx(~i=i - 1, ctx)
  | [_entry, ...ctx] => lookup_typ_by_idx(~i, ctx)
  };
};

let lookup_alias = (ctx: t, t: Token.t): option(Typ.t) =>
  switch (lookup_tvar(ctx, t)) {
  | Some({kind: Singleton(ty), _}) => Some(ty)
  | Some({kind: Abstract, _})
  | _ => None
  };
let empty: t = VarMap.empty;

let get_id: entry => int =
  fun
  | VarEntry({id, _})
  | TagEntry({id, _})
  | TVarEntry({id, _}) => id;

let lookup_var = (ctx: t, name: string): option(var_entry) =>
  switch (lookup(ctx, name)) {
  | Some(VarEntry(v)) => Some(v)
  | _ => None
  };

let lookup_tag = (ctx: t, name: string): option(var_entry) =>
  switch (lookup(ctx, name)) {
  | Some(TagEntry(t)) => Some(t)
  | _ => None
  };

let is_alias = (ctx: t, name: Token.t): bool =>
  switch (lookup_alias(ctx, name)) {
  | Some(_) => true
  | None => false
  };

let add_alias = (ctx: t, name: Token.t, id: Id.t, ty: Typ.t): t =>
  extend(TVarEntry({name, id, kind: Singleton(ty)}), ctx);

let add_tags =
    (ctx: t, sum_idx: Typ.ann(option(int)), id: Id.t, tags: Typ.sum_map): t =>
  List.map(
    ((tag, typ)) =>
      TagEntry({
        name: tag,
        id,
        typ:
          switch (typ) {
          | None => Var(sum_idx)
          | Some(typ) => Arrow(typ, Var(sum_idx))
          },
      }),
    tags,
  )
  @ ctx;

let added_bindings = (ctx_after: t, ctx_before: t): t => {
  /* Precondition: new_ctx is old_ctx plus some new bindings */
  let new_count = List.length(ctx_after) - List.length(ctx_before);
  switch (ListUtil.split_n_opt(new_count, ctx_after)) {
  | Some((ctx, _)) => ctx
  | _ => []
  };
};

let free_in = (ctx_before: t, ctx_after, free: co): co => {
  let added_bindings = added_bindings(ctx_after, ctx_before);
  VarMap.filter(
    ((k, _)) =>
      switch (lookup_var(added_bindings, k)) {
      | None => true
      | Some(_) => false
      },
    free,
  );
};

let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  // NOTE: does not check that the prefix is an actual prefix
  let prefix_length = List.length(prefix_ctx);
  let ctx_length = List.length(ctx);
  if (prefix_length > ctx_length) {
    None;
  } else {
    Some(
      List.rev(
        ListUtil.sublist((prefix_length, ctx_length), List.rev(ctx)),
      ),
    );
  };
};

/* Note: this currently shadows in the case of duplicates */
let union: list(co) => co =
  List.fold_left((free1, free2) => free1 @ free2, []);

module VarSet = Set.Make(Token);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t): t =>
  ctx
  |> List.fold_left(
       ((ctx, term_set, typ_set), entry) => {
         switch (entry) {
         | VarEntry({name, _})
         | TagEntry({name, _}) =>
           VarSet.mem(name, term_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], VarSet.add(name, term_set), typ_set)
         | TVarEntry({name, _}) =>
           VarSet.mem(name, typ_set)
             ? (ctx, term_set, typ_set)
             : ([entry, ...ctx], term_set, VarSet.add(name, typ_set))
         }
       },
       ([], VarSet.empty, VarSet.empty),
     )
  |> (((ctx, _, _)) => List.rev(ctx));

let rec normalize_shallow = (ctx: t, ty: Typ.t): Typ.t =>
  switch (ty) {
  | Var({item: Some(idx), _}) =>
    switch (lookup_typ_by_idx(ctx, ~i=idx)) {
    | Some(ty) => normalize_shallow(ctx, ty)
    | None => ty
    }
  | _ => ty
  };

let rec normalize = (ctx: t, ty: Typ.t): Typ.t => {
  switch (ty) {
  | Var({item: Some(idx), _}) =>
    switch (lookup_typ_by_idx(ctx, ~i=idx)) {
    | Some(ty) => normalize(ctx, ty)
    | None => ty
    }
  | Var(_)
  | Unknown(_)
  | Int
  | Float
  | Bool
  | String => ty
  | List(t) => List(normalize(ctx, t))
  | Arrow(t1, t2) => Arrow(normalize(ctx, t1), normalize(ctx, t2))
  | Prod(ts) => Prod(List.map(normalize(ctx), ts))
  | Sum(ts) => Sum(Util.TagMap.map(Option.map(normalize(ctx)), ts))
  | Forall({item: ty, name}) =>
    /* NOTE: Fake -1 id below is a hack, but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Forall({item: normalize(add_abstract(ctx, name, -1), ty), name})
  | Rec({item: ty, name}) =>
    /* NOTE: Fake -1 id below is a hack, but shouldn't matter
       as in current implementation Recs do not occur in the
       surface syntax, so we won't try to jump to them. */
    Rec({item: normalize(add_abstract(ctx, name, -1), ty), name})
  };
};

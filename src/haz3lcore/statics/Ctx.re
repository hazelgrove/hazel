open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind('a) =
  | Singleton(TermBase.Typ.t('a))
  | Abstract;

[@deriving (show({with_path: false}), sexp, yojson)]
type var_entry('a) = {
  name: Var.t,
  id: Id.t, // TODO: Should this be a type param?
  typ: TermBase.Typ.t('a),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tvar_entry('a) = {
  name: string,
  id: Id.t,
  kind: kind('a),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type entry('a) =
  | VarEntry(var_entry('a))
  | ConstructorEntry(var_entry('a))
  | TVarEntry(tvar_entry('a));

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = list(entry('a));

let extend = (ctx, entry) => List.cons(entry, ctx);

let extend_tvar = (ctx: t('a), tvar_entry: tvar_entry('a)): t('a) =>
  extend(ctx, TVarEntry(tvar_entry));

let extend_alias =
    (ctx: t('a), name: string, id: Id.t, ty: TermBase.Typ.t('a)): t('a) =>
  extend_tvar(ctx, {name, id, kind: Singleton(ty)});

let extend_dummy_tvar = (ctx: t('a), tvar: TPat.t('a)) =>
  switch (TPat.tyvar_of_utpat(tvar)) {
  | Some(name) => extend_tvar(ctx, {kind: Abstract, name, id: Id.invalid})
  | None => ctx
  };

let lookup_tvar = (ctx: t('a), name: string): option(kind('a)) =>
  List.find_map(
    fun
    | TVarEntry(v) when v.name == name => Some(v.kind)
    | _ => None,
    ctx,
  );

let lookup_tvar_id = (ctx: t('a), name: string): option(Id.t) =>
  List.find_map(
    fun
    | TVarEntry(v) when v.name == name => Some(v.id)
    | _ => None,
    ctx,
  );

let get_id: entry('a) => Id.t =
  fun
  | VarEntry({id, _})
  | ConstructorEntry({id, _})
  | TVarEntry({id, _}) => id;

let lookup_var = (ctx: t('a), name: string): option(var_entry('a)) =>
  List.find_map(
    fun
    | VarEntry(v) when v.name == name => Some(v)
    | _ => None,
    ctx,
  );

let lookup_ctr = (ctx: t('a), name: string): option(var_entry('a)) =>
  List.find_map(
    fun
    | ConstructorEntry(t) when t.name == name => Some(t)
    | _ => None,
    ctx,
  );

let is_alias = (ctx: t('a), name: string): bool =>
  switch (lookup_tvar(ctx, name)) {
  | Some(Singleton(_)) => true
  | Some(Abstract)
  | None => false
  };

let is_abstract = (ctx: t('a), name: string): bool =>
  switch (lookup_tvar(ctx, name)) {
  | Some(Abstract) => true
  | Some(Singleton(_))
  | None => false
  };

// TODO Should this return back a fresh ID or should this function just not operate on ids
let lookup_alias =
    (ctx: t(IdTag.t), name: string): option(TermBase.Typ.t(IdTag.t)) =>
  switch (lookup_tvar(ctx, name)) {
  | Some(Singleton(ty)) => Some(ty)
  | Some(Abstract) => None
  | None =>
    Some(TermBase.Typ.Unknown(Hole(Invalid(name))) |> Annotated.fresh)
  };

let add_ctrs =
    (
      ctx: t(IdTag.t),
      name: string,
      id: Id.t,
      ctrs: TermBase.Typ.sum_map(IdTag.t),
    )
    : t(IdTag.t) =>
  List.filter_map(
    fun
    | ConstructorMap.Variant(ctr, _, typ) =>
      Some(
        ConstructorEntry({
          name: ctr,
          id,
          typ:
            switch (typ) {
            | None => TermBase.Typ.Var(name) |> Annotated.fresh
            | Some(typ) =>
              TermBase.Typ.Arrow(
                typ,
                TermBase.Typ.Var(name) |> Annotated.fresh,
              )
              |> Annotated.fresh
            },
        }),
      )
    | ConstructorMap.BadEntry(_) => None,
    ctrs,
  )
  @ ctx;

let subtract_prefix = (ctx: t('a), prefix_ctx: t('a)): option(t('a)) => {
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

let added_bindings = (ctx_after: t('a), ctx_before: t('a)): t('a) => {
  /* Precondition: new_ctx is old_ctx plus some new bindings */
  let new_count = List.length(ctx_after) - List.length(ctx_before);
  switch (ListUtil.split_n_opt(new_count, ctx_after)) {
  | Some((ctx, _)) => ctx
  | _ => []
  };
};

module VarSet = Set.Make(Var);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t('a)): t('a) =>
  ctx
  |> List.fold_left(
       ((ctx, term_set, typ_set), entry) => {
         switch (entry) {
         | VarEntry({name, _})
         | ConstructorEntry({name, _}) =>
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

let shadows_typ = (ctx: t('a), name: string): bool =>
  Form.is_base_typ(name) || lookup_tvar(ctx, name) != None;

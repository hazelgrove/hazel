open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Singleton(TermBase.typslice_t)
  | Abstract;

[@deriving (show({with_path: false}), sexp, yojson)]
type var_entry = {
  name: Var.t,
  id: Id.t,
  typ: TermBase.typslice_t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tvar_entry = {
  name: string,
  id: Id.t,
  kind,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type entry =
  | VarEntry(var_entry)
  | ConstructorEntry(var_entry)
  | TVarEntry(tvar_entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(entry);

let extend = (ctx, entry) => List.cons(entry, ctx);

let extend_tvar = (ctx: t, tvar_entry: tvar_entry): t =>
  extend(ctx, TVarEntry(tvar_entry));

let extend_alias =
    (ctx: t, name: string, id: Id.t, ty: TermBase.TypSlice.t): t =>
  extend_tvar(
    ctx,
    {
      name,
      id,
      kind: Singleton(ty),
    },
  );

let extend_dummy_tvar = (ctx: t, tvar: TPat.t) =>
  switch (TPat.tyvar_of_utpat(tvar)) {
  | Some(name) =>
    extend_tvar(
      ctx,
      {
        kind: Abstract,
        name,
        id: Id.invalid,
      },
    )
  | None => ctx
  };

let lookup_tvar = (ctx: t, name: string): option(kind) =>
  List.find_map(
    fun
    | TVarEntry(v) when v.name == name => Some(v.kind)
    | _ => None,
    ctx,
  );

let lookup_tvar_id = (ctx: t, name: string): option(Id.t) =>
  List.find_map(
    fun
    | TVarEntry(v) when v.name == name => Some(v.id)
    | _ => None,
    ctx,
  );

let get_id: entry => Id.t =
  fun
  | VarEntry({id, _})
  | ConstructorEntry({id, _})
  | TVarEntry({id, _}) => id;

let lookup_var = (ctx: t, name: string): option(var_entry) =>
  List.find_map(
    fun
    | VarEntry(v) when v.name == name => Some(v)
    | _ => None,
    ctx,
  );

let lookup_ctr = (ctx: t, name: string): option(var_entry) =>
  List.find_map(
    fun
    | ConstructorEntry(t) when t.name == name => Some(t)
    | _ => None,
    ctx,
  );

let is_alias = (ctx: t, name: string): bool =>
  switch (lookup_tvar(ctx, name)) {
  | Some(Singleton(_)) => true
  | Some(Abstract)
  | None => false
  };

let is_abstract = (ctx: t, name: string): bool =>
  switch (lookup_tvar(ctx, name)) {
  | Some(Abstract) => true
  | Some(Singleton(_))
  | None => false
  };

let lookup_alias = (ctx: t, name: string): option(TermBase.TypSlice.t) =>
  switch (lookup_tvar(ctx, name)) {
  | Some(Singleton(ty)) => Some(ty)
  | Some(Abstract) => None
  | None =>
    Some(
      `Typ(Unknown(Hole(Invalid(name))): TermBase.Typ.term)
      |> IdTagged.fresh,
    )
  };

// name_ids are the ids to slice source of name
let add_ctrs =
    (
      name_ids: list(Id.t),
      ctx: t,
      name: string,
      id: Id.t,
      ctrs: TermBase.TypSlice.sum_map,
    )
    : t =>
  List.filter_map(
    fun
    | ConstructorMap.Variant(ctr, ids, typ) =>
      Some(
        ConstructorEntry({
          name: ctr,
          id,
          typ:
            switch (typ) {
            | None => `Typ(Var(name): TermBase.typ_term) |> IdTagged.fresh
            | Some(typ) =>
              (
                `SliceIncr((
                  Slice(
                    Arrow(
                      typ,
                      `SliceGlobal((
                        `Typ(Var(name): TermBase.typ_term),
                        {
                          ctx_used: [],
                          term_ids: name_ids,
                        }: TermBase.slice_global,
                      ))
                      |> IdTagged.fresh,
                    ),
                  ),
                  {
                    ctx_used: [],
                    term_ids: [id, ...ids],
                  },
                )): TermBase.typslice_term
              )
              |> IdTagged.fresh
            },
        }),
      )
    | ConstructorMap.BadEntry(_) => None,
    ctrs,
  )
  @ ctx;

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

let added_bindings = (ctx_after: t, ctx_before: t): t => {
  /* Precondition: new_ctx is old_ctx plus some new bindings */
  let new_count = List.length(ctx_after) - List.length(ctx_before);
  switch (ListUtil.split_n_opt(new_count, ctx_after)) {
  | Some((ctx, _)) => ctx
  | _ => []
  };
};

module VarSet = Set.Make(Var);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t): t =>
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

let filter_stepper_filter_variables = (ctx: t): t =>
  ctx
  |> List.fold_left(
       (ctx, entry) => {
         switch (entry) {
         | VarEntry({name, _})
         | ConstructorEntry({name, _})
         | TVarEntry({name, _}) =>
           if (String.starts_with(~prefix="$", name)) {
             ctx;
           } else {
             [entry, ...ctx];
           }
         }
       },
       [],
     )
  |> List.rev;

let shadows_typ = (ctx: t, name: string): bool =>
  Form.is_base_typ(name) || lookup_tvar(ctx, name) != None;

/* The binding (binding site id and name) of `name` in `ctx` */
let binding_of = (ctx: t, name: Var.t): Binding.t =>
  switch (lookup_var(ctx, name)) {
  | Some({id, _}) => {
      id,
      name,
    }
  | _ => {
      id: Id.invalid,
      name,
    }
  };

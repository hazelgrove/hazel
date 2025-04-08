open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Singleton(TermBase.typ_t)
  | Abstract;

[@deriving (show({with_path: false}), sexp, yojson)]
type var_entry = {
  id: Id.t,
  typ: TermBase.typ_t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tvar_entry = {
  id: Id.t,
  kind,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type entry =
  | VarEntry(var_entry)
  | ConstructorEntry(var_entry)
  | TVarEntry(tvar_entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t(entry);

let extend = VarMap.extend;

let extend_tvar = (ctx: t, name: Var.t, tvar_entry: tvar_entry): t =>
  extend(ctx, (name, TVarEntry(tvar_entry)));

let extend_alias = (ctx: t, name: string, id: Id.t, ty: TermBase.Typ.t): t =>
  extend_tvar(
    ctx,
    name,
    {
      id,
      kind: Singleton(ty),
    },
  );

let extend_dummy_tvar = (ctx: t, tvar: TPat.t) =>
  switch (TPat.tyvar_of_utpat(tvar)) {
  | Some(name) =>
    extend_tvar(
      ctx,
      name,
      {
        kind: Abstract,
        id: Id.invalid,
      },
    )
  | None => ctx
  };

let lookup_tvar = (ctx: t, name: string): option(kind) =>
  VarMap.filter_find_map(
    fun
    | TVarEntry(v) => Some(v.kind)
    | _ => None,
    name,
    ctx,
  )
  |> ListUtil.hd_opt;

let lookup_tvar_id = (ctx: t, name: string): option(Id.t) =>
  VarMap.filter_find_map(
    fun
    | TVarEntry(v) => Some(v.id)
    | _ => None,
    name,
    ctx,
  )
  |> ListUtil.hd_opt;

let get_id: entry => Id.t =
  fun
  | VarEntry({id, _})
  | ConstructorEntry({id, _})
  | TVarEntry({id, _}) => id;

let lookup_var = (ctx: t, name: string): option(var_entry) =>
  VarMap.filter_find_map(
    fun
    | VarEntry(v) => Some(v)
    | _ => None,
    name,
    ctx,
  )
  |> ListUtil.hd_opt;

let lookup_ctr = (ctx: t, name: string): option(var_entry) =>
  VarMap.filter_find_map(
    fun
    | ConstructorEntry(v) => Some(v)
    | _ => None,
    name,
    ctx,
  )
  |> ListUtil.hd_opt;

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

let lookup_alias = (ctx: t, name: string): option(TermBase.Typ.t) =>
  switch (lookup_tvar(ctx, name)) {
  | Some(Singleton(ty)) => Some(ty)
  | Some(Abstract) => None
  | None =>
    Some(
      (Unknown(Hole(Invalid(name))): TermBase.Typ.term) |> IdTagged.fresh,
    )
  };

let add_ctrs = (ctx: t, name: string, id: Id.t, ctrs: TermBase.Typ.sum_map): t =>
  List.fold_left(
    m =>
      fun
      | ConstructorMap.Variant(ctr, _, typ) =>
        extend(
          m,
          (
            ctr,
            ConstructorEntry({
              id,
              typ:
                switch (typ) {
                | None => (Var(name): TermBase.typ_term) |> IdTagged.fresh
                | Some(typ) =>
                  (
                    Arrow(
                      typ,
                      (Var(name): TermBase.typ_term) |> IdTagged.fresh,
                    ): TermBase.typ_term
                  )
                  |> IdTagged.fresh
                },
            }),
          ),
        )
      | ConstructorMap.BadEntry(_) => m,
    ctx,
    ctrs,
  );

let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  let ctx_list = VarMap.to_assoc_list(ctx);
  let prefix_ctx_list = VarMap.to_assoc_list(prefix_ctx);

  // NOTE: does not check that the prefix is an actual prefix
  let prefix_length = List.length(prefix_ctx_list);
  let ctx_length = List.length(ctx_list);
  if (prefix_length > ctx_length) {
    None;
  } else {
    Some(
      List.rev(
        ListUtil.sublist((prefix_length, ctx_length), List.rev(ctx_list)),
      )
      |> VarMap.of_assoc_list,
    );
  };
};

let added_bindings = (ctx_after: t, ctx_before: t): t => {
  let ctx_after_list = VarMap.to_assoc_list(ctx_after);
  let ctx_before_list = VarMap.to_assoc_list(ctx_before);
  /* Precondition: new_ctx is old_ctx plus some new bindings */
  let new_count = List.length(ctx_after_list) - List.length(ctx_before_list);
  (
    switch (ListUtil.split_n_opt(new_count, ctx_after_list)) {
    | Some((ctx, _)) => ctx
    | _ => []
    }
  )
  |> VarMap.of_assoc_list;
};

module VarSet = Set.Make(Var);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t): t =>
  ctx
  |> VarMap.to_assoc_list
  |> List.fold_left(
       ((ctx, term_set, typ_set), (name, entry)) => {
         switch (entry) {
         | VarEntry(_)
         | ConstructorEntry(_) =>
           VarSet.mem(name, term_set)
             ? (ctx, term_set, typ_set)
             : (
               [(name, entry), ...ctx],
               VarSet.add(name, term_set),
               typ_set,
             )
         | TVarEntry(_) =>
           VarSet.mem(name, typ_set)
             ? (ctx, term_set, typ_set)
             : (
               [(name, entry), ...ctx],
               term_set,
               VarSet.add(name, typ_set),
             )
         }
       },
       ([], VarSet.empty, VarSet.empty),
     )
  |> (((ctx, _, _)) => List.rev(ctx))
  |> VarMap.of_assoc_list;

let filter_stepper_filter_variables = (ctx: t): t =>
  ctx
  |> VarMap.to_assoc_list
  |> List.fold_left(
       (ctx, (name, entry)) => {
         switch (entry) {
         | VarEntry(_)
         | ConstructorEntry(_)
         | TVarEntry(_) =>
           if (String.starts_with(~prefix="$", name)) {
             ctx;
           } else {
             [(name, entry), ...ctx];
           }
         }
       },
       [],
     )
  |> List.rev
  |> VarMap.of_assoc_list;

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

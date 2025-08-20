open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Singleton(TermBase.typ_t)
  | Abstract;

[@deriving (show({with_path: false}), sexp, yojson)]
type var_entry = {
  name: Var.t,
  id: Id.t,
  typ: TermBase.typ_t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tvar_entry = {
  name: string,
  id: Id.t,
  kind,
};

type model_piece = {
  model: TermBase.Exp.t,
  piece: Base.piece,
};

type node_or_list =
  | Node(Virtual_dom.Vdom.Node.t)
  | List(list(Virtual_dom.Vdom.Node.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type entry =
  | VarEntry(var_entry)
  | ConstructorEntry(var_entry)
  | TVarEntry(tvar_entry)
  | LivelitEntry(LivelitCtx.raw_livelit);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  use_mode: option(Operators.mode), // None if elaboration has already occurred
  entries: list(entry),
};

let empty: t = {
  use_mode: None,
  entries: [],
};

let extend = (ctx: t, entry): t => {
  ...ctx,
  entries: List.cons(entry, ctx.entries),
};

let extend_tvar = (ctx: t, tvar_entry: tvar_entry): t =>
  extend(ctx, TVarEntry(tvar_entry));

let extend_alias = (ctx: t, name: string, id: Id.t, ty: TermBase.Typ.t): t =>
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
    ctx.entries,
  );

let lookup_tvar_id = (ctx: t, name: string): option(Id.t) =>
  List.find_map(
    fun
    | TVarEntry(v) when v.name == name => Some(v.id)
    | _ => None,
    ctx.entries,
  );

let lookup_livelit = (ctx: t, name: string): option(LivelitCtx.raw_livelit) =>
  List.find_map(
    fun
    | LivelitEntry(v) when v.name == name => Some(v)
    | _ => None,
    ctx.entries,
  );

let get_id: entry => Id.t =
  fun
  | VarEntry({id, _})
  | ConstructorEntry({id, _})
  | TVarEntry({id, _}) => id
  | LivelitEntry({name, _}) => Id.mk_str(name);

let lookup_var = (ctx: t, name: string): option(var_entry) =>
  List.find_map(
    fun
    | VarEntry(v) when v.name == name => Some(v)
    | _ => None,
    ctx.entries,
  );

let lookup_ctr = (ctx: t, name: string): option(var_entry) =>
  List.find_map(
    fun
    | ConstructorEntry(t) when t.name == name => Some(t)
    | _ => None,
    ctx.entries,
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

let lookup_alias = (ctx: t, name: string): option(TermBase.Typ.t) =>
  switch (lookup_tvar(ctx, name)) {
  | Some(Singleton(ty)) => Some(ty)
  | Some(Abstract) => None
  | None =>
    Some(
      (
        {
          typ: Unknown(Hole(Invalid(name))),
          syn_slice: CodeSlice.empty,
          ana_slice: CodeSlice.empty,
        }: TermBase.Typ.slice
      )
      |> IdTagged.fresh,
    )
  };

// name_ids are the ids to slice source of name
let add_ctrs =
    (
      def_ids: list(Id.t),
      ctx: t,
      name: string,
      id: Id.t,
      ctrs: TermBase.Typ.sum_map,
    )
    : t => {
  ...ctx,
  entries:
    List.filter_map(
      fun
      | ConstructorMap.Variant(ctr, ctr_ids, typ) =>
        Some(
          ConstructorEntry({
            name: ctr,
            id,
            typ:
              switch (typ) {
              | None =>
                (
                  {
                    typ: Var(name),
                    syn_slice: {
                      ctx_used: [],
                      term_ids: [id, ...ctr_ids],
                    },

                    ana_slice: {
                      ctx_used: [],
                      term_ids: def_ids,
                    },
                  }: TermBase.Typ.slice
                )
                |> IdTagged.fresh
              | Some(ty) =>
                (
                  {
                    typ:
                      Arrow(
                        ty,
                        (
                          {
                            typ: Var(name),
                            ana_slice: {
                              ctx_used: [],
                              term_ids: def_ids,
                            },
                            syn_slice: CodeSlice.empty,
                          }: TermBase.Typ.slice
                        )
                        |> IdTagged.fresh,
                      ),
                    syn_slice: {
                      ctx_used: [],
                      term_ids: [id, ...ctr_ids],
                    },
                    ana_slice: CodeSlice.empty,
                  }: TermBase.Typ.slice
                )
                |> IdTagged.fresh
              },
          }),
        )
      | ConstructorMap.BadEntry(_) => None,
      ctrs,
    )
    @ ctx.entries,
};

let set_use_mode = (ctx: t, use_mode: option(Operators.mode)): t => {
  ...ctx,
  use_mode,
};

let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  // NOTE: does not check that the prefix is an actual prefix
  let prefix_length = List.length(prefix_ctx.entries);
  let ctx_length = List.length(ctx.entries);
  if (prefix_length > ctx_length) {
    None;
  } else {
    Some({
      ...ctx,
      entries:
        List.rev(
          ListUtil.sublist(
            (prefix_length, ctx_length),
            List.rev(ctx.entries),
          ),
        ),
    });
  };
};

let added_bindings = (ctx_after: t, ctx_before: t): t => {
  /* Precondition: new_ctx is old_ctx plus some new bindings */
  let new_count =
    List.length(ctx_after.entries) - List.length(ctx_before.entries);
  switch (ListUtil.split_n_opt(new_count, ctx_after.entries)) {
  | Some((ctx, _)) => {
      ...ctx_after,
      entries: ctx,
    }
  | _ => {
      ...ctx_after,
      entries: [],
    }
  };
};

module VarSet = Set.Make(Var);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t): t => {
  ...ctx,
  entries:
    ctx.entries
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
           | LivelitEntry({name, _}) =>
             VarSet.mem(name, term_set)
               ? (ctx, term_set, typ_set)
               : ([entry, ...ctx], VarSet.add(name, term_set), typ_set)
           }
         },
         ([], VarSet.empty, VarSet.empty),
       )
    |> (((ctx, _, _)) => List.rev(ctx)),
};

let filter_stepper_filter_variables = (ctx: t): t => {
  ...ctx,
  entries:
    ctx.entries
    |> List.fold_left(
         (ctx, entry) => {
           switch (entry) {
           | VarEntry({name, _})
           | ConstructorEntry({name, _})
           | LivelitEntry({name, _})
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
    |> List.rev,
};

let shadows_typ = (ctx: t, name: string): bool =>
  Form.is_base_typ(name) || lookup_tvar(ctx, name) != None;

let empty_pre_elaboration = {
  use_mode: Some(Operators.default_mode),
  entries: [],
};
let empty_post_elaboration = {
  use_mode: None,
  entries: [],
};

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

let concat = (ctx1: t, ctx2: t): t => {
  ...ctx1,
  entries: ctx1.entries @ ctx2.entries,
};

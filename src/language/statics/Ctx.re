open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type custom_statics =
  | ToLvs
  | ProjectLabels
  | OmitLabels
  | OmitAllLabels
  | GroupByLabel
  | SelectLabels;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Singleton(TermBase.typ_t)
  | Abstract;

[@deriving (show({with_path: false}), sexp, yojson)]
type var_entry = {
  name: Var.t,
  id: Id.t,
  typ: TermBase.typ_t,
  custom_statics: option(custom_statics),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tvar_entry = {
  name: string,
  id: Id.t,
  kind,
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

/* An entry list ([entries] is newest-first) plus a SIZE field, so the
   per-binder-scope operations (added_bindings, subtract_prefix) are
   O(diff) instead of paying O(n) List.lengths. Serialization goes
   through [repr]; the wire format carries no size. NOTE a name-keyed
   map representation was tried and reverted — no measured benefit;
   see plans/perf-ledger.md §5/§7 before re-proposing. */
[@deriving (show({with_path: false}), sexp, yojson)]
type repr = {
  use_mode: option(Operators.mode), // None if elaboration has already occurred
  entries: list(entry),
};

type t = {
  use_mode: option(Operators.mode),
  entries: list(entry),
  size: int,
};

let extend = (ctx: t, entry): t => {
  ...ctx,
  entries: [entry, ...ctx.entries],
  size: ctx.size + 1,
};

let of_entries =
    (~use_mode: option(Operators.mode), entries: list(entry)): t => {
  use_mode,
  entries,
  size: List.length(entries),
};

/* prepend a newest-first run of entries (preserves the old
   [new_entries @ ctx.entries] semantics) */
let prepend_entries = (ctx: t, new_entries: list(entry)): t => {
  ...ctx,
  entries: new_entries @ ctx.entries,
  size: ctx.size + List.length(new_entries),
};

/* ---- serialization: [entries] is canonical; the wire format is
   identical to the plain-record representation ---- */
let repr_of = (ctx: t): repr => {
  use_mode: ctx.use_mode,
  entries: ctx.entries,
};
let of_repr = (r: repr): t => of_entries(~use_mode=r.use_mode, r.entries);
let sexp_of_t = (ctx: t) => sexp_of_repr(repr_of(ctx));
let t_of_sexp = s => of_repr(repr_of_sexp(s));
let yojson_of_t = (ctx: t) => yojson_of_repr(repr_of(ctx));
let t_of_yojson = j => of_repr(repr_of_yojson(j));
let pp = (fmt, ctx: t) => pp_repr(fmt, repr_of(ctx));
let show = (ctx: t) => show_repr(repr_of(ctx));

let equal = (a: t, b: t): bool =>
  a.use_mode == b.use_mode && a.entries == b.entries;

let empty: t = of_entries(~use_mode=None, []);

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
      (Unknown(Hole(Invalid(name))): TermBase.Typ.term) |> IdTagged.fresh,
    )
  };

let add_ctrs = (ctx: t, name: string, ctrs: TermBase.Typ.sum_map): t =>
  prepend_entries(
    ctx,
    List.filter_map(
      fun
      | ConstructorMap.Variant(ctr, ann, typ) => {
          assert(ann.ids != []);
          let ctr_id = List.hd(ann.ids);
          Some(
            ConstructorEntry({
              name: ctr,
              id: ctr_id,
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
              custom_statics: None,
            }),
          );
        }
      | ConstructorMap.BadEntry(_) => None,
      ctrs,
    ),
  );

let set_use_mode = (ctx: t, use_mode: option(Operators.mode)): t => {
  ...ctx,
  use_mode,
};

let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  // NOTE: does not check that the prefix is an actual prefix
  let n = ctx.size - prefix_ctx.size;
  if (n < 0) {
    None;
  } else {
    switch (ListUtil.split_n_opt(n, ctx.entries)) {
    | Some((added, _)) => Some(of_entries(~use_mode=ctx.use_mode, added))
    | None => None
    };
  };
};

let added_bindings = (ctx_after: t, ctx_before: t): t => {
  /* Precondition: new_ctx is old_ctx plus some new bindings */
  let new_count = ctx_after.size - ctx_before.size;
  switch (ListUtil.split_n_opt(new_count, ctx_after.entries)) {
  | Some((added, _)) => of_entries(~use_mode=ctx_after.use_mode, added)
  | _ => of_entries(~use_mode=ctx_after.use_mode, [])
  };
};

module VarSet = Set.Make(Var);

/* Removes shadowed variables from the context */
let filter_shadowed = (ctx: t): t =>
  ctx.entries
  |> List.fold_left(
       ((kept, term_set, typ_set), entry) => {
         switch (entry) {
         | VarEntry({name, _})
         | ConstructorEntry({name, _}) =>
           VarSet.mem(name, term_set)
             ? (kept, term_set, typ_set)
             : ([entry, ...kept], VarSet.add(name, term_set), typ_set)
         | TVarEntry({name, _}) =>
           VarSet.mem(name, typ_set)
             ? (kept, term_set, typ_set)
             : ([entry, ...kept], term_set, VarSet.add(name, typ_set))
         | LivelitEntry({name, _}) =>
           VarSet.mem(name, term_set)
             ? (kept, term_set, typ_set)
             : ([entry, ...kept], VarSet.add(name, term_set), typ_set)
         }
       },
       ([], VarSet.empty, VarSet.empty),
     )
  |> (((kept, _, _)) => of_entries(~use_mode=ctx.use_mode, List.rev(kept)));

let filter_stepper_filter_variables = (ctx: t): t =>
  ctx.entries
  |> List.filter(entry =>
       switch (entry) {
       | VarEntry({name, _})
       | ConstructorEntry({name, _})
       | LivelitEntry({name, _})
       | TVarEntry({name, _}) => !String.starts_with(~prefix="$", name)
       }
     )
  |> of_entries(~use_mode=ctx.use_mode);

/* Keep in sync with Token.base_typs */
let is_base_typ = (name: string): bool =>
  name == "Bool"
  || name == "Float"
  || name == "Int"
  || name == "Nat"
  || name == "SInt"
  || name == "String"
  || name == "Void"
  || name == "DrvJdmt"
  || name == "DrvCtx"
  || name == "DrvProp"
  || name == "ALFAExp"
  || name == "DrvPat"
  || name == "ALFATyp"
  || name == "DrvTPat";

let empty_pre_elaboration =
  of_entries(~use_mode=Some(Operators.default_mode), []);
let empty_post_elaboration = of_entries(~use_mode=None, []);

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

let get_var_entries = (ctx: t): list(var_entry) =>
  List.filter_map(
    fun
    | VarEntry(v) => Some(v)
    | _ => None,
    ctx.entries,
  );

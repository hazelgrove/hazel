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

/* newest-first entries plus a size, so added_bindings/subtract_prefix
   are O(diff) rather than paying List.length */
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

/* prepend a newest-first run of entries */
let prepend_entries = (ctx: t, new_entries: list(entry)): t => {
  ...ctx,
  entries: new_entries @ ctx.entries,
  size: ctx.size + List.length(new_entries),
};

/* serialization goes through [repr], the plain record without size */
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

/* Bind the member declared by a signature item, if any. Signature items
   scope sequentially: later items may mention earlier type members (`T`)
   and, through paths, earlier value and module members (`Inner.T`). */
let extend_sig_item = (ctx: t, item: TermBase.Sig.t): t =>
  switch (item.term) {
  | SigType({term: Var(name), _} as tp, ty) =>
    extend_alias(ctx, name, IdTagged.rep_id(tp), ty)
  | SigTypeAbstract({term: Var(name), _} as tp) =>
    extend_tvar(
      ctx,
      {
        name,
        id: IdTagged.rep_id(tp),
        kind: Abstract,
      },
    )
  | SigLet(_)
  | SigModule(_) =>
    switch (Sig.member_of_item(item)) {
    | Some(Val(name, typ)) =>
      extend(
        ctx,
        VarEntry({
          name,
          id: IdTagged.rep_id(item),
          typ,
          custom_statics: None,
        }),
      )
    | _ => ctx
    }
  | SigType(_, _)
  | SigTypeAbstract(_)
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => ctx
  };

let extend_sig_items = (ctx: t, items: list(TermBase.Sig.t)): t =>
  List.fold_left(extend_sig_item, ctx, items);

/* While Some, lookup_tvar and lookup_var add each name they are asked
 * for. A computation that reads its ctx only through those two (and
 * lookup_alias, which is lookup_tvar) depends on the ctx only through their
 * answers for the names recorded, which is what lets a caller cache it and
 * check the cache by asking again: see TyDiCtx.named_fields_cached. Off, it
 * costs one dereference per lookup. */
let lookup_trace: ref(option(list(string))) = ref(None);

let note_lookup = (name: string): unit =>
  switch (lookup_trace^) {
  | Some(names) => lookup_trace := Some([name, ...names])
  | None => ()
  };

/* Run [f] with the trace on; its answer and the names it looked up. */
let with_lookup_trace = (f: unit => 'a): ('a, list(string)) => {
  let outer = lookup_trace^;
  lookup_trace := Some([]);
  switch (f()) {
  | result =>
    let names = Option.value(lookup_trace^, ~default=[]);
    /* An enclosing trace sees these lookups too. */
    lookup_trace := Option.map(outer_names => names @ outer_names, outer);
    (result, names);
  | exception e =>
    lookup_trace := outer;
    raise(e);
  };
};

/* An index of one shared tail of entries: the builtin ctx, which every
 * statics ctx is a user prefix consed onto (Builtins registers it). A
 * lookup scans the prefix and, on reaching that exact list, answers from
 * the index, which holds each name's FIRST entry of each kind -- what the
 * scan would have found. A ctx not ending in it is scanned as before.
 * Resolving a module path (Typ.path_sig) looks its name up as a type
 * variable, which misses and so used to scan every builtin, then as a
 * variable, which finds it among the builtins; on the Color slide that was
 * ~50 ms of every keystroke's ~300 ms of statics. */
type tail_index = {
  tail: list(entry),
  tvars: Hashtbl.t(string, kind),
  vars: Hashtbl.t(string, var_entry),
  ctrs: Hashtbl.t(string, var_entry),
};

let tail_index: ref(option(tail_index)) = ref(None);

/* For tests: lookups answered from the index. */
let tail_index_hits = ref(0);

let index_tail = (tail: list(entry)): unit => {
  let tvars = Hashtbl.create(64);
  let vars = Hashtbl.create(256);
  let ctrs = Hashtbl.create(256);
  let first = (tbl, name, v) =>
    if (!Hashtbl.mem(tbl, name)) {
      Hashtbl.add(tbl, name, v);
    };
  List.iter(
    fun
    | TVarEntry(v) => first(tvars, v.name, v.kind)
    | VarEntry(v) => first(vars, v.name, v)
    | ConstructorEntry(v) => first(ctrs, v.name, v)
    | LivelitEntry(_) => (),
    tail,
  );
  tail_index :=
    Some({
      tail,
      tvars,
      vars,
      ctrs,
    });
};

let lookup_tvar = (ctx: t, name: string): option(kind) => {
  note_lookup(name);
  switch (tail_index^) {
  | None =>
    List.find_map(
      fun
      | TVarEntry(v) when v.name == name => Some(v.kind)
      | _ => None,
      ctx.entries,
    )
  | Some(ix) =>
    let rec go = (entries: list(entry)) =>
      if (entries === ix.tail) {
        incr(tail_index_hits);
        Hashtbl.find_opt(ix.tvars, name);
      } else {
        switch (entries) {
        | [] => None
        | [TVarEntry(v), ..._] when v.name == name => Some(v.kind)
        | [_, ...rest] => go(rest)
        };
      };
    go(ctx.entries);
  };
};

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

let lookup_var = (ctx: t, name: string): option(var_entry) => {
  note_lookup(name);
  switch (tail_index^) {
  | None =>
    List.find_map(
      fun
      | VarEntry(v) when v.name == name => Some(v)
      | _ => None,
      ctx.entries,
    )
  | Some(ix) =>
    let rec go = (entries: list(entry)) =>
      if (entries === ix.tail) {
        incr(tail_index_hits);
        Hashtbl.find_opt(ix.vars, name);
      } else {
        switch (entries) {
        | [] => None
        | [VarEntry(v), ..._] when v.name == name => Some(v)
        | [_, ...rest] => go(rest)
        };
      };
    go(ctx.entries);
  };
};

/* the NEWEST binding of a capitalized name, whichever kind: a module (or
   any variable) bound after a constructor of the same name shadows it
   lexically, as any later binding shadows an earlier one */
let newest_var_or_ctr =
    (ctx: t, name: string)
    : option(
        [
          | `Var(var_entry)
          | `Ctr(var_entry)
        ],
      ) =>
  List.find_map(
    fun
    | VarEntry(v) when v.name == name => Some(`Var(v))
    | ConstructorEntry(c) when c.name == name => Some(`Ctr(c))
    | _ => None,
    ctx.entries,
  );

let lookup_ctr = (ctx: t, name: string): option(var_entry) =>
  switch (tail_index^) {
  | None =>
    List.find_map(
      fun
      | ConstructorEntry(t) when t.name == name => Some(t)
      | _ => None,
      ctx.entries,
    )
  | Some(ix) =>
    let rec go = (entries: list(entry)) =>
      if (entries === ix.tail) {
        incr(tail_index_hits);
        Hashtbl.find_opt(ix.ctrs, name);
      } else {
        switch (entries) {
        | [] => None
        | [ConstructorEntry(t), ..._] when t.name == name => Some(t)
        | [_, ...rest] => go(rest)
        };
      };
    go(ctx.entries);
  };

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

let is_base_typ = (name: string): bool => List.mem(name, Token.base_typs);

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

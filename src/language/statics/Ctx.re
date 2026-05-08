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
  typ_kind: TypKind.t,
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

let extend_alias =
    (
      ctx: t,
      name: string,
      id: Id.t,
      ~typ_kind=TypKind.Type,
      ty: TermBase.Typ.t,
    )
    : t =>
  extend_tvar(
    ctx,
    {
      name,
      id,
      kind: Singleton(ty),
      typ_kind,
    },
  );

let extend_dummy_tvar = (ctx: t, tvar: TPat.t) =>
  /* `tvar` may be a single binder or a `TPat.Tuple` representing a
     comma-separated list of binders; flatten and extend ctx with each
     name. Non-name binders (e.g. holes) are ignored. */
  List.fold_left(
    (ctx, name) =>
      extend_tvar(
        ctx,
        {
          kind: Abstract,
          typ_kind: TypKind.Type,
          name,
          id: Id.invalid,
        },
      ),
    ctx,
    TPat.tyvars_of(tvar),
  );

let lookup_tvar = (ctx: t, name: string): option(kind) =>
  List.find_map(
    fun
    | TVarEntry(v) when v.name == name => Some(v.kind)
    | _ => None,
    ctx.entries,
  );

let lookup_tvar_typ_kind = (ctx: t, name: string): option(TypKind.t) =>
  List.find_map(
    fun
    | TVarEntry(v) when v.name == name => Some(v.typ_kind)
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

/* All constructor entries with a given name, innermost first.
   Multiple sum types in scope can declare a constructor of the same
   name — `lookup_ctr` returns just the innermost one (its OCaml-style
   shadowing semantics), while this helper exposes every candidate so
   higher-level code can disambiguate (typically by analysis type). */
let lookup_ctrs = (ctx: t, name: string): list(var_entry) =>
  List.filter_map(
    fun
    | ConstructorEntry(t) when t.name == name => Some(t)
    | _ => None,
    ctx.entries,
  );

/* Walk `ty` past `Poly`, `Arrow` outputs, `TypParamAp` callees, and
   `Parens` to the leftmost type-alias name. Used to align a
   constructor schema's result-type head with an analysis target's
   head for type-directed disambiguation. */
let rec result_head_name_of = (ty: TermBase.Typ.t): option(string) =>
  switch (ty.term) {
  /* Look-through forms: walk towards the result-type's head. */
  | Poly(_, body) => result_head_name_of(body)
  | Arrow(_, out) => result_head_name_of(out)
  | TypParamAp(callee, _) => result_head_name_of(callee)
  | Parens(inner)
  | Projector(_, inner) => result_head_name_of(inner)
  | Rec(_, body) => result_head_name_of(body)
  | TypFun(_, body) => result_head_name_of(body)
  /* The leftmost type-alias name — what we want. */
  | Var(name) => Some(name)
  /* Forms that have no head name. Listed explicitly (no `_` wildcard)
     so adding a new `Typ.term` constructor in the future forces this
     code to be revisited. */
  | Atom(_)
  | Unknown(_)
  | DrvQuoteTy(_)
  | List(_)
  | Sum(_)
  | Prod(_)
  | TypTuple(_)
  | Label(_)
  | TupLabel(_, _)
  | ExplicitNonlabel
  | ProdProjection(_, _)
  | ProdExtension(_, _)
  | ProofOf(_)
  | Sig(_) => None
  };

/* Type-directed constructor lookup. When two sum types in scope both
   declare a constructor `B`, plain `lookup_ctr` returns the innermost,
   but the user-visible meaning is determined by the analysis type
   (`ana`): a `B(true) : OneOfThree(_, _, _)` should resolve to
   `OneOfThree`'s `B` even if a more recent `Either`'s `B` is in
   scope. We pick the entry whose schema result-type head matches
   `ana`'s head; if no candidate matches we fall back to the
   innermost. */
let lookup_ctr_for_ana =
    (ctx: t, name: string, ana: option(TermBase.Typ.t)): option(var_entry) => {
  let candidates = lookup_ctrs(ctx, name);
  let target = Option.bind(ana, result_head_name_of);
  switch (target) {
  | None =>
    switch (candidates) {
    | [hd, ..._] => Some(hd)
    | [] => None
    }
  | Some(target) =>
    let matching =
      List.find_opt(
        (e: var_entry) => result_head_name_of(e.typ) == Some(target),
        candidates,
      );
    switch (matching) {
    | Some(_) => matching
    | None =>
      switch (candidates) {
      | [hd, ..._] => Some(hd)
      | [] => None
      }
    };
  };
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

/* `None` for both "no such tvar" and "tvar is abstract (no aliased
   definition)". Callers should use the `None` case to mean "this
   name has no concrete RHS to substitute" — typically by leaving the
   `Var` alone. */
let lookup_alias = (ctx: t, name: string): option(TermBase.Typ.t) =>
  switch (lookup_tvar(ctx, name)) {
  | Some(Singleton(ty)) => Some(ty)
  | Some(Abstract)
  | None => None
  };

/* Build the result type that a constructor of a parameterized
   alias produces. For `type Either(a, b) = + A(a) + B(b)`, the
   `A` constructor's result type is `Either(a, b)` — i.e. the
   alias name applied to its parameters in *one* step, not a
   curried `Either(a)(b)`.

   Surface application of multiple type-args parses as a single
   `TypParamAp(callee, TypTuple([…]))` (multi-arg) or
   `TypParamAp(callee, arg)` (single-arg). Both reduction sites
   (`Typ.weak_head_normalize`, `Typ.apply_args`) are now uncurried-
   aware via `TPat.binders_of`, so this function also produces the
   uncurried shape. */
let result_type_for_params = (name: string, params: list(TermBase.TPat.t)) => {
  let head: TermBase.Typ.t = (Var(name): TermBase.Typ.term) |> IdTagged.fresh;
  let arg_vars =
    List.filter_map(
      (param: TermBase.TPat.t) =>
        switch (TermBase.TPat.tyvar_of_utpat(param)) {
        | Some(param_name) =>
          Some((Var(param_name): TermBase.Typ.term) |> IdTagged.fresh)
        | None => None
        },
      params,
    );
  switch (arg_vars) {
  | [] => head
  | [arg] => (TypParamAp(head, arg): TermBase.Typ.term) |> IdTagged.fresh
  | _ =>
    let tuple: TermBase.Typ.t =
      (TypTuple(arg_vars): TermBase.Typ.term) |> IdTagged.fresh;
    (TypParamAp(head, tuple): TermBase.Typ.term) |> IdTagged.fresh;
  };
};

let quantify_params = (params: list(TermBase.TPat.t), ty: TermBase.Typ.t) =>
  /* A parameterized type's constructor schema gets a single `Poly`
     wrapping. Single-parameter types use the bare param as the binder
     (`Poly(a, ty)`); multi-parameter types wrap the params into a
     `TPat.Tuple` so the schema mirrors the source-level multi-binder
     form `poly a, b -> …` (`Poly(Tuple([a, b]), ty)`). This way both
     the user's `pair@<Int, Bool>` and constructor specialization in
     elaboration peel one binder layer in one step. */
  switch (params) {
  | [] => ty
  | [param] => (Poly(param, ty): TermBase.Typ.term) |> IdTagged.fresh
  | _ =>
    let tuple_binder: TermBase.TPat.t =
      (Tuple(params): TermBase.TPat.term) |> IdTagged.fresh;
    (Poly(tuple_binder, ty): TermBase.Typ.term) |> IdTagged.fresh;
  };

let add_ctrs_with_params =
    (
      ctx: t,
      name: string,
      params: list(TermBase.TPat.t),
      ctrs: TermBase.Typ.sum_map,
    )
    : t => {
  ...ctx,
  entries:
    List.filter_map(
      fun
      | ConstructorMap.Variant(ctr, ann, typ) => {
          assert(ann.ids != []);
          let ctr_id = List.hd(ann.ids);
          let result_ty = result_type_for_params(name, params);
          let typ =
            switch (typ) {
            | None => result_ty
            | Some(typ) =>
              (Arrow(typ, result_ty): TermBase.Typ.term) |> IdTagged.fresh
            };
          Some(
            ConstructorEntry({
              name: ctr,
              id: ctr_id,
              typ: quantify_params(params, typ),
              custom_statics: None,
            }),
          );
        }
      | ConstructorMap.BadEntry(_) => None,
      ctrs,
    )
    @ ctx.entries,
};

let add_ctrs = (ctx: t, name: string, ctrs: TermBase.Typ.sum_map): t =>
  add_ctrs_with_params(ctx, name, [], ctrs);

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

/* Removes shadowed variables from the context */
let filter_shadowed = (ctx: t): t => {
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

/* Keep in sync with Token.base_typs */
let is_base_typ = (name: string): bool =>
  name == "Bool"
  || name == "Float"
  || name == "Int"
  || name == "Nat"
  || name == "SInt"
  || name == "String"
  || name == "DrvJdmt"
  || name == "DrvCtx"
  || name == "DrvProp"
  || name == "ALFAExp"
  || name == "DrvPat"
  || name == "ALFATyp"
  || name == "DrvTPat";

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

let get_var_entries = (ctx: t): list(var_entry) =>
  List.filter_map(
    fun
    | VarEntry(v) => Some(v)
    | _ => None,
    ctx.entries,
  );

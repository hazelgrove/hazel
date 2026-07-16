open Util;

type gamma = VarMap.t_(Typ.t);
type result = {
  omitted: Id.Set.t,
  gamma,
  psi: Typ.t,
  context: Ctx.t,
  ana: Typ.t,
};
type direction = [
  | `Syn
  | `Ana
];

type analysis_support =
  | Unsupported
  | ExpressionAscription
  | BindingAscription;

type child_mode =
  | Keep
  | Omit
  | Source
  | Track
  | Map
  | Prune
  | Ascribe
  | Alias
  | Alternative
  | Matched;

type exp_result = (Info.exp, Exp.t, Id.Map.t(Info.t));

type path = list(int);

type node = {
  id: Id.t,
  shape: Typ.t,
  typ: Typ.t,
  ana: Typ.t,
  dispatch: Typ.t => result,
};

type binding = {
  name: string,
  id: Id.t,
  path: option(path),
};

type lens = {
  parent_path: option(path),
  child_path: option(path),
};

type child = {
  mode: child_mode,
  node,
  lens: option(lens),
  bindings: list(binding),
  aliases: list(Ctx.tvar_entry),
  pattern: option(Info.pat),
};

exception Focus_not_found(Id.t);
exception Wrong_focus_sort;
exception Incompatible_query(Typ.t);
exception Pattern_ascription;

let gap: Typ.t = Typ.temp(Unknown(Hole(EmptyHole)));

let rec is_gap = (ty: Typ.t): bool =>
  switch (Typ.term_of(ty)) {
  | Parens(inner)
  | Projector(_, inner) => is_gap(inner)
  | Unknown(Internal)
  | Unknown(Hole(EmptyHole))
  | Unknown(SynSwitch) => true
  | _ => false
  };

let empty_result = {
  omitted: Id.Set.empty,
  gamma: VarMap.empty,
  psi: gap,
  context: Ctx.empty,
  ana: gap,
};

let meet = (ctx: Ctx.t, left: Typ.t, right: Typ.t): Typ.t =>
  if (is_gap(left)) {
    right;
  } else if (is_gap(right)) {
    left;
  } else {
    Typ.meet(ctx, left, right) |> Option.value(~default=left);
  };

let gamma_add = (ctx: Ctx.t, gamma: gamma, name: string, ty: Typ.t): gamma =>
  if (is_gap(ty)) {
    gamma;
  } else {
    switch (VarMap.lookup(gamma, name)) {
    | None => VarMap.extend(gamma, (name, ty))
    | Some(old) => VarMap.update(gamma, name, _ => meet(ctx, old, ty))
    };
  };

let gamma_join = (ctx: Ctx.t, left: gamma, right: gamma): gamma =>
  List.fold_left(
    (acc, (name, ty)) => gamma_add(ctx, acc, name, ty),
    left,
    VarMap.to_list(right),
  );

let gamma_remove = (gamma: gamma, names: list(string)): gamma =>
  VarMap.filter(((name, _)) => !List.mem(name, names), gamma);

let close_sum_gaps = ty =>
  Typ.map_term(
    ~f_typ=
      (continue, ty) =>
        switch (Typ.term_of(ty)) {
        | Sum(items)
            when List.exists(
                   fun | ConstructorMap.Variant(_, _, _) => true | _ => false,
                   items,
                 ) =>
          {
            ...ty,
            term:
              Sum(
                List.filter(
                  fun | ConstructorMap.BadEntry(_) => false | _ => true,
                  items,
                ),
              ),
          }
        | _ => continue(ty)
        },
    ty,
  );

let context_key =
  fun
  | Ctx.VarEntry({name, _}) => Some((0, name))
  | Ctx.ConstructorEntry({name, _}) => Some((1, name))
  | Ctx.TVarEntry({name, _}) => Some((2, name))
  | Ctx.LivelitEntry(_) => None;

let context_join = (left: Ctx.t, right: Ctx.t): Ctx.t => {
  ...left,
  entries:
    List.fold_left(
      (entries, entry) =>
        switch (context_key(entry)) {
        | Some(key)
            when List.exists(e => context_key(e) == Some(key), entries) => entries
        | _ => entries @ [entry]
        },
      left.entries,
      right.entries,
    ),
};

let context_join_branches = (ctx, left: Ctx.t, right: Ctx.t): Ctx.t => {
  let merge = (old, entry) =>
    switch (old, entry) {
    | (
        Ctx.TVarEntry({name, kind: Singleton(a), _} as old),
        Ctx.TVarEntry({name: other, kind: Singleton(b), _}),
      ) when name == other =>
      Ctx.TVarEntry({
        ...old,
        kind: Singleton(close_sum_gaps(meet(ctx, a, b))),
      })
    | _ => old
    };
  {
    ...left,
    entries:
      List.fold_left(
        (entries, entry) =>
          switch (context_key(entry)) {
          | Some(key)
              when List.exists(e => context_key(e) == Some(key), entries) =>
            List.map(
              old => context_key(old) == Some(key) ? merge(old, entry) : old,
              entries,
            )
          | _ => entries @ [entry]
          },
        left.entries,
        right.entries,
      ),
  };
};

let result_join = (ctx: Ctx.t, left: result, right: result): result => {
  omitted: Id.Set.union(left.omitted, right.omitted),
  gamma: gamma_join(ctx, left.gamma, right.gamma),
  psi: meet(ctx, left.psi, right.psi),
  context: context_join(left.context, right.context),
  ana: meet(ctx, left.ana, right.ana),
};

let results_join = (ctx: Ctx.t, results: list(result)): result =>
  List.fold_left(result_join(ctx), empty_result, results);

let queried = (ty: Typ.t): result => {
  ...empty_result,
  psi: ty,
  ana: ty,
};

let rec sum_definition = (params, ty) =>
  switch (Typ.term_of(ty)) {
  | TypFun(param, body) => sum_definition(params @ TPat.binders_of(param), body)
  | Parens(body)
  | Rec(_, body) => sum_definition(params, body)
  | Sum(constructors) => Some((params, constructors))
  | _ => None
  };

let rec constructor_payload = (query: Typ.t) =>
  switch (Typ.term_of(query)) {
  | Poly(_, body)
  | Parens(body) => constructor_payload(body)
  | Arrow(payload, _) => Some(payload)
  | _ => None
  };

let rec unconstrained_result = (query: Typ.t): bool =>
  switch (Typ.term_of(query)) {
  | Poly(_, body)
  | Parens(body) => unconstrained_result(body)
  | Arrow(_, result) =>
    switch (Typ.term_of(result)) {
    | TypParamAp(_, {term: TypTuple(args), _}) => List.for_all(is_gap, args)
    | TypParamAp(_, arg) => is_gap(arg)
    | _ => is_gap(result)
    }
  | _ => is_gap(query)
  };

let rec minimal_alias =
        (name, payload, ~keep_link, definition: Typ.t): Typ.t =>
  switch (Typ.term_of(definition)) {
  | TypFun(param, body) =>
    {
      ...definition,
      term: TypFun(param, minimal_alias(name, payload, ~keep_link, body)),
    }
  | Rec(_, body) => minimal_alias(name, payload, ~keep_link, body)
  | Sum(constructors) =>
    {
      ...definition,
      term:
        Sum(
          List.map(
            fun
            | ConstructorMap.Variant(constructor, ann, arg)
                when constructor == name =>
              ConstructorMap.Variant(
                constructor,
                ann,
                Option.map(
                  arg =>
                    switch (payload, Typ.term_of(arg)) {
                    | (Some(payload), _) when !is_gap(payload) => payload
                    | (_, Var(_)) when keep_link => arg
                    | _ => gap
                    },
                  arg,
                ),
              )
            | _ => ConstructorMap.BadEntry(gap),
            constructors,
          ),
        ),
    }
  | _ => definition
  };

let constructor_from_alias = (ctx: Ctx.t, name: string, query) =>
  List.find_map(
    fun
    | Ctx.TVarEntry({name: alias, kind: Singleton(definition), _} as entry) => {
      let constructor =
        switch (Typ.term_of(definition)) {
      | Var(constructor)
          when constructor == name && Ctx.lookup_alias(ctx, constructor) == None =>
        Some(
          {
            name,
            id: Typ.rep_id(definition),
            typ: Var(alias) |> Typ.temp,
            custom_statics: None,
          }: Ctx.var_entry,
        )
      | TypParamAp({term: Var(constructor), _}, payload)
          when constructor == name && Ctx.lookup_alias(ctx, constructor) == None =>
        Some(
          {
            name,
            id: Typ.rep_id(definition),
            typ: Arrow(payload, Var(alias) |> Typ.temp) |> Typ.temp,
            custom_statics: None,
          }: Ctx.var_entry,
        )
      | _ =>
        Option.bind(sum_definition([], definition), ((params, constructors)) =>
          Ctx.add_ctrs_with_params(Ctx.empty, alias, params, constructors)
          |> Ctx.lookup_ctr(_, name)
        )
        };
      Option.map(
        constructor => (
          constructor,
          {
            ...entry,
            kind:
              Singleton(
                minimal_alias(
                  name,
                  constructor_payload(query),
                  ~keep_link=unconstrained_result(query),
                  definition,
                ),
              ),
          },
        ),
        constructor,
      );
    }
    | _ => None,
    ctx.entries,
  );

let schema = (info: Info.exp): Typ.t =>
  switch (Exp.term_of(info.user_term)) {
  | Constructor(name, _) =>
    constructor_from_alias(info.ctx, name, gap)
    |> Option.map(((constructor, _)) => (constructor: Ctx.var_entry).typ)
    |> Option.value(~default=info.ty)
  | Var(name) =>
    Ctx.lookup_var(info.ctx, name)
    |> Option.map((entry: Ctx.var_entry) => entry.typ)
    |> Option.value(~default=info.ty)
  | _ => info.ty
  };

let context_for_name = (ctx: Ctx.t, name: string, query): Ctx.t =>
  switch (Ctx.lookup_var(ctx, name)) {
  | Some(entry) => Ctx.extend(Ctx.empty, Ctx.VarEntry(entry))
  | None =>
    switch (constructor_from_alias(ctx, name, query)) {
    | Some((constructor, alias)) =>
      Ctx.empty
      |> Ctx.extend(_, Ctx.TVarEntry(alias))
      |> Ctx.extend(_, Ctx.ConstructorEntry(constructor))
    | None =>
      switch (Ctx.lookup_ctr(ctx, name)) {
      | Some(entry) => Ctx.extend(Ctx.empty, Ctx.ConstructorEntry(entry))
      | None => Ctx.empty
      }
    }
  };

let typ_children = (ty: Typ.t): list(Typ.t) =>
  switch (Typ.term_of(ty)) {
  | Parens(t)
  | Projector(_, t)
  | List(t)
  | Poly(_, t)
  | TypFun(_, t)
  | Rec(_, t) => [t]
  | Arrow(a, b)
  | TupLabel(a, b)
  | TypParamAp(a, b)
  | ProdProjection(a, b)
  | ProdExtension(a, b) => [a, b]
  | Prod(ts)
  | TypTuple(ts) => ts
  | Sum(variants) =>
    List.map(
      fun
      | ConstructorMap.Variant(_, _, Some(t)) => t
      | ConstructorMap.Variant(_, _, None) => gap
      | ConstructorMap.BadEntry(t) => t,
      variants,
    )
  | Unknown(_)
  | Atom(_)
  | DrvQuoteTy(_)
  | Var(_)
  | ExplicitNonlabel
  | Label(_)
  | ProofOf(_)
  | Sig(_) => []
  };

let typ_rebuild = (shape: Typ.t, children: list(Typ.t)): Typ.t =>
  switch (Typ.term_of(shape), children) {
  | (Parens(_), [t]) => Parens(t) |> Typ.temp
  | (Projector(data, _), [t]) => Projector(data, t) |> Typ.temp
  | (List(_), [t]) => List(t) |> Typ.temp
  | (Poly(b, _), [t]) => Poly(b, t) |> Typ.temp
  | (TypFun(b, _), [t]) => TypFun(b, t) |> Typ.temp
  | (Rec(b, _), [t]) => Rec(b, t) |> Typ.temp
  | (Arrow(_, _), [a, b]) => Arrow(a, b) |> Typ.temp
  | (TupLabel(_, _), [a, b]) => TupLabel(a, b) |> Typ.temp
  | (TypParamAp(_, _), [a, b]) => TypParamAp(a, b) |> Typ.temp
  | (ProdProjection(_, _), [a, b]) => ProdProjection(a, b) |> Typ.temp
  | (ProdExtension(_, _), [a, b]) => ProdExtension(a, b) |> Typ.temp
  | (Prod(_), ts) => Prod(ts) |> Typ.temp
  | (TypTuple(_), ts) => TypTuple(ts) |> Typ.temp
  | (Sum(variants), ts) when List.length(variants) == List.length(ts) =>
    Sum(
      List.map2(
        (variant, t) =>
          switch (variant) {
          | ConstructorMap.Variant(name, ids, Some(_)) =>
            is_gap(t)
              ? ConstructorMap.BadEntry(gap)
              : ConstructorMap.Variant(name, ids, Some(t))
          | ConstructorMap.Variant(_, _, None) => variant
          | ConstructorMap.BadEntry(_) => ConstructorMap.BadEntry(t)
          },
        variants,
        ts,
      ),
    )
    |> Typ.temp
  | _ => shape
  };

let same_node = (left: Typ.t, right: Typ.t): bool => {
  let l = Typ.rep_id(left);
  let r = Typ.rep_id(right);
  !Id.equal(l, Id.invalid) && Id.equal(l, r) || left == right;
};

let rec find_path = (needle: Typ.t, haystack: Typ.t): option(path) =>
  switch (Typ.term_of(needle), Typ.term_of(haystack)) {
  | (Parens(inner), _)
  | (Projector(_, inner), _) => find_path(inner, haystack)
  | (_, Parens(inner))
  | (_, Projector(_, inner)) => find_path(needle, inner)
  | _ when same_node(needle, haystack) => Some([])
  | _ =>
    let rec scan = (i, children) =>
      switch (children) {
      | [] => None
      | [child, ...rest] =>
        switch (find_path(needle, child)) {
        | Some(path) => Some([i, ...path])
        | None => scan(i + 1, rest)
        }
      };
    scan(0, typ_children(haystack));
  };

let rec find_path_right = (needle: Typ.t, haystack: Typ.t): option(path) =>
  if (same_node(needle, haystack)) {
    Some([]);
  } else {
    typ_children(haystack)
    |> List.mapi((i, child) => (i, child))
    |> List.rev
    |> List.find_map(((i, child)) =>
         Option.map(path => [i, ...path], find_path_right(needle, child))
       );
  };

let rec expose = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(ty)) {
  | Parens(inner)
  | Projector(_, inner) => expose(inner)
  | Unknown(Hole(MultiHole(items))) =>
    switch (
      List.filter_map(fun | Grammar.Typ(ty) => Some(ty) | _ => None, items)
    ) {
    | [inner] => expose(inner)
    | _ => ty
    }
  | _ => ty
  };

let rec find_shape_path = (needle: Typ.t, haystack: Typ.t): option(path) =>
  if (Typ.equal(expose(needle), expose(haystack))) {
    Some([]);
  } else {
    typ_children(expose(haystack))
    |> List.mapi((i, child) => (i, child))
    |> List.find_map(((i, child)) =>
         Option.map(path => [i, ...path], find_shape_path(needle, child))
       );
  };

let rec project = (query: Typ.t, path: path): Typ.t =>
  switch (path) {
  | [] => expose(query)
  | [i, ...rest] =>
    switch (List.nth_opt(typ_children(expose(query)), i)) {
    | Some(child) => project(child, rest)
    | None => gap
    }
  };

let rec has_path = (query: Typ.t, path: path): bool =>
  switch (path) {
  | [] => true
  | [i, ...rest] =>
    switch (List.nth_opt(typ_children(expose(query)), i)) {
    | Some(child) => has_path(child, rest)
    | None => false
    }
  };

let rec lift = (shape: Typ.t, path: path, value: Typ.t): Typ.t =>
  switch (Typ.term_of(shape)) {
  | Parens(inner) => Parens(lift(inner, path, value)) |> Typ.temp
  | Projector(projector, inner) =>
    Projector(projector, lift(inner, path, value)) |> Typ.temp
  | _ =>
    switch (path) {
    | [] => value
    | [i, ...rest] =>
      typ_children(shape)
      |> List.mapi((j, child) => j == i ? lift(child, rest, value) : gap)
      |> typ_rebuild(shape)
    }
  };

let lens_down = (lens: lens, child_shape: Typ.t, query: Typ.t): Typ.t =>
  switch (lens.parent_path, lens.child_path) {
  | (Some(parent_path), Some(child_path)) =>
    lift(child_shape, child_path, project(query, parent_path))
  | _ => gap
  };

let lens_up = (lens: lens, parent_shape: Typ.t, supplied: Typ.t): Typ.t =>
  switch (lens.parent_path, lens.child_path) {
  | (Some(parent_path), Some(child_path)) =>
    lift(parent_shape, parent_path, project(supplied, child_path))
  | _ => gap
  };

let rec empty_query = (query: Typ.t): bool =>
  if (is_gap(query)) {
    true;
  } else {
    let children = typ_children(expose(query));
    children != [] && List.for_all(empty_query, children);
  };

let rec query_shell = (shape: Typ.t): Typ.t =>
  switch (Typ.term_of(shape)) {
  | Label(_)
  | ExplicitNonlabel => shape
  | _ =>
    let children = typ_children(shape);
    children == []
      ? gap : typ_rebuild(shape, List.map(query_shell, children));
  };

let rec route_query = (_ctx, parent: Typ.t, child: Typ.t, query: Typ.t): Typ.t =>
  switch (find_path(child, parent)) {
  | Some(path) =>
    let routed = project(query, path);
    if (!empty_query(routed)) {
      routed;
    } else {
      switch (find_shape_path(child, query)) {
      | Some(path) => project(query, path)
      | None => routed
      };
    }
  | None =>
    switch (find_shape_path(child, query)) {
    | Some(path) => project(query, path)
    | None =>
      switch (find_path(parent, child)) {
    | Some(path) => lift(child, path, query)
    | None =>
      let ps = typ_children(expose(parent));
      let cs = typ_children(expose(child));
      let qs = typ_children(expose(query));
      let routed =
        List.length(ps) == List.length(cs)
        && List.length(ps) == List.length(qs)
          ? List.map2(
              (p, (c, q)) => route_query(_ctx, p, c, q),
              ps,
              List.combine(cs, qs),
            )
          : List.map(route_query(_ctx, parent, _, query), cs);
      routed == [] || List.for_all(empty_query, routed)
        ? gap : typ_rebuild(child, routed);
      }
    }
  };

let rec subtract = (ctx: Ctx.t, query: Typ.t, supplied: Typ.t): Typ.t =>
  if (is_gap(query)) {
    gap;
  } else if (is_gap(supplied)) {
    query;
  } else {
    switch (Typ.term_of(query), Typ.term_of(supplied)) {
    | (Parens(q), _) => subtract(ctx, q, supplied)
    | (_, Parens(s)) => subtract(ctx, query, s)
    | _ =>
      let qs = typ_children(query);
      let ss = typ_children(supplied);
      if (qs != [] && List.length(qs) == List.length(ss)) {
        typ_rebuild(query, List.map2(subtract(ctx), qs, ss));
      } else {
        let query' = Typ.weak_head_normalize(ctx, query);
        let supplied' = Typ.weak_head_normalize(ctx, supplied);
        if (!Typ.equal(query, query') || !Typ.equal(supplied, supplied')) {
          subtract(ctx, query', supplied');
        } else {
          Typ.meet(ctx, query, supplied) == None ? query : gap;
        };
      };
    };
  };

let query_residual = (ctx, query, supplied) => {
  let query = subtract(ctx, query, supplied);
  empty_query(query) ? gap : query;
};

let query_overlap = (ctx, query, supplied) => {
  let supplied =
    switch (Typ.term_of(query)) {
    | Sum(_) => Typ.weak_head_normalize(ctx, supplied)
    | _ => supplied
    };
  let overlap =
    query_residual(ctx, query, query_residual(ctx, query, supplied));
  !unconstrained_result(Arrow(gap, query) |> Typ.temp)
  && unconstrained_result(Arrow(gap, overlap) |> Typ.temp)
    ? gap : overlap;
};

let ids_of_typ = (actual: Typ.t, query: Typ.t): Id.Set.t => {
  let rec go = (actual, query) =>
    if (is_gap(query)) {
      Id.Set.singleton(Typ.rep_id(actual));
    } else {
      let omitted_entries =
        switch (Typ.term_of(actual), Typ.term_of(query)) {
        | (Sum(actual), Sum(query)) when List.length(actual) == List.length(query) =>
          List.map2(
            (actual, query) =>
              switch (actual, query) {
              | (ConstructorMap.Variant(_, ann, _), ConstructorMap.BadEntry(q))
                  when is_gap(q) =>
                List.fold_left(
                  (ids, id) => Id.Set.add(id, ids),
                  Id.Set.empty,
                  ann.ids,
                )
              | _ => Id.Set.empty
              },
            actual,
            query,
          )
          |> List.fold_left(Id.Set.union, Id.Set.empty)
        | _ => Id.Set.empty
        };
      let actual_children = typ_children(actual);
      let query_children = typ_children(query);
      let omitted_children =
        List.length(actual_children) == List.length(query_children)
          ? List.map2(go, actual_children, query_children)
            |> List.fold_left(Id.Set.union, Id.Set.empty)
          : Id.Set.empty;
      Id.Set.union(omitted_entries, omitted_children);
    };
  go(actual, query);
};

let source_result = (info: Info.exp, query: Typ.t): result =>
  if (is_gap(query)) {
    {
      ...empty_result,
      omitted: Id.Set.singleton(Exp.rep_id(info.user_term)),
    };
  } else {
    let names =
      switch (VarMap.to_list(info.co_ctx), Exp.term_of(info.user_term)) {
      | ([], Constructor(name, _)) => [name]
      | (uses, _) => List.map(fst, uses)
      };
    if (names == []) {
      {
        ...queried(info.ty),
        ana: query,
      };
    } else {
      {
        ...queried(query),
        gamma:
          List.fold_left(
            (gamma, name) =>
              Ctx.lookup_ctr(info.ctx, name) == None
                ? gamma_add(info.ctx, gamma, name, query) : gamma,
            VarMap.empty,
            names,
          ),
        context:
          List.map(context_for_name(info.ctx, _, query), names)
          |> List.fold_left(context_join, Ctx.empty),
      };
    };
  };

let of_info_mode =
  fun
  | Info.SliceKeep => Keep
  | Info.SliceOmit => Omit
  | Info.SliceSource => Source
  | Info.SliceTrack => Track
  | Info.SliceMap => Map
  | Info.SlicePrune => Prune
  | Info.SliceAscribe => Ascribe
  | Info.SliceAlias => Alias
  | Info.SliceAlternative => Alternative
  | Info.SliceMatched => Matched;

let lens = (parent_shape: Typ.t, child_shape: Typ.t): option(lens) =>
  switch (find_path(child_shape, parent_shape)) {
  | Some(parent_path) =>
    Some({
      parent_path: Some(parent_path),
      child_path: Some([]),
    })
  | None =>
    switch (find_path(parent_shape, child_shape)) {
    | Some(child_path) =>
      Some({
        parent_path: Some([]),
        child_path: Some(child_path),
      })
    | None => None
    }
  };

let take_children =
    (~parent: Exp.t, ~parent_shape: Typ.t, m: Id.Map.t(Info.t)) => {
  ignore(parent_shape);
  let id = Exp.rep_id(parent);
  switch (Id.Map.find_opt(id, m)) {
  | Some(Info.InfoSliceScratch({children, _})) =>
    (children, Id.Map.remove(id, m))
  | Some(Info.InfoExp({slice_children, _})) => (slice_children, m)
  | _ => ([], m)
  };
};

let record_child =
    (mode, ~pattern=None, ~parent: Exp.t, (info, elab, m): exp_result)
    : exp_result => {
  let parent_id = Exp.rep_id(parent);
  let child_id = Exp.rep_id(info.user_term);
  if (Id.equal(parent_id, child_id)) {
    (info, elab, m);
  } else {
    let prior =
      switch (Id.Map.find_opt(parent_id, m)) {
      | Some(Info.InfoSliceScratch({children, _})) => children
      | _ => []
      };
    let edge: Info.slice_child = {
      mode:
        switch (mode) {
        | Keep => Info.SliceKeep
        | Omit => Info.SliceOmit
        | Source => Info.SliceSource
        | Track => Info.SliceTrack
        | Map => Info.SliceMap
        | Prune => Info.SlicePrune
        | Ascribe => Info.SliceAscribe
        | Alias => Info.SliceAlias
        | Alternative => Info.SliceAlternative
        | Matched => Info.SliceMatched
        },
      child: child_id,
      pattern,
    };
    let prior =
      List.filter(
        (e: Info.slice_child) => !Id.equal(e.child, child_id),
        prior,
      );
    let patterns =
      switch (Id.Map.find_opt(parent_id, m)) {
      | Some(Info.InfoSliceScratch({patterns, _})) => patterns
      | _ => []
      };
    (
      info,
      elab,
      Id.Map.add(
        parent_id,
        Info.InfoSliceScratch({
          children: prior @ [edge],
          patterns,
        }),
        m,
      ),
    );
  };
};

let keep = (~parent, child, k) => k(record_child(Keep, ~parent, child));
let omit = (~parent, child, k) => k(record_child(Omit, ~parent, child));
let source_child = (~parent, child, k) =>
  k(record_child(Source, ~parent, child));
let track = (~parent, child, k) => k(record_child(Track, ~parent, child));
let map = (~parent, child, k) => k(record_child(Map, ~parent, child));
let prune = (~parent, child, k) => k(record_child(Prune, ~parent, child));
let ascribe = (~parent, child, k) => k(record_child(Ascribe, ~parent, child));
let alias = (~parent, child, k) => k(record_child(Alias, ~parent, child));
let matched = (~parent, child, k) => k(record_child(Matched, ~parent, child));
let alternative = (~parent, child, k) =>
  k(record_child(Alternative, ~parent, child));

let pattern = (~parent, (info: Info.pat, elab, m)) => {
  let parent_id = Exp.rep_id(parent);
  let pattern_id = Pat.rep_id(info.user_term);
  let (children, patterns) =
    switch (Id.Map.find_opt(parent_id, m)) {
    | Some(Info.InfoSliceScratch({children, patterns})) => (
        children,
        patterns,
      )
    | _ => ([], [])
    };
  let patterns =
    List.exists(Id.equal(pattern_id), patterns)
      ? patterns : patterns @ [pattern_id];
  (
    info,
    elab,
    Id.Map.add(
      parent_id,
      Info.InfoSliceScratch({
        children,
        patterns,
      }),
      m,
    ),
  );
};

let bindings_of = (~ctx: Ctx.t, pattern: Info.pat) =>
  Ctx.added_bindings(pattern.ctx, ctx).entries
  |> List.filter_map(
       fun
       | Ctx.VarEntry({name, id, typ, _}) =>
         Some({name, id, path: find_path(typ, pattern.ty)}: binding)
       | _ => None,
     );

let local_binding = (m, path, name) =>
  Id.Map.fold(
    (_, info, found) =>
      found
      || switch (info) {
         | Info.InfoPat(pattern) =>
           List.exists(Id.Set.mem(_, path), pattern.ancestors)
           && Pat.bindings(pattern.user_term)
              |> List.exists((binding: Binding.t) => binding.name == name)
         | _ => false
         },
    m,
    false,
  );

let record_binding = (mode, ~parent, child, k) => {
  let (_, _, m) = child;
  let patterns =
    switch (Id.Map.find_opt(Exp.rep_id(parent), m)) {
    | Some(Info.InfoSliceScratch({patterns, _})) => patterns
    | _ => []
    };
  let prior =
    switch (Id.Map.find_opt(Exp.rep_id(parent), m)) {
    | Some(Info.InfoSliceScratch({children, _})) => children
    | _ => []
    };
  let index =
    mode == Alternative
      ? List.length(
          List.filter(
            (edge: Info.slice_child) => edge.mode == Info.SliceAlternative,
            prior,
          ),
        )
      : 0;
  k(record_child(mode, ~pattern=List.nth_opt(patterns, index), ~parent, child));
};

let source_binding = (~parent, child, k) =>
  record_binding(Source, ~parent, child, k);
let bound_child = (~parent, child, k) =>
  record_binding(Keep, ~parent, child, k);
let omitted_binding = (~parent, child, k) =>
  record_binding(Omit, ~parent, child, k);
let alternative_binding = (~parent, child, k) =>
  record_binding(Alternative, ~parent, child, k);

let binding_demand = (ctx, bindings, shape, gamma) =>
  List.fold_left(
    (demand, binding: binding) =>
      switch (VarMap.lookup(gamma, binding.name), binding.path) {
      | (Some(query), Some(path)) =>
        meet(ctx, demand, lift(shape, path, query))
      | (Some(query), None) when List.length(bindings) == 1 =>
        meet(ctx, demand, query)
      | _ => demand
      },
    gap,
    bindings,
  );

let pattern_focus_demand = (m, root, focus, shape, query) =>
  switch (focus) {
  | Some(id) =>
    switch (Id.Map.find_opt(id, m)) {
    | Some(Info.InfoPat(info))
        when Id.equal(id, root) || List.exists(Id.equal(root), info.ancestors) =>
      route_query(info.ctx, info.ty, shape, query)
    | _ => gap
    }
  | None => gap
  };

let rec pattern_omissions =
        (~covered=false, gamma: gamma, pattern: Pat.t): Id.Set.t =>
  switch (Pat.term_of(pattern)) {
  | Parens(inner)
  | Projector(_, inner)
  | TupLabel(_, inner) => pattern_omissions(~covered, gamma, inner)
  | Asc(inner, _) => pattern_omissions(~covered=true, gamma, inner)
  | _ =>
    let bindings = Pat.bindings(pattern);
    let used =
      List.exists(
        (binding: Binding.t) => VarMap.contains(gamma, binding.name),
        bindings,
      );
    if (!used) {
      covered ? Id.Set.singleton(Pat.rep_id(pattern)) : Id.Set.empty;
    } else {
      switch (Pat.term_of(pattern)) {
      | Ap(_, inner) => pattern_omissions(~covered, gamma, inner)
    | Tuple(items)
    | ListLit(items) =>
      List.map(pattern_omissions(~covered, gamma), items)
      |> List.fold_left(Id.Set.union, Id.Set.empty)
    | Cons(head, tail) =>
      Id.Set.union(
        pattern_omissions(~covered, gamma, head),
        pattern_omissions(~covered, gamma, tail),
      )
    | _ => Id.Set.empty
    };
    };
  };

let pattern_has_ascription = pattern =>
  switch (
    Pat.map_term(
      ~f_pat=
        (continue, pattern) =>
          switch (Pat.term_of(pattern)) {
          | Asc(_, _) => raise(Pattern_ascription)
          | _ => continue(pattern)
          },
      pattern,
    )
  ) {
  | exception Pattern_ascription => true
  | _ => false
  };

let binding_omissions =
    (children: list(child), gamma: gamma, demands): Id.Set.t =>
  List.fold_left(
    (omitted, child) => {
      let demanded =
        List.filter(
          (binding: binding) =>
            VarMap.contains(gamma, binding.name),
          child.bindings,
        );
      let omitted =
        List.fold_left(
          (omitted, binding: binding) =>
            VarMap.contains(gamma, binding.name)
              ? omitted : Id.Set.add(binding.id, omitted),
          omitted,
          child.bindings,
        );
      let omitted =
        switch (child.pattern) {
        | Some(pattern) =>
          let id = Pat.rep_id(pattern.user_term);
          let shape = child.mode == Source ? child.node.shape : pattern.ty;
          let demand =
            List.find_map(
              ((pattern, demand, _)) =>
                pattern == Some(id) ? Some(demand) : None,
              demands,
            )
            |> Option.value(~default=gap);
          Id.Set.union(
            omitted,
            Id.Set.union(
              pattern_omissions(gamma, pattern.user_term),
              ids_of_typ(shape, demand),
            ),
          );
        | None => omitted
        };
      switch (child.pattern) {
      | Some(pattern)
          when
            demanded == []
            && !
                 List.exists(
                   ((pattern_id, demand, _)) =>
                     pattern_id == Some(Pat.rep_id(pattern.user_term))
                     && !is_gap(demand),
                   demands,
                 ) =>
        Id.Set.add(Pat.rep_id(pattern.user_term), omitted)
      | _ => omitted
      };
    },
    Id.Set.empty,
    children,
  );

let rec alias_source = (definition: Typ.t): Typ.t =>
  switch (Typ.term_of(definition)) {
  | Rec(_, body) => alias_source(body)
  | _ => definition
  };

let rec unused_type_parameters = (definition: Typ.t, minimal: Typ.t) =>
  switch (Typ.term_of(definition), Typ.term_of(minimal)) {
  | (Rec(_, body), _) => unused_type_parameters(body, minimal)
  | (TypFun(pattern, body), TypFun(_, minimal_body)) =>
    let free = Typ.free_vars(minimal_body);
    TPat.binders_of(pattern)
    |> List.filter_map(binder =>
         switch (TPat.tyvar_of_utpat(binder)) {
         | Some(name) when !List.mem(name, free) => Some(TPat.rep_id(binder))
         | _ => None
         }
       )
    |> List.fold_left((ids, id) => Id.Set.add(id, ids), Id.Set.empty)
    |> Id.Set.union(unused_type_parameters(body, minimal_body))
  | _ => Id.Set.empty
  };

let alias_omissions = (children: list(child), context: Ctx.t): Id.Set.t =>
  children
  |> List.concat_map(child => child.aliases)
  |> List.map((alias: Ctx.tvar_entry) =>
       switch (alias.kind) {
       | Abstract => Id.Set.empty
       | Singleton(definition) =>
         switch (
           List.find_map(
             fun
             | Ctx.TVarEntry({name, kind: Singleton(minimal), _})
                 when name == alias.name => Some(minimal)
             | _ => None,
             context.entries,
           )
         ) {
         | Some(minimal) =>
           Id.Set.union(
             ids_of_typ(alias_source(definition), alias_source(minimal)),
             unused_type_parameters(definition, minimal),
           )
         | None =>
           Id.Set.empty
           |> Id.Set.add(alias.id)
           |> Id.Set.add(Typ.rep_id(alias_source(definition)))
         }
       }
     )
  |> List.fold_left(Id.Set.union, Id.Set.empty);

let rec matched_body =
        (
          ~replace_bound=false,
          ctx,
          bound: list(string),
          schema: Typ.t,
          query: Typ.t,
        )
        : (Typ.t, list((string, Typ.t))) => {
  let schema = expose(schema);
  let query = expose(query);
  switch (Typ.term_of(schema)) {
  | Var(name) when List.mem(name, bound) => (
      replace_bound ? query : schema,
      [(name, query)],
    )
  | _ =>
    let ss = typ_children(schema);
    let qs = typ_children(query);
    if (
      ss != []
      && Typ.cls_of_term(Typ.term_of(schema))
         == Typ.cls_of_term(Typ.term_of(query))
      && List.length(ss) == List.length(qs)
    ) {
      let pairs =
        List.map2(matched_body(~replace_bound, ctx, bound), ss, qs);
      (
        typ_rebuild(schema, List.map(fst, pairs)),
        List.concat_map(snd, pairs),
      );
    } else {
      switch (Typ.term_of(query)) {
      | Sum(_) =>
        let schema = Typ.weak_head_normalize(ctx, schema);
        switch (Typ.term_of(schema)) {
        | Sum(_) => matched_body(~replace_bound, ctx, bound, schema, query)
        | _ => (query, [])
        }
      | _ => (query, [])
      };
    };
  };
};

let matched_type_application =
    (~implicit=false, ctx: Ctx.t, fn: node, args: Typ.t, query: Typ.t)
    : result => {
  let rec peel = (binders, schema) =>
    switch (Typ.term_of(schema)) {
    | Poly(binder, body) => peel(binders @ [binder], body)
    | Parens(inner) => peel(binders, inner)
    | _ => (binders, schema)
    };
  let (binders, schema) = peel([], fn.typ);
  let flat_binders = List.concat_map(TPat.binders_of, binders);
  let names = List.filter_map(TPat.tyvar_of_utpat, flat_binders);
  let (matched, constraints) =
    implicit && names == []
      ? (query, [])
      : matched_body(~replace_bound=implicit, ctx, names, schema, query);
  let constraint_for = name =>
    constraints
    |> List.filter_map(((n, ty)) => n == name ? Some(ty) : None)
    |> List.fold_left(meet(ctx), gap);
  let matched =
    implicit
      ? matched
      : Typ.map_term(
          ~f_typ=
            (continue, ty) =>
              switch (Typ.term_of(ty)) {
              | Var(name)
                  when List.mem(name, names) && is_gap(constraint_for(name)) =>
                gap
              | _ => continue(ty)
              },
          matched,
        );
  let fn_query =
    List.fold_right(
      (binder, body) =>
        Poly(
          TPat.map_term(
            ~f_tpat=
              (continue, binder) =>
                switch (binder.term) {
                | Var(name) when implicit || is_gap(constraint_for(name)) =>
                  {...binder, term: EmptyHole}
                | _ => continue(binder)
                },
            binder,
          ),
          body,
        )
        |> Typ.temp,
      binders,
      matched,
    );
  let slice = fn.dispatch(fn_query);
  let actual_args =
    switch (Typ.term_of(args)) {
    | TypTuple(ts) => ts
    | _ => [args]
    };
  let omitted =
    List.map2(
      (binder, arg) =>
        switch (TPat.tyvar_of_utpat(binder)) {
        | Some(name) => ids_of_typ(arg, constraint_for(name))
        | None => Id.Set.empty
        },
      List.length(flat_binders) == List.length(actual_args)
        ? flat_binders : [],
      List.length(flat_binders) == List.length(actual_args)
        ? actual_args : [],
    )
    |> List.fold_left(Id.Set.union, Id.Set.empty);
  {
    ...slice,
    omitted: Id.Set.union(slice.omitted, omitted),
    psi: query,
  };
};

let applied_type = (ctx, fn, args) => {
  let (binder, body) = MatchedTyp.poly_pair_tolerant(ctx, fn);
  switch (binder) {
  | None => body
  | Some(binder) =>
    let binders = TPat.binders_of(binder);
    let args =
      switch (Typ.term_of(args)) {
      | TypTuple(args) when List.length(binders) > 1 => args
      | _ => [args]
      };
    List.length(args) == List.length(binders)
      ? Typ.subst_many(args, binders, body) : gap;
  };
};

let slice_forward =
    (
      ~direction,
      ~pattern_focus=false,
      ~focus_query,
      ~path=Id.Set.empty,
      ctx: Ctx.t,
      parent_shape: Typ.t,
      children: list(child),
      query: Typ.t,
    )
    : result => {
  let forward =
    children
    |> List.map(child => {
         let upwards = slice => {
           ...slice,
           psi:
             switch (child.lens) {
             | Some(lens) => lens_up(lens, parent_shape, slice.psi)
             | None =>
               empty_query(slice.psi)
                 ? gap
                 : route_query(
                     ctx,
                     child.node.shape,
                     parent_shape,
                     slice.psi,
                   )
             },
         };
         if (Id.Set.mem(child.node.id, path)
             && direction == `Ana
             && child.mode == Omit
             && Typ.meet(ctx, child.node.ana, focus_query) != None) {
           {
             ...empty_result,
             omitted: Id.Set.singleton(child.node.id),
           };
         } else if (Id.Set.mem(child.node.id, path)) {
           upwards(
             child.node.dispatch(
               direction == `Ana
               && !pattern_focus
               && child.mode != Ascribe
                 ? route_query(
                     ctx,
                     parent_shape,
                     child.node.shape,
                     focus_query,
                   )
                 : gap,
             ),
           );
         } else {
           let follow = prune => {
             let child_query =
               switch (child.lens) {
               | Some(lens) =>
                 let routed = lens_down(lens, child.node.shape, query);
                 empty_query(routed)
                 &&
                 switch (lens.parent_path) {
                 | Some(path) => !has_path(query, path)
                 | None => true
                 }
                   ? route_query(ctx, parent_shape, child.node.shape, query)
                   : routed;
               | None => route_query(ctx, parent_shape, child.node.shape, query)
               };
             let slice =
               child.node.dispatch(
                 prune && empty_query(child_query) ? gap : child_query,
               );
             upwards(slice);
           };
           switch (child.mode) {
           | Omit => {
               ...empty_result,
               omitted: Id.Set.singleton(child.node.id),
             }
           | Source => empty_result
           | Track => empty_result
           | Map =>
             let child_query =
               route_query(ctx, parent_shape, child.node.shape, query);
             let child_query =
               empty_query(child_query) && typ_children(child.node.shape) != []
                 ? query_shell(child.node.shape)
                 : child_query;
             child.node.dispatch(child_query)
           | Alternative => empty_result
           | Ascribe => {
               ...child.node.dispatch(query),
               omitted: Id.Set.singleton(child.node.id),
               psi: query,
             }
           | Alias => follow(false)
           | Matched => {
               ...matched_type_application(
                 ~implicit=true,
                 ctx,
                 child.node,
                 TypTuple([]) |> Typ.temp,
                 Arrow(gap, query) |> Typ.temp,
               ),
               psi: query,
             }
           | Keep => follow(false)
           | Prune => follow(true)
           };
         }
       });
  let reverse =
    direction == `Ana
      ? children
        |> List.filter(child =>
             child.mode == Omit && Id.Set.mem(child.node.id, path)
             && Typ.meet(ctx, child.node.ana, focus_query) != None
           )
        |> List.concat_map(checked =>
             children
             |> List.filter_map(source =>
                  if (source.mode != Keep && source.mode != Matched) {
                    None;
                  } else {
                    Option.map(
                      path =>
                        source.node.dispatch(
                          lift(source.node.shape, path, focus_query),
                        ),
                      find_path(checked.node.ana, source.node.shape),
                    );
                  }
                )
           )
      : [];
  let result = results_join(ctx, forward @ reverse);
  let has_ascription = List.exists(child => child.mode == Ascribe, children);
  let annotation_query =
    direction == `Ana && !Id.Set.is_empty(path)
      ? Option.map(
          path => lift(parent_shape, path, focus_query),
          find_shape_path(focus_query, parent_shape),
        )
        |> Option.value(~default=result.psi)
      : result.psi;
  has_ascription
    ? {
      ...result,
      omitted:
        Id.Set.union(result.omitted, ids_of_typ(parent_shape, annotation_query)),
    }
    : result;
};

let slice_branches =
    (
      ~direction,
      ~path=Id.Set.empty,
      ctx: Ctx.t,
      branches: list(node),
      query: Typ.t,
    )
    : result => {
  let (slices, _) =
    List.fold_left(
      ((slices, residual), (branch: node)) => {
        let branch_query =
          (direction == `Ana
             ? !Id.Set.is_empty(path) : Id.Set.mem(branch.id, path))
          || empty_query(residual)
            ? gap
            : query_overlap(ctx, residual, branch.typ);
        let slice = branch.dispatch(branch_query);
        (slices @ [slice], query_residual(ctx, residual, slice.psi));
      },
      ([], query),
      branches,
    );
  let result = results_join(ctx, slices);
  {
    ...result,
    context:
      slices
      |> List.map(slice => slice.context)
      |> List.fold_left(context_join_branches(ctx), Ctx.empty),
  };
};

let rec compile =
        (
          ~direction=`Syn,
          ~support=Unsupported,
          ~seen=Id.Set.empty,
          ~focus=None,
          ~focus_query=gap,
          ~path=Id.Set.empty,
          m: Id.Map.t(Info.t),
          info: Info.exp,
        )
        : node => {
  let id = Exp.rep_id(info.user_term);
  if (Id.Set.mem(id, seen)) {
    {
      id,
      shape: info.elab_syn_ty,
      typ: schema(info),
      ana: info.ana,
      dispatch: query => source_result(info, query),
    };
  } else {
    let seen = Id.Set.add(id, seen);
    let pattern_focus =
      switch (focus) {
      | Some(id) =>
        switch (Id.Map.find_opt(id, m)) {
        | Some(Info.InfoPat(_)) => true
        | _ => false
        }
      | None => false
      };
    let children =
      info.slice_children
      |> List.filter_map((edge: Info.slice_child) =>
           switch (Id.Map.find_opt(edge.child, m)) {
           | Some(Info.InfoExp(child_info)) =>
             let pattern =
               Option.bind(edge.pattern, id =>
                 switch (Id.Map.find_opt(id, m)) {
                 | Some(Info.InfoPat(pattern)) => Some(pattern)
                 | _ => None
                 }
               );
             let mode = of_info_mode(edge.mode);
             let edge_lens =
               pattern != None && mode == Keep
                 ? Option.map(
                     parent_path => {
                       parent_path: Some(parent_path),
                       child_path: Some([]),
                     },
                     find_path_right(child_info.elab_syn_ty, info.elab_syn_ty),
                   )
                 : lens(info.elab_syn_ty, child_info.elab_syn_ty);
             let edge_lens =
               mode == Track && edge_lens == None
                 ? Option.map(
                     parent_path => {
                       parent_path: Some(parent_path),
                       child_path: Some([]),
                     },
                     find_path(child_info.ana, info.elab_syn_ty),
                   )
                 : edge_lens;
             Some({
               mode: mode == Track ? edge_lens == None ? Omit : Keep : mode,
               bindings:
                 Option.map(bindings_of(~ctx=info.ctx), pattern)
                 |> Option.value(~default=[]),
               aliases:
                 mode == Alias
                   ? Ctx.added_bindings(child_info.ctx, info.ctx).entries
                     |> List.filter_map(
                          fun | Ctx.TVarEntry(entry) => Some(entry) | _ => None
                        )
                   : [],
               pattern,
               lens: edge_lens,
               node:
                 compile(
                     ~direction,
                     ~support=
                       mode == Source
                       && switch (pattern) {
                          | Some(pattern) =>
                            pattern_has_ascription(pattern.user_term)
                          | None => false
                          }
                         ? BindingAscription
                         : mode == Ascribe ? ExpressionAscription : support,
                     ~seen,
                   ~focus,
                   ~focus_query,
                   ~path,
                   m,
                   child_info,
                 ),
             })
           | _ => None
           }
         );
    let sources = List.filter(c => c.mode == Source, children);
    let kept =
      List.filter_map(c => c.mode == Keep ? Some(c.node) : None, children);
    let alternatives =
      List.filter_map(
        c => c.mode == Alternative ? Some(c.node) : None,
        children,
      );
    let typ =
      switch (Exp.term_of(info.user_term), kept) {
      | (TypAp(_, args), [fn, ..._]) => applied_type(info.ctx, fn.typ, args)
      | _ =>
        children
        |> List.find_map(child =>
             child.mode == Matched
               ? Some(MatchedTyp.arrow_tolerant(info.ctx, child.node.typ) |> snd)
               : None
           )
        |> Option.value(~default=schema(info))
      };
    let dispatch = query => {
      let at_focus =
        switch (focus) {
        | Some(focus) => Id.equal(focus, id)
        | None => false
        };
      let query =
        at_focus
        && (
          direction == `Syn
          || empty_query(query)
          || Typ.meet(info.ctx, info.elab_syn_ty, focus_query) != None
        )
          ? focus_query : query;
      if (is_gap(query) && (at_focus || !Id.Set.mem(id, path))) {
        {
          ...empty_result,
          omitted: Id.Set.singleton(id),
        };
      } else {
        let term = Exp.term_of(info.user_term);
        let forward =
          if (alternatives != []) {
            result_join(
              info.ctx,
              slice_branches(~direction, ~path, info.ctx, alternatives, query),
              slice_forward(
                ~path,
                ~direction,
                ~pattern_focus,
                ~focus_query,
                info.ctx,
                info.elab_syn_ty,
                List.filter(child => child.mode == Omit, children),
                gap,
              ),
            );
          } else {
            switch (term) {
            | TypAp(_, args) =>
              switch (kept) {
              | [fn, ..._] =>
                matched_type_application(info.ctx, fn, args, query)
              | [] => source_result(info, query)
              }
            | _ =>
              children == []
                ? source_result(info, query)
                : slice_forward(
                    ~path,
                    ~direction,
                    ~pattern_focus,
                    ~focus_query,
                    info.ctx,
                    info.elab_syn_ty,
                    children,
                    query,
                  )
            };
          };
        let forward =
          at_focus
          && direction == `Ana
          && (
            support == BindingAscription
            || (
              support == ExpressionAscription
              && List.for_all(
                   ((name, _)) =>
                     switch (Ctx.lookup_var(info.ctx, name)) {
                     | Some(_) => local_binding(m, path, name)
                     | None => false
                     },
                   VarMap.to_list(forward.gamma),
                 )
              && List.for_all(
                   fun
                   | Ctx.VarEntry({name, _}) => local_binding(m, path, name)
                   | _ => false,
                   forward.context.entries,
                 )
            )
          )
            ? {
              ...empty_result,
              omitted: Id.Set.add(id, forward.omitted),
              psi: query,
              ana: query,
            }
            : forward;
        let binding_children = List.filter(c => c.pattern != None, children);
        let demands =
          binding_children
          |> List.filter_map(child =>
               Option.map(
                 (pattern: Info.pat) => {
                   let shape =
                     child.mode == Source ? child.node.shape : pattern.ty;
                   let pattern_id = Pat.rep_id(pattern.user_term);
                   let body =
                     binding_demand(
                       info.ctx,
                       child.bindings,
                       shape,
                       forward.gamma,
                     );
                   let parent =
                     if (
                       child.mode != Keep
                       || (direction == `Ana && support == ExpressionAscription)
                     ) {
                       gap;
                     } else {
                       switch (find_path(shape, info.elab_syn_ty)) {
                       | Some(path) => project(query, path)
                       | None => gap
                       };
                     };
                   let source = meet(info.ctx, body, parent);
                   let focus_demand =
                     direction == `Ana
                     && Id.Set.mem(child.node.id, path)
                     && (
                       typ_children(focus_query) != []
                       || pattern_has_ascription(pattern.user_term)
                     )
                       ? child.node.dispatch(gap).psi
                       : gap;
                   (
                     Some(pattern_id),
                     direction == `Ana
                       ? meet(
                           info.ctx,
                           source,
                           meet(
                             info.ctx,
                             focus_demand,
                             pattern_focus_demand(
                               m,
                               pattern_id,
                               focus,
                               shape,
                               focus_query,
                             ),
                           ),
                         )
                       : source,
                     source,
                   );
                 },
                 child.pattern,
               )
             );
        let body_demand =
          List.map(((_, _, demand)) => demand, demands)
          |> List.fold_left(meet(info.ctx), gap);
        let source_query = empty_query(body_demand) ? gap : body_demand;
        let deps =
          List.map(
            source => source.node.dispatch(source_query),
            sources,
          );
        let combined =
          result_join(info.ctx, forward, results_join(info.ctx, deps));
        let omitted =
          Id.Set.union(
            combined.omitted,
            binding_omissions(binding_children, forward.gamma, demands),
          )
          |> Id.Set.union(alias_omissions(children, forward.context));
        let names =
          List.concat_map(
            child =>
              List.map(
                (binding: binding) => binding.name,
                child.bindings,
              ),
            binding_children,
          );
        {
          ...combined,
          omitted,
          gamma: gamma_remove(combined.gamma, names),
          psi: forward.psi,
          ana: query,
        };
      };
    };
    {
      id,
      shape: info.elab_syn_ty,
      typ,
      ana: info.ana,
      dispatch,
    };
  };
};

let exp_path = (m: Id.Map.t(Info.t), focus: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(focus, m)) {
  | Some(Info.InfoExp({ancestors, _})) =>
    List.fold_left(
      (ids, id) => Id.Set.add(id, ids),
      Id.Set.singleton(focus),
      ancestors,
    )
  | Some(Info.InfoPat({ancestors, _})) =>
    List.fold_left(
      (ids, id) => Id.Set.add(id, ids),
      Id.Set.singleton(focus),
      ancestors,
    )
  | _ => Id.Set.singleton(focus)
  };

let focus_shell_ids = (m: Id.Map.t(Info.t), focus: Id.t): Id.Set.t => {
  let rec go =
    fun
    | [id, ...rest] =>
      switch (Id.Map.find_opt(id, m)) {
      | Some(
          Info.InfoExp({
            user_term: {term: Parens(_) | Asc(_, _), _} as e,
            _,
          }),
        ) =>
        List.fold_left(
          (ids, id) => Id.Set.add(id, ids),
          go(rest),
          IdTagged.ids(e),
        )
      | _ => Id.Set.empty
      }
    | [] => Id.Set.empty;
  switch (Id.Map.find_opt(focus, m)) {
  | Some(Info.InfoExp({ancestors, _})) => go(ancestors)
  | _ => Id.Set.empty
  };
};

let compatible_query = (ctx: Ctx.t, actual: Typ.t, query: Typ.t): bool =>
  Typ.meet(ctx, actual, query) != None
  || (
    switch (
      Typ.term_of(Typ.weak_head_normalize(ctx, actual)),
      Typ.term_of(Typ.weak_head_normalize(ctx, query)),
    ) {
    | (Sum(_), Sum(_))
    | (Sum(_), Var(_))
    | (Sum(_), TypParamAp(_, _)) => true
    | _ => false
    }
  );

let validate = (~focus, ~direction, m, query) =>
  switch (focus) {
  | None => ()
  | Some(id) =>
    switch (Id.Map.find_opt(id, m)) {
    | None => raise(Focus_not_found(id))
    | Some(Info.InfoPat(_)) when direction == `Ana => ()
    | Some(Info.InfoExp(info)) =>
      if (direction == `Syn
          && !is_gap(query)
          && !compatible_query(info.ctx, info.elab_syn_ty, query)) {
        raise(Incompatible_query(query));
      }
    | Some(_) => raise(Wrong_focus_sort)
    }
  };

let with_run = (f: unit => 'a): 'a => f();

let slice =
    (
      ~focus: option(Id.t)=None,
      ~direction: direction=`Syn,
      root: exp_result,
      query,
    )
    : result => {
  let (root_info, _, m) = root;
  validate(~focus, ~direction, m, query);
  let focused = focus != None;
  let node =
    focused
      ? compile(
          ~direction,
          ~focus,
          ~focus_query=query,
          ~path=exp_path(m, Option.get(focus)),
          m,
          root_info,
        )
      : compile(~direction, m, root_info);
  let result = node.dispatch(focused ? gap : query);
  let result =
    direction == `Ana
      ? {
      ...result,
      ana: query,
    }
      : result;
  switch (direction, focus) {
  | (`Ana, Some(id)) =>
    switch (Id.Map.find_opt(id, m)) {
    | Some(Info.InfoPat(_)) => {
        ...result,
        omitted: Id.Set.add(id, result.omitted),
      }
    | _ => result
    }
  | _ => result
  };
};

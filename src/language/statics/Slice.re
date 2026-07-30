/* Slice.re - sliceable types.

   A sliceable type is a type with a lazy function to slice the type, getting
   terms which explain it. The slicing function is derived from the type
   checker's binding operators in Statics.re, automagically inferring slicing.

   `slice` explains a query at the root. Every node is wrapped by `mk`, and
   under it `assemble` turns the sub-terms a rule recorded into the two
   directions: forwards, a query on the constructed type; backwards, the demand
   a binder's body places on its definition. */

// A query's answer: dropped ids, minimal assumptions, sliced type.
type slice = {
  omitted: Id.Set.t,
  gamma: Ctx.t,
  psi: Typ.t,
};

// The focus of the whole query, threaded unchanged through every dispatch.
type env = {
  focus: option(Id.t),
  query: Typ.t,
  path: Id.Set.t,
};

type witness =
  | Syn(Typ.t)
  | Ana(Typ.t);

// The context above the focus either synthesises ψ or analyses against υ.
type analysis = {
  omitted: Id.Set.t,
  retained: Id.Set.t,
  gamma: Ctx.t,
  witness,
};

// A sliceable type: a type plus its query routing, forwards and backwards.
// υ is the query asked of it, γ the assumptions a body accumulated, ψ the
// sliced type answered with, and ⊑ is "at most as precise as" (? ⊑ τ for all τ):
//   dispatch(υ) = {psi: ψ, _}   υ ⊑ ψ ⊑ shape,   υ₁ ⊑ υ₂ ⟹ ψ₁ ⊑ ψ₂
//   demand(γ)   = {psi: ψ, _}   ψ ⊑ shape,       γ₁ ⊑ γ₂ ⟹ ψ₁ ⊑ ψ₂
type t = {
  shape: Typ.t,
  ids: Id.Set.t,
  binder: bool,
  supplied: Typ.t,
  declared: bool,
  dispatch: (env, Typ.t) => slice,
  analyse: env => analysis,
  demand: (env, Ctx.t) => slice,
};

// How a sub-term's type enters the type this rule constructs.
type role =
  | Part // a type component of the type this rule constructs
  | Through // the whole constructed type, no constructor applied
  | Prune // Through, but omit a structurally empty query
  | Omit // only type checked, kept out of the slice
  | Retain // no type contribution, but retained and focusable
  | Source // a definition, sliced by the demand this rule's binders produce
  | Alternative // one branch; the branches split the query co-Heytingly
  | Binder; // binds names, without contributing to the constructed type

// How the checker's info map carries a rule's recorded sub-terms until `add`
// takes them.
type scratch('info) = {
  read: 'info => option(list((role, t))),
  write: list((role, t)) => 'info,
};

exception Focus_not_found(Id.t);
exception Wrong_focus_sort;
exception Incompatible_query(Typ.t);

let gap = Typ.gap;
let is_gap = Typ.is_gap;

let subtree_ids = (x: Any.t): Id.Set.t => {
  let ids = ref(Id.Set.empty);
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) => {
      ids :=
        List.fold_left(
          (ids, id) => Id.Set.add(id, ids),
          ids^,
          IdTagged.ids(x),
        );
      continue(x);
    };
  let _ = Any.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f, x);
  ids^;
};

let exp_ids = (e: Exp.t) => subtree_ids(Exp(e));
let pat_ids = (p: Pat.t) => subtree_ids(Pat(p));
let typ_ids = (t: Typ.t) => subtree_ids(Typ(t));

let empty_gamma: Ctx.t = Ctx.empty;

let empty_slice: slice = {
  omitted: Id.Set.empty,
  gamma: empty_gamma,
  psi: gap,
};

let empty_analysis = {
  omitted: Id.Set.empty,
  retained: Id.Set.empty,
  gamma: empty_gamma,
  witness: Syn(gap),
};

let analysis_of_slice = (slice: slice): analysis => {
  omitted: slice.omitted,
  retained: Id.Set.empty,
  gamma: slice.gamma,
  witness: Syn(slice.psi),
};

let entry_key = (entry: Ctx.entry): option((CoCtx.sort, string)) =>
  switch (entry) {
  | VarEntry({name, _}) => Some((CoCtx.Value, name))
  | ConstructorEntry({name, _}) => Some((CoCtx.Constructor, name))
  | TVarEntry({name, _}) => Some((CoCtx.Alias, name))
  | LivelitEntry(_) => None
  };

let entry_typ = (entry: Ctx.entry): Typ.t =>
  switch (entry) {
  | VarEntry({typ, _})
  | ConstructorEntry({typ, _}) => typ
  | TVarEntry({kind: Singleton(typ), _}) => typ
  | TVarEntry(_)
  | LivelitEntry(_) => gap
  };

let entry_with_typ = (entry: Ctx.entry, typ: Typ.t): Ctx.entry =>
  switch (entry) {
  | VarEntry(v) =>
    VarEntry({
      ...v,
      typ,
    })
  | ConstructorEntry(v) =>
    ConstructorEntry({
      ...v,
      typ,
    })
  | TVarEntry(v) =>
    TVarEntry({
      ...v,
      kind: Singleton(typ),
    })
  | LivelitEntry(_) => entry
  };

let entry_of = (~sort, ~name, ~id, typ): Ctx.entry =>
  switch (sort) {
  | CoCtx.Value =>
    Ctx.VarEntry({
      name,
      id,
      typ,
      custom_statics: None,
    })
  | CoCtx.Constructor =>
    Ctx.ConstructorEntry({
      name,
      id,
      typ,
      custom_statics: None,
    })
  | CoCtx.Alias =>
    Ctx.TVarEntry({
      name,
      id,
      kind: Singleton(typ),
      typ_kind: TypKind.Type,
    })
  };

let singleton = (~sort, ~name, ~id, typ): Ctx.t => {
  ...Ctx.empty,
  entries: [entry_of(~sort, ~name, ~id, typ)],
};

// Assumptions combine per name by meet: what both sides ask of it.
let join = (ctx: Ctx.t, left: Ctx.t, right: Ctx.t): Ctx.t => {
  let add = (entries, entry) =>
    switch (entry_key(entry)) {
    | None => entries @ [entry]
    | key when List.exists(e => entry_key(e) == key, entries) =>
      List.map(
        e =>
          entry_key(e) == key
            ? entry_with_typ(
                e,
                Typ.meet_gap(ctx, entry_typ(e), entry_typ(entry)),
              )
            : e,
        entries,
      )
    | _ => entries @ [entry]
    };
  {
    ...left,
    entries: List.fold_left(add, left.entries, right.entries),
  };
};

let join_all = (ctx: Ctx.t, gammas: list(Ctx.t)): Ctx.t =>
  List.fold_left(join(ctx), empty_gamma, gammas);

let lookup = (~sort, ~name, gamma: Ctx.t): Typ.t =>
  gamma.entries
  |> List.find_opt(e => entry_key(e) == Some((sort, name)))
  |> Option.map(entry_typ)
  |> Option.value(~default=gap);

let discharge = (~sort, ~name, gamma: Ctx.t): Ctx.t => {
  ...gamma,
  entries:
    List.filter(e => entry_key(e) != Some((sort, name)), gamma.entries),
};

let merge = (ctx: Ctx.t, slices: list(slice)): slice => {
  omitted:
    List.fold_left(
      (ids, s: slice) => Id.Set.union(ids, s.omitted),
      Id.Set.empty,
      slices,
    ),
  gamma: join_all(ctx, List.map((s: slice) => s.gamma, slices)),
  psi: gap,
};

let combine_analysis = (ctx: Ctx.t, left: analysis, right: analysis) => {
  let retained = Id.Set.union(left.retained, right.retained);
  {
    omitted: Id.Set.diff(Id.Set.union(left.omitted, right.omitted), retained),
    retained,
    gamma: join(ctx, left.gamma, right.gamma),
    witness: right.witness,
  };
};

// What a query still asks for once `supplied` has answered part of it.
let residual = (ctx: Ctx.t, query: Typ.t, supplied: Typ.t): Typ.t => {
  let left = Typ.subtract(ctx, query, supplied);
  Typ.is_empty(left) ? gap : left;
};

let fills = (sub_terms: list((role, t)), role, node: t): bool =>
  List.exists(((role, _)) => role == Part, sub_terms)
  && (role == Part || role == Binder && node.binder);

// Break a type into the parts a rule's sub-terms fill.
let components = (~former: option(MatchedTyp.former), ctx, ty) =>
  switch (former) {
  | Some(former) => MatchedTyp.tolerant(former.match_, ctx, ty)
  | None => Typ.children(ty)
  };

// Which component slot each sub-term fills, and the query each is asked at.
// Read forwards by `place` and backwards by `lift`, so they cannot disagree.
type routing = {
  slots: list(option(int)),
  queries: list(Typ.t),
  broadcast: bool,
};

// Match the query's type components to the constructed type's, or share the
// one type component between them all (a list literal and its elements).
let route =
    (~former: option(MatchedTyp.former), ctx, shape, sub_terms, query)
    : routing => {
  let fills = fills(sub_terms);
  let (count, slots) =
    List.fold_left_map(
      (taken, (role, node)) =>
        fills(role, node) ? (taken + 1, Some(taken)) : (taken, None),
      0,
      sub_terms,
    );
  let components = components(~former, ctx);
  let supplied = components(Typ.weak_head_normalize(ctx, query));
  let arity =
    switch (components(shape)) {
    | [] => List.length(supplied)
    | components => List.length(components)
    };
  let broadcast = arity == 1 && count > 1;
  let queries =
    if (is_gap(query) || supplied == []) {
      List.init(count, _ => gap);
    } else if (count == arity && List.length(supplied) == arity) {
      supplied;
    } else if (count == 1 && arity == 1) {
      [query];
    } else if (broadcast && List.length(supplied) == 1) {
      List.init(count, _ => List.hd(supplied));
    } else {
      List.init(count, _ => gap);
    };
  {
    slots,
    queries,
    broadcast,
  };
};

// The inverse of `place`: put a query back in the slot the routing gave it.
let lift =
    (
      ~former: option(MatchedTyp.former),
      ~routing: routing,
      ctx: Ctx.t,
      shape,
      selected: int,
      query,
    )
    : Typ.t =>
  if (is_gap(query)) {
    gap;
  } else {
    switch (List.nth(routing.slots, selected)) {
    | None => query
    | Some(slot) =>
      let shape = Typ.weak_head_normalize(ctx, shape);
      let components = components(~former, ctx, shape);
      let slot = routing.broadcast ? 0 : slot;
      let parts =
        List.mapi((index, _) => index == slot ? query : gap, components);
      switch (former) {
      | Some(former) when parts != [] => former.build(parts)
      | Some(_) => query
      | None => Typ.rebuild(shape, parts) |> Option.value(~default=query)
      };
    };
  };

let dispatch_analysis =
    (~project=x => x, ~reveal=false, node: t, env, query): analysis => {
  let slice =
    node.dispatch(
      {
        ...env,
        focus: None,
        path:
          switch (reveal ? env.focus : None) {
          | Some(focus) => Id.Set.remove(focus, env.path)
          | None => Id.Set.empty
          },
      },
      query,
    );
  {
    omitted: slice.omitted,
    retained: Id.Set.diff(node.ids, slice.omitted),
    gamma: slice.gamma,
    witness: Syn(project(slice.psi)),
  };
};

let source_analysis = (~embed, ~project, node: t, env): analysis =>
  switch (node.analyse(env)) {
  | {witness: Ana(query), _} =>
    dispatch_analysis(~project, node, env, embed(query))
  | {witness: Syn(query), _} as inner => {
      ...inner,
      witness: Syn(project(query)),
    }
  };

// A recorded sub-term mid-assembly: its routed query, and its slice once
// dispatched.
type placed = {
  role,
  node: t,
  query: Typ.t,
  result: option(slice),
};

// Give every recorded sub-term the query it is asked at.
let place = (~routing: routing, sub_terms, query) =>
  List.map2(
    (slot, (role, node)) => {
      let query =
        switch (slot) {
        | Some(taken) => List.nth(routing.queries, taken)
        | None => role == Through || role == Prune ? query : gap
        };
      let query =
        role == Prune && Typ.children(query) != [] && Typ.is_empty(query)
          ? gap : query;
      {
        role: role == Prune ? Through : role,
        node,
        query,
        result: None,
      };
    },
    routing.slots,
    sub_terms,
  );

// Dispatch the sub-terms whose query is known; branches take residuals in turn.
let forward = (ctx: Ctx.t, env: env, query: Typ.t, placed: list(placed)) => {
  let (placed, _) =
    List.fold_left(
      ((acc, left), item) =>
        switch (item.role) {
        | Part
        | Through
        | Prune => (
            [
              {
                ...item,
                result: Some(item.node.dispatch(env, item.query)),
              },
              ...acc,
            ],
            left,
          )
        | Alternative =>
          let slice = item.node.dispatch(env, left);
          (
            [
              {
                ...item,
                query: left,
                result: Some(slice),
              },
              ...acc,
            ],
            residual(ctx, left, slice.psi),
          );
        | Omit
        | Retain when !Id.Set.is_empty(Id.Set.inter(item.node.ids, env.path)) => (
            [
              {
                ...item,
                result: Some(item.node.dispatch(env, gap)),
              },
              ...acc,
            ],
            left,
          )
        | Omit
        | Retain
        | Source
        | Binder => ([item, ...acc], left)
        },
      ([], query),
      placed,
    );
  List.rev(placed);
};

// Resolve each binder against the assumptions of what it scopes.
let backward = (ctx: Ctx.t, env: env, placed: list(placed)) => {
  let (placed, _) =
    List.fold_left(
      ((acc, gamma), item) =>
        if (item.role == Binder) {
          let need = item.node.demand(env, gamma);
          let query = Typ.meet_gap(ctx, item.query, need.psi);
          let slice = item.node.dispatch(env, query);
          (
            [
              {
                ...item,
                query,
                result:
                  Some({
                    ...merge(ctx, [need, slice]),
                    psi: need.psi,
                  }),
              },
              ...acc,
            ],
            need.gamma,
          );
        } else {
          let gamma =
            switch (item.result) {
            | Some(slice) => join(ctx, slice.gamma, gamma)
            | None => gamma
            };
          ([item, ...acc], gamma);
        },
      ([], empty_gamma),
      List.rev(placed),
    );
  placed;
};

// Dispatch each definition once, at the join of the demands the binders produced.
let sources = (ctx: Ctx.t, env: env, placed: list(placed)) => {
  let demanded =
    placed
    |> List.filter(item => item.role == Binder)
    |> List.filter_map(item =>
         Option.map(
           (slice: slice) => residual(ctx, slice.psi, item.node.supplied),
           item.result,
         )
       )
    |> Typ.meet_gap_all(ctx);
  List.map(
    item =>
      switch (item.role) {
      | Source => {
          ...item,
          query: demanded,
          result: Some(item.node.dispatch(env, demanded)),
        }
      | _ => item
      },
    placed,
  );
};

// Rebuild the sliced type from the type components the sub-terms supplied.
let assembled_psi =
    (
      ctx: Ctx.t,
      shape: Typ.t,
      query: Typ.t,
      fills,
      broadcast,
      placed: list(placed),
    ) => {
  let psis = keep =>
    placed
    |> List.filter(item => keep(item))
    |> List.filter_map(item => item.result)
    |> List.map(slice => slice.psi);
  let filled = psis(item => fills(item.role, item.node));
  switch (
    filled,
    psis(item => item.role == Alternative),
    psis(item => item.role == Through),
  ) {
  | ([], [], []) => query
  | ([], [], throughs) => Typ.meet_gap_all(ctx, throughs)
  | ([], alternatives, _) => Typ.meet_gap_all(ctx, alternatives)
  | (filled, _, _) =>
    let filled = broadcast ? [Typ.meet_gap_all(ctx, filled)] : filled;
    Typ.rebuild(shape, filled) |> Option.value(~default=query);
  };
};

// The interpreter: both directions, derived from the sub-terms recorded.
let assemble = (~ctx: Ctx.t, ~former, ~shape: Typ.t, ~sub_terms) => {
  let dispatch = (env, query) => {
    let routing = route(~former, ctx, shape, sub_terms, query);
    let placed =
      place(~routing, sub_terms, query)
      |> forward(ctx, env, query)
      |> backward(ctx, env)
      |> sources(ctx, env);
    let slices = List.filter_map(item => item.result, placed);
    {
      ...merge(ctx, slices),
      psi:
        assembled_psi(
          ctx,
          shape,
          query,
          fills(sub_terms),
          routing.broadcast,
          placed,
        ),
    };
  };
  let demand = (env, gamma) => {
    let (needs, gamma) =
      List.fold_left(
        ((needs, gamma), (role, node)) =>
          switch (role) {
          | Part
          | Through
          | Prune
          | Binder =>
            let need = node.demand(env, gamma);
            ([(role, need), ...needs], need.gamma);
          | Omit
          | Retain
          | Source
          | Alternative => (needs, gamma)
          },
        ([], gamma),
        List.rev(sub_terms),
      );
    let parts =
      needs
      |> List.filter_map(((role, need)) =>
           role == Part ? Some(need.psi) : None
         );
    let psi =
      switch (parts) {
      | [] =>
        needs |> List.map(((_, need)) => need.psi) |> Typ.meet_gap_all(ctx)
      | parts =>
        switch (former) {
        | Some(former) =>
          List.for_all(Typ.is_empty, parts) ? gap : former.build(parts)
        | None =>
          List.length(parts) == List.length(Typ.children(shape))
            ? Typ.rebuild(shape, parts) |> Option.value(~default=gap)
            : Typ.meet_gap_all(ctx, parts)
        }
      };
    let psi = Typ.is_empty(psi) ? gap : psi;
    {
      ...merge(ctx, List.map(snd, needs)),
      gamma,
      psi,
    };
  };
  let analyse = env => {
    let selected =
      sub_terms
      |> List.mapi((index, (_, node)) =>
           Id.Set.is_empty(Id.Set.inter(node.ids, env.path))
             ? None : Some(index)
         )
      |> List.find_map(x => x);
    switch (selected) {
    | None => empty_analysis
    | Some(index) =>
      let (role, node) = List.nth(sub_terms, index);
      let inner = node.analyse(env);
      let siblings =
        sub_terms
        |> List.mapi((other, (_, node)) =>
             other == index ? Id.Set.empty : node.ids
           )
        |> List.fold_left(Id.Set.union, Id.Set.empty);
      let finish = result => {
        ...result,
        omitted:
          Id.Set.diff(
            Id.Set.union(result.omitted, siblings),
            result.retained,
          ),
      };
      let lifted = query => {
        let routing = route(~former, ctx, shape, sub_terms, query);
        lift(~former, ~routing, ctx, shape, index, query);
      };
      switch (inner.witness, role) {
      | (Ana(query), Source) =>
        let supplied =
          sub_terms
          |> List.filter_map(((role, node: t)) =>
               role == Binder ? Some(node.supplied) : None
             )
          |> Typ.meet_gap_all(ctx);
        let asked =
          is_gap(supplied) ? query : residual(ctx, query, supplied);
        sub_terms
        |> List.fold_left(
             (result, (role, node)) =>
               if (role == Binder) {
                 let unused = node.demand(env, empty_gamma).omitted;
                 let bound = dispatch_analysis(node, env, query);
                 combine_analysis(
                   ctx,
                   result,
                   {
                     ...bound,
                     omitted: Id.Set.union(bound.omitted, unused),
                     retained: Id.Set.diff(bound.retained, unused),
                   },
                 );
               } else {
                 result;
               },
             dispatch_analysis(
               ~project=_ => gap,
               ~reveal=true,
               node,
               env,
               asked,
             ),
           )
        |> finish;
      | (Ana(_), Omit) =>
        {
          ...inner,
          witness: Syn(shape),
        }
        |> finish
      | (Ana(query), _) =>
        {
          ...inner,
          witness: Ana(lifted(query)),
        }
        |> finish
      | (Syn(query), _) =>
        {
          ...inner,
          witness: Syn(lifted(query)),
        }
        |> finish
      };
    };
  };
  (dispatch, analyse, demand);
};

let unit_demand = (_env, gamma) => {
  ...empty_slice,
  gamma,
};

// Resolve one bound name: what the assumptions ask of it, and its id if
// nothing does.
let binder_demand = (~sort, ~name, ~id, _env, gamma) => {
  let demanded = lookup(~sort, ~name, gamma);
  {
    omitted: is_gap(demanded) ? Id.Set.singleton(id) : Id.Set.empty,
    gamma: discharge(~sort, ~name, gamma),
    psi: demanded,
  };
};

// Wrap a node: the focus overrides its query, and a node nothing asks for
// omits its whole subtree.
let mk =
    (
      ~ctx: Ctx.t,
      ~id: Id.t,
      ~ids: Id.Set.t,
      ~shape: Typ.t,
      ~sub_terms: list((role, t))=[],
      ~former: option(MatchedTyp.former)=None,
      ~co_ctx: CoCtx.t=CoCtx.empty,
      ~binds: option((CoCtx.sort, string, Id.t))=None,
      ~binder: bool=false,
      ~declared: bool=false,
      (),
    )
    : t => {
  let (assembled, analyse_assembled, demand) =
    assemble(~ctx, ~former, ~shape, ~sub_terms);
  let demand =
    switch (binds) {
    | Some((sort, name, id)) => binder_demand(~sort, ~name, ~id)
    | None => demand
    };
  let checked =
    sub_terms
    |> List.filter_map(((role, node)) =>
         role == Omit ? Some(node.ids) : None
       )
    |> List.fold_left(Id.Set.union, Id.Set.empty);
  let dispatch = (env, query) => {
    let query = env.focus == Some(id) ? env.query : query;
    let capped = Typ.overlap(ctx, query, shape);
    // A node supplying nothing of what was asked is as good as one asked
    // nothing. An unknown type supplies nothing in particular, but is asked.
    let unasked =
      is_gap(query)
      || !Typ.is_empty(shape)
      && Typ.is_consistent(ctx, query, shape)
      && Typ.is_empty(capped)
      && !Typ.is_empty(query);
    if (unasked && !Id.Set.mem(id, env.path)) {
      {
        omitted: ids,
        gamma: empty_gamma,
        psi: gap,
      };
    } else {
      let slice = assembled(env, query);
      let used =
        co_ctx
        |> CoCtx.entries_at(id)
        |> List.map(((name, entry: CoCtx.entry)) =>
             singleton(
               ~sort=entry.sort,
               ~name,
               ~id=entry.id,
               entry.demanded |> Option.value(~default=query),
             )
           )
        |> join_all(ctx);
      {
        omitted: Id.Set.union(slice.omitted, Id.Set.diff(checked, env.path)),
        gamma: join(ctx, slice.gamma, used),
        psi: capped,
      };
    };
  };
  let analyse = env =>
    if (env.focus == Some(id)) {
      {
        omitted: ids,
        retained: Id.Set.empty,
        gamma: empty_gamma,
        witness: Ana(env.query),
      };
    } else {
      analyse_assembled(env);
    };
  let declared =
    declared
    || former != None
    || List.exists(
         ((role, node)) => role == Through && node.declared,
         sub_terms,
       );
  let of_role = role =>
    List.filter_map(
      ((r, node: t)) => r == role ? Some(node.supplied) : None,
      sub_terms,
    );
  let supplied =
    if (declared) {
      former == None ? shape : Typ.without_type_args(shape);
    } else {
      switch (of_role(Part)) {
      | [] => Typ.meet_gap_all(ctx, of_role(Through))
      | parts =>
        switch (former) {
        | Some(former) =>
          List.for_all(Typ.is_empty, parts) ? gap : former.build(parts)
        | None =>
          List.length(parts) == List.length(Typ.children(shape))
            ? Typ.rebuild(shape, parts) |> Option.value(~default=gap) : gap
        }
      };
    };
  {
    shape,
    ids,
    binder,
    supplied,
    declared,
    dispatch,
    analyse,
    demand,
  };
};

let recorded = (~scratch, ~id: Id.t, m) =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(info) => scratch.read(info) |> Option.value(~default=[])
  | None => []
  };

// Append a sub-term to what the enclosing rule has recorded so far, or, for a
// binder checked after what it scopes, put it in front.
let record = (~scratch, ~id: Id.t, ~first=false, role: role, sub_term: t, m) => {
  let so_far = recorded(~scratch, ~id, m);
  Id.Map.add(
    id,
    scratch.write(
      first ? [(role, sub_term), ...so_far] : so_far @ [(role, sub_term)],
    ),
    m,
  );
};

let take = (~scratch, ~id: Id.t, m) =>
  switch (Option.bind(Id.Map.find_opt(id, m), scratch.read)) {
  | Some(components) => (components, Id.Map.remove(id, m))
  | None => ([], m)
  };

let edge =
    (~scratch, ~at: Id.t, ~first=false, role, slice_of, (info, elab, m), k) =>
  k((
    info,
    elab,
    record(~scratch, ~id=at, ~first, role, slice_of(info), m),
  ));

let edge_typ = (~scratch, ~at: Id.t, role, slice_of, (info, m), k) =>
  k((info, record(~scratch, ~id=at, role, slice_of(info), m)));

// A binding site that is a bare name rather than a pattern, such as a type alias.
let binding = (~sort, ~name, ~id, ~ids, ~demand_of: Ctx.t => Typ.t): t => {
  shape: gap,
  ids,
  binder: true,
  supplied: gap,
  declared: false,
  dispatch: (_, query) =>
    is_gap(query)
      ? {
        ...empty_slice,
        omitted: ids,
      }
      : {
        ...empty_slice,
        psi: query,
      },
  analyse: _ => empty_analysis,
  demand: (_, gamma) => {
    let demanded = demand_of(gamma);
    {
      omitted: is_gap(demanded) ? Id.Set.singleton(id) : Id.Set.empty,
      gamma: discharge(~sort, ~name, gamma),
      psi: demanded,
    };
  },
};

let opaque: t = {
  shape: gap,
  ids: Id.Set.empty,
  binder: false,
  supplied: gap,
  declared: false,
  dispatch: (_, query) => {
    ...empty_slice,
    psi: query,
  },
  analyse: _ => empty_analysis,
  demand: unit_demand,
};

// `demand` rewrites a query into the one to ask the wrapped node; `answer`
// projects that node's reply back out.
let reshaped = (~demand: Typ.t => Typ.t, ~answer: Typ.t => Typ.t, node: t): t => {
  ...node,
  shape: answer(node.shape),
  demand: unit_demand,
  dispatch: (env, query) => {
    let slice = node.dispatch(env, demand(query));
    {
      ...slice,
      psi: answer(slice.psi),
    };
  },
  analyse: source_analysis(~embed=demand, ~project=answer, node),
};

// For a rule whose result is a type component of a sub-term's type: embed
// the query in that type, project the answer back out.
let routed =
    (~ctx: Ctx.t, ~former: MatchedTyp.former, ~input, ~output, node: t): t => {
  let components = components(~former=Some(former), ctx);
  let answer = ty =>
    List.nth_opt(components(ty), output) |> Option.value(~default=gap);
  let demand = query => {
    let shape = Typ.weak_head_normalize(ctx, node.shape);
    Typ.embed(~build=former.build, shape, components(shape), input, query);
  };
  {
    ...reshaped(~demand, ~answer, node),
    binder: false,
    supplied: gap,
    declared: false,
  };
};

let component = (~ctx, ~former, ~index, node) =>
  routed(~ctx, ~former, ~input=index, ~output=index, node);

// A checked premise asks its explicit source for the outer υ it needs.
let checked_by = (~ctx: Ctx.t, ~source: t, checked: t): t => {
  ...checked,
  analyse: env =>
    switch (checked.analyse(env)) {
    | {witness: Syn(query) | Ana(query), _} as inner =>
      combine_analysis(ctx, inner, dispatch_analysis(source, env, query))
    },
};

// What a slice query returns to the UI and the tests.
type result = {
  omitted: Id.Set.t,
  gamma: Ctx.t,
  psi: Typ.t,
  ana: Typ.t,
};

type direction = [
  | `Syn
  | `Ana
];

// What the entry point needs to know about the focused node.
type focused = {
  is_exp: bool,
  ancestors: list(Id.t),
  ctx: Ctx.t,
  syn: Typ.t,
};

// Entry point: check the focus, then dispatch the root at the query.
let slice =
    (
      ~focus: option(Id.t),
      ~direction: direction,
      ~root_id: Id.t,
      ~root: t,
      ~focused: Id.t => option(focused),
      query,
    )
    : result => {
  let path =
    switch (focus) {
    | None => Id.Set.empty
    | Some(id) =>
      switch (focused(id)) {
      | None => raise(Focus_not_found(id))
      | Some({is_exp: false, _}) when direction == `Syn =>
        raise(Wrong_focus_sort)
      | Some({ancestors, ctx, syn, _}) =>
        if (direction == `Syn
            && !is_gap(query)
            && !Typ.is_askable(ctx, syn, query)) {
          raise(Incompatible_query(query));
        };
        let path = Id.Set.of_list(ancestors);
        direction == `Ana ? Id.Set.add(id, path) : path;
      }
    };
  let env = {
    focus,
    query,
    path,
  };
  let at_root = focus == None || focus == Some(root_id);
  let analysis =
    switch (direction) {
    | `Syn => analysis_of_slice(root.dispatch(env, at_root ? query : gap))
    | `Ana when at_root => analysis_of_slice(root.dispatch(env, query))
    | `Ana => root.analyse(env)
    };
  switch (analysis.witness) {
  | Syn(psi) => {
      omitted: analysis.omitted,
      gamma: analysis.gamma,
      psi,
      ana: gap,
    }
  | Ana(ana) => {
      omitted: analysis.omitted,
      gamma: analysis.gamma,
      psi: gap,
      ana,
    }
  };
};

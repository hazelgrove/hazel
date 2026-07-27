/* Slice.re - sliceable types.

   A sliceable type is a type paired with the routing of a query on it back to
   the terms that produced it. The routing is derived from the roles the
   checker's binding operators in Statics.re, automagically inferring slicing. */

// The namespace a demanded name lives in.
type sort =
  | Value
  | Constructor
  | Alias;

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

// A sliceable type: a type plus its query routing, forwards and backwards.
// υ is the query asked of it, γ the assumptions a body accumulated, ψ the
// sliced type answered with, and ⊑ is "at most as precise as" (? ⊑ τ for all τ):
//   dispatch(υ) = {psi: ψ, _}   υ ⊑ ψ ⊑ shape,   υ₁ ⊑ υ₂ ⟹ ψ₁ ⊑ ψ₂
//   demand(γ)   = {psi: ψ, _}   ψ ⊑ shape,       γ₁ ⊑ γ₂ ⟹ ψ₁ ⊑ ψ₂
type t = {
  shape: Typ.t,
  ids: Id.Set.t,
  binder: bool,
  dispatch: (env, Typ.t) => slice,
  demand: (env, Ctx.t) => slice,
};

// How a sub-term's type enters the type this rule constructs.
type role =
  | Part // an argument of the type constructor this rule applies
  | Through // the whole constructed type, no constructor applied
  | Omit // checked, but absent from the constructed type
  | Source // a definition, sliced by the demand this rule's binders produce
  | Alternative // one branch; the branches split the query co-Heytingly
  | Binder; // binds names, but absent from the constructed type

// A name a term uses, and what the term's query demands of it.
type use = {
  sort,
  name: string,
  id: Id.t,
  demanded: Typ.t => Typ.t,
};

// How the checker's info map carries recorded components until `add` takes them.
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

let entry_key = (entry: Ctx.entry): option((sort, string)) =>
  switch (entry) {
  | VarEntry({name, _}) => Some((Value, name))
  | ConstructorEntry({name, _}) => Some((Constructor, name))
  | TVarEntry({name, _}) => Some((Alias, name))
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
  | Value =>
    Ctx.VarEntry({
      name,
      id,
      typ,
      custom_statics: None,
    })
  | Constructor =>
    Ctx.ConstructorEntry({
      name,
      id,
      typ,
      custom_statics: None,
    })
  | Alias =>
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
      (ids, s) => Id.Set.union(ids, s.omitted),
      Id.Set.empty,
      slices,
    ),
  gamma: join_all(ctx, List.map(s => s.gamma, slices)),
  psi: gap,
};

let residual = (ctx: Ctx.t, query: Typ.t, supplied: Typ.t): Typ.t => {
  let left = Typ.subtract(ctx, query, supplied);
  Typ.is_empty(left) ? gap : left;
};

let fills = (components: list((role, t)), role, node: t): bool =>
  List.exists(((role, _)) => role == Part, components)
  && (role == Part || role == Binder && node.binder);

let route =
    (ctx: Ctx.t, shape: Typ.t, query: Typ.t, count: int)
    : (list(Typ.t), bool) => {
  let arity = List.length(Typ.children(shape));
  let supplied = Typ.children(Typ.weak_head_normalize(ctx, query));
  let broadcast = arity == 1 && count > 1;
  if (is_gap(query) || supplied == []) {
    (List.init(count, _ => gap), broadcast);
  } else if (count == arity && List.length(supplied) == arity) {
    (supplied, false);
  } else if (broadcast && List.length(supplied) == 1) {
    (List.init(count, _ => List.hd(supplied)), true);
  } else {
    (List.init(count, _ => gap), broadcast);
  };
};

// A recorded component mid-assembly: its routed query, and its slice once dispatched.
type placed = {
  role,
  node: t,
  query: Typ.t,
  result: option(slice),
};

let place = (ctx: Ctx.t, shape: Typ.t, components: list((role, t)), query) => {
  let fills = fills(components);
  let count =
    List.length(
      List.filter(((role, node)) => fills(role, node), components),
    );
  let (queries, broadcast) = route(ctx, shape, query, count);
  let (placed, _) =
    List.fold_left(
      ((placed, taken), (role, node)) => {
        let (query, taken) =
          if (fills(role, node)) {
            (List.nth(queries, taken), taken + 1);
          } else {
            (role == Through ? query : gap, taken);
          };
        (
          placed
          @ [
            {
              role,
              node,
              query,
              result: None,
            },
          ],
          taken,
        );
      },
      ([], 0),
      components,
    );
  (placed, broadcast);
};

let forward = (ctx: Ctx.t, env: env, query: Typ.t, placed: list(placed)) => {
  let (placed, _) =
    List.fold_left(
      ((acc, left), item) =>
        switch (item.role) {
        | Part
        | Through => (
            acc
            @ [
              {
                ...item,
                result: Some(item.node.dispatch(env, item.query)),
              },
            ],
            left,
          )
        | Alternative =>
          let slice = item.node.dispatch(env, left);
          (
            acc
            @ [
              {
                ...item,
                query: left,
                result: Some(slice),
              },
            ],
            residual(ctx, left, slice.psi),
          );
        | Omit
        | Source
        | Binder => (acc @ [item], left)
        },
      ([], query),
      placed,
    );
  placed;
};

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

let sources = (ctx: Ctx.t, env: env, placed: list(placed)) => {
  let demanded =
    placed
    |> List.filter(item => item.role == Binder)
    |> List.filter_map(item => item.result)
    |> List.map(slice => slice.psi)
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

let assemble = (~ctx: Ctx.t, ~shape: Typ.t, ~components: list((role, t))) => {
  let dispatch = (env, query) => {
    let (placed, broadcast) = place(ctx, shape, components, query);
    let placed =
      placed
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
          fills(components),
          broadcast,
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
          | Binder =>
            let need = node.demand(env, gamma);
            ([(role, need), ...needs], need.gamma);
          | Omit
          | Source
          | Alternative => (needs, gamma)
          },
        ([], gamma),
        List.rev(components),
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
        List.length(parts) == List.length(Typ.children(shape))
          ? Typ.rebuild(shape, parts) |> Option.value(~default=gap)
          : Typ.meet_gap_all(ctx, parts)
      };
    let psi = Typ.is_empty(psi) ? gap : psi;
    {
      ...merge(ctx, List.map(snd, needs)),
      gamma,
      psi,
    };
  };
  (dispatch, demand);
};

let unit_demand = (_env, gamma) => {
  ...empty_slice,
  gamma,
};

let binder_demand = (~sort, ~name, ~id, _env, gamma) => {
  let demanded = lookup(~sort, ~name, gamma);
  {
    omitted: is_gap(demanded) ? Id.Set.singleton(id) : Id.Set.empty,
    gamma: discharge(~sort, ~name, gamma),
    psi: demanded,
  };
};

let use = (~sort, ~name, ~id, ~demanded=Fun.id, ()): use => {
  sort,
  name,
  id,
  demanded,
};

let mk =
    (
      ~ctx: Ctx.t,
      ~id: Id.t,
      ~ids: Id.Set.t,
      ~shape: Typ.t,
      ~components: list((role, t))=[],
      ~uses: list(use)=[],
      ~binds: list((sort, string, Id.t))=[],
      ~binder: bool=false,
      ~override: option(t)=None,
      (),
    )
    : t => {
  let (assembled, demand) =
    switch (override) {
    | Some(node) => (node.dispatch, node.demand)
    | None => assemble(~ctx, ~shape, ~components)
    };
  let demand =
    switch (binds) {
    | [(sort, name, id)] => binder_demand(~sort, ~name, ~id)
    | [] => demand
    | binds => (
        (env, gamma) =>
          List.fold_left(
            (need, (sort, name, id)) => {
              let one = binder_demand(~sort, ~name, ~id, env, need.gamma);
              {
                ...merge(ctx, [need, one]),
                psi: Typ.meet_gap(ctx, need.psi, one.psi),
              };
            },
            {
              ...empty_slice,
              gamma,
            },
            binds,
          )
      )
    };
  let checked =
    components
    |> List.filter_map(((role, node)) =>
         role == Omit ? Some(node.ids) : None
       )
    |> List.fold_left(Id.Set.union, Id.Set.empty);
  let dispatch = (env, query) => {
    let query = env.focus == Some(id) ? env.query : query;
    if (is_gap(query) && !Id.Set.mem(id, env.path)) {
      {
        omitted: ids,
        gamma: empty_gamma,
        psi: gap,
      };
    } else {
      let slice = assembled(env, query);
      let used =
        uses
        |> List.map(u =>
             singleton(
               ~sort=u.sort,
               ~name=u.name,
               ~id=u.id,
               u.demanded(query),
             )
           )
        |> join_all(ctx);
      {
        ...slice,
        omitted: Id.Set.union(slice.omitted, checked),
        gamma: join(ctx, slice.gamma, used),
      };
    };
  };
  {
    shape,
    ids,
    binder,
    dispatch,
    demand,
  };
};

let recorded = (~scratch, ~id: Id.t, m) =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(info) => scratch.read(info) |> Option.value(~default=[])
  | None => []
  };

let record = (~scratch, ~id: Id.t, role: role, component: t, m) =>
  Id.Map.add(
    id,
    scratch.write(recorded(~scratch, ~id, m) @ [(role, component)]),
    m,
  );

let take = (~scratch, ~id: Id.t, m) =>
  switch (Option.bind(Id.Map.find_opt(id, m), scratch.read)) {
  | Some(components) => (components, Id.Map.remove(id, m))
  | None => ([], m)
  };

let edge = (~scratch, ~at: Id.t, role, slice_of, (info, elab, m), k) =>
  k((info, elab, record(~scratch, ~id=at, role, slice_of(info), m)));

let edge_typ = (~scratch, ~at: Id.t, role, slice_of, (info, m), k) =>
  k((info, record(~scratch, ~id=at, role, slice_of(info), m)));

let binding = (~sort, ~name, ~id, ~ids): t => {
  shape: gap,
  ids,
  binder: true,
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
  demand: binder_demand(~sort, ~name, ~id),
};

let opaque: t = {
  shape: gap,
  ids: Id.Set.empty,
  binder: false,
  dispatch: (_, query) => {
    ...empty_slice,
    psi: query,
  },
  demand: unit_demand,
};

let component =
    (~ctx: Ctx.t, ~matcher: MatchedTyp.matcher, ~index, node: t): t => {
  let components = ty => MatchedTyp.tolerant(matcher, ctx, ty);
  let project = ty =>
    List.nth_opt(components(ty), index) |> Option.value(~default=gap);
  {
    shape: project(node.shape),
    ids: node.ids,
    binder: false,
    dispatch: (env, query) => {
      let embedded =
        Typ.embed(Typ.weak_head_normalize(ctx, node.shape), index, query);
      let slice = node.dispatch(env, embedded);
      {
        ...slice,
        psi: project(slice.psi),
      };
    },
    demand: unit_demand,
  };
};

// What a slice query returns to the UI and the tests.
type result = {
  omitted: Id.Set.t,
  gamma: Ctx.t,
  psi: Typ.t,
  ana: Typ.t,
};

// What the entry point needs to know about the focused node.
type focused = {
  is_exp: bool,
  ancestors: list(Id.t),
  ctx: Ctx.t,
  syn: Typ.t,
};

let compatible = (ctx: Ctx.t, actual: Typ.t, query: Typ.t): bool =>
  Typ.meet(ctx, actual, query) != None
  || (
    switch (
      Typ.term_of(Typ.weak_head_normalize(ctx, actual)),
      Typ.term_of(Typ.weak_head_normalize(ctx, query)),
    ) {
    | (Sum(_), Sum(_) | Var(_) | TypParamAp(_, _)) => true
    | _ => false
    }
  );

let slice =
    (
      ~focus: option(Id.t),
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
      | Some({is_exp: false, _}) => raise(Wrong_focus_sort)
      | Some({ancestors, ctx, syn, _}) =>
        if (!is_gap(query) && !compatible(ctx, syn, query)) {
          raise(Incompatible_query(query));
        };
        Id.Set.of_list(ancestors);
      }
    };
  let env = {
    focus,
    query,
    path,
  };
  let at_root = focus == None || focus == Some(root_id);
  let slice = root.dispatch(env, at_root ? query : gap);
  {
    omitted: slice.omitted,
    gamma: slice.gamma,
    psi: slice.psi,
    ana: gap,
  };
};

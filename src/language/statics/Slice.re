/* Slice.re — sliceable types.

   A sliceable type is a type paired with the routing of a query on it back to
   the terms that produced it. The routing is derived from the roles the
   checker's binding operators record on each child recursion, so a typing rule
   never writes slicing logic: `assemble` is the whole interpreter. */

type sort =
  | Value
  | Constructor
  | Alias;

type slice = {
  omitted: Id.Set.t,
  gamma: Ctx.t,
  psi: Typ.t,
};

type env = {
  focus: option(Id.t),
  query: Typ.t,
  path: Id.Set.t,
};

type t = {
  shape: Typ.t,
  ids: Id.Set.t,
  binder: bool,
  dispatch: (env, Typ.t) => slice,
  demand: (env, Ctx.t) => slice,
};

type role =
  | Part
  | Through
  | Omit
  | Source
  | Alternative
  | Binder;

type use = {
  sort,
  name: string,
  id: Id.t,
  demanded: Typ.t => Typ.t,
};

exception Focus_not_found(Id.t);
exception Wrong_focus_sort;
exception Incompatible_query(Typ.t);

let gap = Typ.gap;
let is_gap = Typ.is_gap;

let empty_gamma: Ctx.t = Ctx.empty;

let empty_slice: slice = {
  omitted: Id.Set.empty,
  gamma: empty_gamma,
  psi: gap,
};

let key_of_sort =
  fun
  | Value => 0
  | Constructor => 1
  | Alias => 2;

let entry_key = (entry: Ctx.entry): option((int, string)) =>
  switch (entry) {
  | VarEntry({name, _}) => Some((key_of_sort(Value), name))
  | ConstructorEntry({name, _}) => Some((key_of_sort(Constructor), name))
  | TVarEntry({name, _}) => Some((key_of_sort(Alias), name))
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
  |> List.find_opt(e => entry_key(e) == Some((key_of_sort(sort), name)))
  |> Option.map(entry_typ)
  |> Option.value(~default=gap);

let discharge = (~sort, ~name, gamma: Ctx.t): Ctx.t => {
  ...gamma,
  entries:
    List.filter(
      e => entry_key(e) != Some((key_of_sort(sort), name)),
      gamma.entries,
    ),
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

let fills = (children: list((role, t)), role, node: t): bool =>
  List.exists(((role, _)) => role == Part, children)
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

type placed = {
  role,
  node: t,
  query: Typ.t,
  result: option(slice),
};

let place = (ctx: Ctx.t, shape: Typ.t, children: list((role, t)), query) => {
  let fills = fills(children);
  let count =
    List.length(
      List.filter(((role, node)) => fills(role, node), children),
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
      children,
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

let assemble = (~ctx: Ctx.t, ~shape: Typ.t, ~children: list((role, t))) => {
  let dispatch = (env, query) => {
    let (placed, broadcast) = place(ctx, shape, children, query);
    let placed =
      placed
      |> forward(ctx, env, query)
      |> backward(ctx, env)
      |> sources(ctx, env);
    let slices = List.filter_map(item => item.result, placed);
    {
      ...merge(ctx, slices),
      psi:
        assembled_psi(ctx, shape, query, fills(children), broadcast, placed),
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
        List.rev(children),
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
      ~children: list((role, t))=[],
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
    | None => assemble(~ctx, ~shape, ~children)
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
    children
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

let leaf = (~ctx, ~id, ~ids, ~shape): t => mk(~ctx, ~id, ~ids, ~shape, ());

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

type result = {
  omitted: Id.Set.t,
  gamma: Ctx.t,
  psi: Typ.t,
  ana: Typ.t,
};

let focus_shell_ids = (_info_map, _id: Id.t): Id.Set.t => Id.Set.empty;

let slice =
    (~focus: option(Id.t), ~root_id: Id.t, ~path: Id.Set.t, ~root: t, query)
    : result => {
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

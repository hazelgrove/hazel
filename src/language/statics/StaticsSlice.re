open Util;
open Info;
include TypQuery;
include DemandCtx;
include SliceFocus;

let compose_route = (a: query_route, b: query_route): query_route => {
  down: q => b.down(a.down(q)),
  up: (shape, psi) => a.up(shape, b.up(a.down(shape), psi)),
};

let dependency_route = (child_shape: Typ.t): query_route => {
  down: query => is_gap(query) ? gap : child_shape,
  up: (parent_shape, child_query) =>
    is_gap(child_query) ? gap : parent_shape,
};

let at = (ty: Typ.t, route: query_route): routed(Typ.t) => {
  value: ty,
  route,
};

let component_route = (f: MatchedTyp.matcher, ctx, i): query_route => {
  down: q =>
    f(ctx, q)
    |> Option.map(List.nth_opt(_, i))
    |> Option.join
    |> Option.value(~default=gap),
  up: (shape, psi) =>
    switch (f(ctx, shape)) {
    | Some(components) =>
      typ_rebuild(
        shape,
        components |> List.mapi((j, _) => i == j ? psi : gap),
      )
      |> Option.value(~default=gap)
    | None => gap
    },
};

let decompose = (f: MatchedTyp.matcher, ctx, ana): list(routed(Typ.t)) =>
  MatchedTyp.tolerant(f, ctx, ana)
  |> List.mapi((i, c) =>
       {
         value: c,
         route: component_route(f, ctx, i),
       }
     );

let assembler_route =
    (asm: Info.assembler, shapes: list(Typ.t), anas: list(Typ.t), i: int)
    : query_route => {
  down: q => {
    let parent = asm(shapes);
    let routed = route_query(parent, List.nth(shapes, i), q);
    empty_query(routed)
      ? route_query(parent, List.nth(anas, i), q) : routed;
  },
  up: (_, psi) =>
    asm(
      List.mapi((j, shape) => j == i ? psi : query_shell(shape), shapes),
    ),
};

type result = {
  omitted: Id.Set.t,
  gamma: Ctx.t,
  psi: Typ.t,
  ana: Typ.t,
};
type exp_result = (Info.exp, Exp.t, Id.Map.t(Info.t));

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

type child = {
  mode: Info.slice_child_mode,
  node,
  route: query_route,
  type_source: option(Typ.t),
  bindings: list(binding),
  aliases: list(Ctx.tvar_entry),
  pattern: option(Info.pat),
  ascribed: bool,
};

let empty_result = {
  omitted: Id.Set.empty,
  gamma: Ctx.empty,
  psi: gap,
  ana: gap,
};

let result_join = (ctx: Ctx.t, left: result, right: result): result => {
  omitted: Id.Set.union(left.omitted, right.omitted),
  gamma: gamma_join(ctx, left.gamma, right.gamma),
  psi: meet(ctx, left.psi, right.psi),
  ana: meet(ctx, left.ana, right.ana),
};

let results_join = (ctx: Ctx.t, results: list(result)): result =>
  List.fold_left(result_join(ctx), empty_result, results);

let queried = (ty: Typ.t): result => {
  ...empty_result,
  psi: ty,
  ana: ty,
};

let source_result = (info: Info.exp, query: Typ.t): result =>
  if (is_gap(query)) {
    {
      ...empty_result,
      omitted: Id.Set.singleton(Exp.rep_id(info.user_term)),
    };
  } else {
    let references = info.slice_source.references;
    let declared =
      List.map(context_for_reference(_, query), references)
      |> List.fold_left(context_join, Ctx.empty);
    {
      ...queried(references == [] ? info.ty : query),
      ana: query,
      gamma: declared,
    };
  };

let take_children = (~id: Id.t, m: Id.Map.t(Info.t)) =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(Info.InfoSliceScratch({children, _})) => (
      children,
      Id.Map.remove(id, m),
    )
  | Some(Info.InfoExp({slice_children, _}))
  | Some(Info.InfoPat({slice_children, _})) => (slice_children, m)
  | _ => ([], m)
  };

let scratch = (id, m) =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(Info.InfoSliceScratch(trace)) => trace
  | _ => {
      children: [],
      patterns: [],
    }
  };

let update_scratch = (id, m, f) =>
  Id.Map.add(id, Info.InfoSliceScratch(f(scratch(id, m))), m);

let record =
    (
      mode,
      ~pattern=None,
      ~type_source=None,
      ~focus_shell=false,
      ~parent: Id.t,
      ~child: Id.t,
      m,
    ) =>
  if (Id.equal(parent, child)) {
    m;
  } else {
    let trace = scratch(parent, m);
    let edge: Info.slice_child = {
      mode,
      child,
      pattern,
      type_source,
      focus_shell,
    };
    let children =
      List.filter(
        (e: Info.slice_child) => !Id.equal(e.child, child),
        trace.children,
      );
    update_scratch(parent, m, trace =>
      {
        ...trace,
        children: children @ [edge],
      }
    );
  };

let record_child =
    (
      mode,
      ~type_source=None,
      ~focus_shell=false,
      ~parent: Exp.t,
      (info, elab, m): exp_result,
    )
    : exp_result => {
  let parent = Exp.rep_id(parent);
  let trace = scratch(parent, m);
  let pattern = List.nth_opt(trace.patterns, 0);
  let m =
    mode == Info.SliceAlternative && trace.patterns != []
      ? update_scratch(parent, m, trace =>
          {
            ...trace,
            patterns: List.tl(trace.patterns),
          }
        )
      : m;
  (
    info,
    elab,
    record(
      mode,
      ~pattern,
      ~type_source,
      ~focus_shell,
      ~parent,
      ~child=Exp.rep_id(info.user_term),
      m,
    ),
  );
};

let pat_edge = (mode, ~parent: Pat.t, (info: Info.pat, elab, m), k) =>
  k((
    info,
    elab,
    record(
      mode,
      ~parent=Pat.rep_id(parent),
      ~child=Pat.rep_id(info.user_term),
      m,
    ),
  ));

let edge = (mode, ~type_source=None, ~focus_shell=false, ~parent, child, k) =>
  k(record_child(mode, ~type_source, ~focus_shell, ~parent, child));
let keep = (~parent, child, k) => edge(SliceKeep, ~parent, child, k);
let omit = (~parent, child, k) => edge(SliceOmit, ~parent, child, k);
let source_child = (~parent, child, k) =>
  edge(SliceSource, ~parent, child, k);
let prune = (~parent, child, k) => edge(SlicePrune, ~parent, child, k);
let checked_source = (~parent, (annotation, child), k) =>
  edge(
    SliceOmit,
    ~type_source=Some(annotation),
    ~focus_shell=true,
    ~parent,
    child,
    k,
  );
let transparent = (~parent, child, k) =>
  edge(SliceKeep, ~focus_shell=true, ~parent, child, k);
let alias = (~parent, (definition, child), k) =>
  edge(SliceKeep, ~type_source=Some(definition), ~parent, child, k);
let matched = (~parent, child, k) => edge(SliceMatched, ~parent, child, k);
let alternative = (~parent, child, k) =>
  edge(SliceAlternative, ~parent, child, k);

let pattern = (~parent, (info: Info.pat, elab, m)) => {
  let parent_id = Exp.rep_id(parent);
  let pattern_id = Pat.rep_id(info.user_term);
  let trace = scratch(parent_id, m);
  let patterns =
    List.exists(Id.equal(pattern_id), trace.patterns)
      ? trace.patterns : trace.patterns @ [pattern_id];
  (
    info,
    elab,
    update_scratch(parent_id, m, trace =>
      {
        ...trace,
        patterns,
      }
    ),
  );
};

let rec pattern_infos = (m, pattern: Info.pat): list(Info.pat) => [
  pattern,
  ...pattern.slice_children
     |> List.filter_map((edge: Info.slice_child) =>
          switch (Id.Map.find_opt(edge.child, m)) {
          | Some(Info.InfoPat(child)) => Some(pattern_infos(m, child))
          | _ => None
          }
        )
     |> List.flatten,
];

let bindings_of = (~ctx: Ctx.t, m, pattern: Info.pat) => {
  let shape = Typ.weak_head_normalize(ctx, pattern.ty);
  pattern_infos(m, pattern)
  |> List.concat_map((info: Info.pat) => info.slice_bindings)
  |> List.map((entry: Info.slice_binding): binding =>
       {
         name: entry.name,
         id: entry.id,
         path: find_any_path(entry.typ, shape),
       }
     );
};

let binding_demand = (ctx, bindings, shape, gamma) =>
  List.fold_left(
    (demand, binding: binding) =>
      switch (lookup_demand(gamma, binding.name)) {
      | Some(query) =>
        let query =
          empty_query(query) && !is_gap(query)
            ? query : query_residual(ctx, query, query_shell(query));
        switch (binding.path) {
        | Some(path) => meet(ctx, demand, lift(shape, path, query))
        | None when List.length(bindings) == 1 => meet(ctx, demand, query)
        | None => demand
        };
      | None => demand
      },
    gap,
    bindings,
  );

let typ_ctx_at = (m, ty: Typ.t): option(Ctx.t) =>
  switch (Id.Map.find_opt(Typ.rep_id(ty), m)) {
  | Some(Info.InfoTyp({ctx, _})) => Some(ctx)
  | _ => None
  };

let rec typ_ids = (ty: Typ.t): Id.Set.t =>
  List.fold_left(
    (ids, child) => Id.Set.union(ids, typ_ids(child)),
    Id.Set.of_list(IdTagged.ids(ty)),
    typ_children(ty),
  );

let rec annotation_result =
        (
          ~direction,
          ~erase_types,
          ~minimal_aliases=true,
          m,
          ~required: Ctx.t,
          ~has_ctors,
          shape,
          demand,
          ty,
        )
        : (Ctx.t, Id.Set.t) => {
  let routed = route_query(shape, ty, demand);
  let omitted = erase_types ? ids_of_typ(ty, routed) : Id.Set.empty;
  let (context, omitted) =
    switch (Typ.term_of(ty)) {
    | Var(name) =>
      switch (Option.bind(typ_ctx_at(m, ty), tvar_entry(_, name))) {
      | Some(entry) =>
        let key = context_key(Ctx.TVarEntry(entry));
        let declared =
          List.exists(item => context_key(item) == key, required.entries);
        let supplied =
          switch (entry.kind) {
          | Singleton(definition) =>
            List.exists(
              fun
              | Ctx.TVarEntry({name, kind: Singleton(required), _}) =>
                name == entry.name && Typ.equal(definition, required)
              | _ => false,
              required.entries,
            )
          | Abstract => false
          };
        let minimal = minimal_aliases && Typ.equal(routed, ty);
        let entry =
          minimal
            ? Ctx.TVarEntry(minimal_tvar(entry)) : Ctx.TVarEntry(entry);
        (
          direction == `Ana && minimal && !has_ctors || !declared
            ? Ctx.extend(Ctx.empty, entry) : Ctx.empty,
          supplied
            ? Id.Set.union(Id.Set.of_list(IdTagged.ids(ty)), omitted)
            : omitted,
        );
      | None => (Ctx.empty, omitted)
      }
    | _ => (Ctx.empty, omitted)
    };
  List.fold_left(
    ((context, omitted), child) => {
      let (child_context, child_omitted) =
        annotation_result(
          ~direction,
          ~erase_types,
          ~minimal_aliases,
          m,
          ~required,
          ~has_ctors,
          shape,
          demand,
          child,
        );
      (
        context_join(context, child_context),
        Id.Set.union(omitted, child_omitted),
      );
    },
    (context, omitted),
    typ_children(ty),
  );
};

let pattern_has_ascription = (m, pattern) =>
  List.exists(
    (info: Info.pat) => info.slice_has_ascription,
    pattern_infos(m, pattern),
  );

type pat_trace_node = {
  pat_node: node,
  annotation_ids: Id.Set.t,
  has_bindings: bool,
};

let rec pat_node =
        (
          ~direction,
          ~erase_types,
          ~required,
          ~is_root=false,
          m,
          info: Info.pat,
        )
        : pat_trace_node => {
  let id = Pat.rep_id(info.user_term);
  let shape = info.ty;
  let annotations = info.slice_annotations;
  let constructors =
    info.slice_references
    |> List.map(context_for_reference(_, info.elab_syn_ty))
    |> List.fold_left(context_join, Ctx.empty);
  let children =
    info.slice_children
    |> List.filter_map((edge: Info.slice_child) =>
         switch (Id.Map.find_opt(edge.child, m)) {
         | Some(Info.InfoPat(child_info)) =>
           let compiled =
             pat_node(~direction, ~erase_types, ~required, m, child_info);
           Some((
             {
               mode: edge.mode,
               node: compiled.pat_node,
               route: child_info.route,
               type_source: edge.type_source,
               bindings: [],
               aliases: [],
               pattern: None,
               ascribed: false,
             },
             compiled,
           ));
         | _ => None
         }
       );
  let annotation_ids =
    List.fold_left(
      (ids, annotation) => Id.Set.union(ids, typ_ids(annotation)),
      Id.Set.empty,
      info.slice_annotations,
    )
    |> List.fold_left(
         (ids, (_, compiled)) => Id.Set.union(ids, compiled.annotation_ids),
         _,
         children,
       );
  let has_bindings =
    info.slice_bindings != []
    || List.exists(((_, compiled)) => compiled.has_bindings, children);
  let children = List.map(fst, children);
  let dispatch = (query: Typ.t): result =>
    if (is_gap(query)) {
      {
        ...empty_result,
        omitted:
          Id.Set.union(
            erase_types ? annotation_ids : Id.Set.empty,
            is_root || has_bindings ? Id.Set.empty : Id.Set.singleton(id),
          ),
      };
    } else {
      let descended =
        children
        |> List.map((child: child) =>
             child.node.dispatch(child.route.down(query))
           )
        |> results_join(info.ctx);
      let annotated =
        List.fold_left(
          ((context, omitted), ann) => {
            let (ann_context, ann_omitted) =
              annotation_result(
                ~direction,
                ~erase_types,
                m,
                ~required=
                  context_join(
                    descended.gamma,
                    context_join(constructors, required),
                  ),
                ~has_ctors=constructors.entries != [],
                shape,
                query,
                ann,
              );
            (
              context_join(context, ann_context),
              Id.Set.union(omitted, ann_omitted),
            );
          },
          (constructors, Id.Set.empty),
          annotations,
        );
      let (context, omitted) = annotated;
      {
        omitted: Id.Set.union(descended.omitted, omitted),
        gamma: context_join(context, descended.gamma),
        psi: query,
        ana: query,
      };
    };
  {
    annotation_ids,
    has_bindings,
    pat_node: {
      id,
      shape: info.elab_syn_ty,
      typ: info.ty,
      ana: info.ana,
      dispatch,
    },
  };
};

let pattern_result = (~direction, ~erase_types, m, root, demand, dependencies) =>
  switch (Id.Map.find_opt(root, m)) {
  | Some(Info.InfoPat(info)) =>
    let result =
      pat_node(
        ~direction,
        ~erase_types,
        ~required=dependencies,
        ~is_root=true,
        m,
        info,
      ).
        pat_node.
        dispatch(
        demand,
      );
    {
      ...empty_result,
      gamma: result.gamma,
      omitted: result.omitted,
    };
  | _ => empty_result
  };

type binding_slice = {
  child,
  bound: Info.pat,
  collapsed: Typ.t,
  source: Typ.t,
  raw: Typ.t,
};

let binder_omissions =
    (~module_item, gamma: Ctx.t, slice: binding_slice): Id.Set.t => {
  let child = slice.child;
  let ascribed = child.ascribed;
  let keeps_empty = !ascribed;
  let used = (binding: binding) => {
    let demanded =
      (keeps_empty || module_item) && List.length(child.bindings) == 1
        ? switch (binding.path) {
          | Some(path) when has_path(slice.collapsed, path) =>
            !is_gap(project(slice.collapsed, path))
          | None
          | Some(_) => !is_gap(slice.collapsed)
          }
        : false;
    if (demanded) {
      true;
    } else {
      switch (lookup_demand(gamma, binding.name)) {
      | Some(query) => !is_gap(query)
      | None => false
      };
    };
  };
  let (used, unused) = List.partition(used, child.bindings);
  let omitted =
    List.fold_left(
      (omitted, binding: binding) => Id.Set.add(binding.id, omitted),
      Id.Set.empty,
      unused,
    );
  let omitted =
    if (ascribed) {
      let demand = is_gap(slice.collapsed) ? slice.raw : slice.collapsed;
      let shape =
        child.mode == SliceSource && is_gap(demand)
          ? child.node.shape : slice.bound.ty;
      Id.Set.union(omitted, ids_of_typ(shape, demand));
    } else {
      omitted;
    };
  used == [] && is_gap(slice.collapsed) && is_gap(slice.raw)
    ? Id.Set.add(Pat.rep_id(slice.bound.user_term), omitted) : omitted;
};

let rec alias_source = (definition: Typ.t): Typ.t =>
  switch (Typ.term_of(definition)) {
  | Rec(_, body)
  | TypFun(_, body) => alias_source(body)
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
         | Some(name) when !List.mem(name, free) =>
           Some(TPat.rep_id(binder))
         | _ => None
         }
       )
    |> Id.Set.of_list
    |> Id.Set.union(unused_type_parameters(body, minimal_body));
  | _ => Id.Set.empty
  };

let alias_omissions =
    (
      ~source=None,
      ~preserve_parameters=false,
      children: list(child),
      context: Ctx.t,
    )
    : Id.Set.t =>
  children
  |> List.concat_map(child => child.aliases)
  |> List.map((alias: Ctx.tvar_entry) =>
       switch (alias.kind) {
       | Abstract => Id.Set.empty
       | Singleton(definition) =>
         switch (tvar_entry(context, alias.name)) {
         | Some({kind: Singleton(minimal), _}) =>
           let unused =
             preserve_parameters
               ? Id.Set.empty : unused_type_parameters(definition, minimal);
           switch (minimal) {
           | minimal when is_gap(minimal) =>
             Id.Set.union(
               Option.map(Typ.rep_id, source)
               |> Option.map(Id.Set.singleton)
               |> Option.value(~default=Id.Set.empty),
               unused,
             )
           | minimal =>
             Id.Set.union(
               ids_of_typ(alias_source(definition), alias_source(minimal)),
               unused,
             )
           };
         | Some({kind: Abstract, _})
         | None =>
           Id.Set.empty
           |> Id.Set.add(alias.id)
           |> Id.Set.add(
                Option.map(Typ.rep_id, source)
                |> Option.value(
                     ~default=Typ.rep_id(alias_source(definition)),
                   ),
              )
         }
       }
     )
  |> List.fold_left(Id.Set.union, Id.Set.empty);

let matched_type_application =
    (
      ~direction,
      ~implicit=false,
      ctx: Ctx.t,
      fn: node,
      args: Typ.t,
      query: Typ.t,
    )
    : result => {
  let rec peel = (binders, schema) =>
    switch (Typ.term_of(schema)) {
    | Poly(binder, body) =>
      implicit ? peel(binders @ [binder], body) : ([binder], body)
    | Parens(inner) => peel(binders, inner)
    | _ => (binders, schema)
    };
  let (binders, schema) = peel([], fn.typ);
  let flat_binders = List.concat_map(TPat.binders_of, binders);
  let names = List.filter_map(TPat.tyvar_of_utpat, flat_binders);
  let (matched, constraints) =
    implicit && names == []
      ? (query, [])
      : Typ.collect_constraints(
          ~replace_bound=implicit,
          ctx,
          names,
          schema,
          query,
        );
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
                  when List.mem(name, names) && is_gap(constraint_for(name)) => gap
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
                | Var(name) when implicit || is_gap(constraint_for(name)) => {
                    ...binder,
                    term: EmptyHole,
                  }
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
    (
      List.length(flat_binders) == List.length(actual_args)
        ? List.combine(flat_binders, actual_args) : []
    )
    |> List.map(((binder, arg)) =>
         switch (TPat.tyvar_of_utpat(binder)) {
         | Some(name) => ids_of_typ(arg, constraint_for(name))
         | None => Id.Set.empty
         }
       )
    |> List.fold_left(Id.Set.union, Id.Set.empty);
  let omitted =
    direction == `Ana
      ? List.fold_left(
          (omitted, binder) =>
            switch (TPat.tyvar_of_utpat(binder)) {
            | Some(name) when is_gap(constraint_for(name)) =>
              Id.Set.add(TPat.rep_id(binder), omitted)
            | _ => omitted
            },
          omitted,
          flat_binders,
        )
      : omitted;
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
      ~overlay,
      ctx: Ctx.t,
      parent_shape: Typ.t,
      children: list(child),
      query: Typ.t,
    )
    : result => {
  let directives =
    node_directives(
      ~overlay,
      ctx,
      parent_shape,
      List.map(
        child =>
          {
            view_id: child.node.id,
            view_mode: child.mode,
            view_shape: child.node.shape,
            view_ana: child.node.ana,
            view_type_source: child.type_source != None,
          },
        children,
      ),
    );
  let forward =
    children
    |> List.map(child => {
         let upwards = slice => {
           ...slice,
           psi:
             empty_query(slice.psi)
               ? gap : child.route.up(parent_shape, slice.psi),
         };
         let directive =
           Id.Map.find_opt(child.node.id, directives)
           |> Option.value(~default=Forward);
         switch (directive) {
         | ForceOmit => {
             ...empty_result,
             omitted: Id.Set.singleton(child.node.id),
           }
         | RouteUp(routed) => upwards(child.node.dispatch(routed))
         | Reverse(routed) => child.node.dispatch(routed)
         | Drop => empty_result
         | Forward
         | Suppressed =>
           let query = directive == Suppressed ? gap : query;
           let follow = prune => {
             let child_query = child.route.down(query);
             let child_query =
               child.aliases != []
               && empty_query(
                    query_residual(
                      ctx,
                      child_query,
                      query_shell(child.node.shape),
                    ),
                  )
               && !
                    empty_query(
                      query_residual(ctx, query, query_shell(parent_shape)),
                    )
                 ? query : child_query;
             upwards(
               child.node.dispatch(
                 prune && empty_query(child_query) ? gap : child_query,
               ),
             );
           };
           switch (child.mode) {
           | SliceOmit =>
             switch (child.type_source) {
             | Some(_) => {
                 ...child.node.dispatch(query),
                 omitted: Id.Set.singleton(child.node.id),
                 psi: query,
               }
             | None => {
                 ...empty_result,
                 omitted: Id.Set.singleton(child.node.id),
               }
             }
           | SliceSource
           | SliceAlternative => empty_result
           | SliceKeep => follow(false)
           | SliceMatched =>
             is_gap(query)
               ? child.node.dispatch(gap)
               : {
                 ...
                   matched_type_application(
                     ~direction=overlay.direction,
                     ~implicit=true,
                     ctx,
                     child.node,
                     TypTuple([]) |> Typ.temp,
                     Arrow(gap, query) |> Typ.temp,
                   ),
                 psi: query,
               }
           | SlicePrune => follow(true)
           };
         };
       });
  results_join(ctx, forward);
};

let matched_branch_decomposition = (ctx, demand, supplied) => {
  let supplied = Typ.weak_head_normalize(ctx, supplied);
  switch (Typ.term_of(demand), Typ.term_of(supplied)) {
  | (Sum(demanded), Sum(supplied)) =>
    let contains = name =>
      List.exists(
        fun
        | ConstructorMap.Variant(other, _, _) =>
          Constructor.equal(name, other)
        | ConstructorMap.BadEntry(_) => false,
        supplied,
      );
    Sum(
      List.map(
        fun
        | ConstructorMap.Variant(name, _, _) as variant =>
          contains(name) ? variant : ConstructorMap.BadEntry(gap)
        | ConstructorMap.BadEntry(_) => ConstructorMap.BadEntry(gap),
        demanded,
      ),
    )
    |> Typ.temp;
  | _ => query_overlap(ctx, demand, supplied)
  };
};

let slice_branches =
    (~overlay, ctx: Ctx.t, branches: list(node), query: Typ.t): result => {
  let matched = matched_query(ctx, query);
  let parametric = !Typ.equal(matched, query);
  let (slices, _) =
    List.fold_left(
      ((slices, residual), branch: node) => {
        let blocked = branch_blocked(~overlay, branch.id);
        let instantiated = MatchedTyp.poly_pair(ctx, branch.typ) != None;
        let slice =
          if (blocked || empty_query(residual)) {
            branch.dispatch(gap);
          } else if (parametric) {
            let candidate = branch.dispatch(query);
            let supplied =
              if (instantiated) {
                branch.shape;
              } else {
                switch (Typ.term_of(query)) {
                | TypParamAp({term: Var(name), _}, _) =>
                  switch (tvar_entry(candidate.gamma, name)) {
                  | Some(entry) =>
                    Typ.weak_head_normalize(
                      Ctx.extend(ctx, Ctx.TVarEntry(entry)),
                      query,
                    )
                  | None => candidate.psi
                  }
                | _ => candidate.psi
                };
              };
            let branch_query =
              matched_branch_decomposition(ctx, residual, supplied);
            let slice =
              empty_query(branch_query) ? branch.dispatch(gap) : candidate;
            {
              ...slice,
              psi: branch_query,
            };
          } else if (instantiated) {
            let candidate = branch.dispatch(residual);
            let branch_query = query_overlap(ctx, residual, candidate.psi);
            empty_query(branch_query)
              ? branch.dispatch(gap)
              : {
                ...candidate,
                psi: branch_query,
              };
          } else {
            branch.dispatch(query_overlap(ctx, residual, branch.typ));
          };
        (slices @ [slice], query_residual(ctx, residual, slice.psi));
      },
      ([], parametric ? matched : query),
      branches,
    );
  let result = results_join(ctx, slices);
  {
    ...result,
    psi: parametric ? query : result.psi,
    gamma:
      slices
      |> List.map(slice => slice.gamma)
      |> List.fold_left(context_join_branches(ctx), Ctx.empty),
  };
};

let compile = (~overlay, m: Id.Map.t(Info.t), root: Info.exp): node => {
  let {direction, focus, focus_query, path, _} = overlay;
  let rec go = (~support=Unsupported, ~seen=Id.Set.empty, info: Info.exp) => {
    let id = Exp.rep_id(info.user_term);
    let support = info.slice_group_members == [] ? support : ModuleItem;
    if (Id.Set.mem(id, seen)) {
      {
        id,
        shape: info.elab_syn_ty,
        typ: info.slice_source.schema,
        ana: info.ana,
        dispatch: query => source_result(info, query),
      };
    } else {
      let seen = Id.Set.add(id, seen);
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
               let ascribed =
                 Option.map(
                   (pattern: Info.pat) => pattern_has_ascription(m, pattern),
                   pattern,
                 )
                 |> Option.value(~default=false);
               let mode = edge.mode;
               Some({
                 mode,
                 type_source: edge.type_source,
                 bindings:
                   Option.map(bindings_of(~ctx=info.ctx, m), pattern)
                   |> Option.value(~default=[]),
                 aliases:
                   edge.mode == SliceKeep && edge.type_source != None
                     ? Ctx.added_bindings(child_info.ctx, info.ctx).entries
                       |> List.filter_map(
                            fun
                            | Ctx.TVarEntry(entry) => Some(entry)
                            | _ => None,
                          )
                     : [],
                 pattern,
                 ascribed,
                 route: child_info.route,
                 node: {
                   let node =
                     go(
                       ~support=
                         mode == SliceSource && ascribed
                           ? BindingAscription
                           : mode == SliceOmit && edge.type_source != None
                               ? ExpressionAscription : support,
                       ~seen,
                       child_info,
                     );
                   node;
                 },
               });
             | _ => None
             }
           );
      let children =
        switch (info.assemble) {
        | None => children
        | Some(asm) =>
          let kept = children |> List.filter(c => c.mode == SliceKeep);
          let keep_shapes = List.map(c => c.node.shape, kept);
          let keep_anas = List.map(c => c.node.ana, kept);
          List.fold_left(
            ((rerouted, i), c) =>
              c.mode == SliceKeep
                ? (
                  rerouted
                  @ [
                    {
                      ...c,
                      route: assembler_route(asm, keep_shapes, keep_anas, i),
                    },
                  ],
                  i + 1,
                )
                : (rerouted @ [c], i),
            ([], 0),
            children,
          )
          |> fst;
        };
      let sources = List.filter(c => c.mode == SliceSource, children);
      let nodes = mode =>
        List.filter_map(c => c.mode == mode ? Some(c.node) : None, children);
      let kept = nodes(SliceKeep);
      let alternatives = nodes(SliceAlternative);
      let typ =
        switch (Exp.term_of(info.user_term), kept) {
        | (TypAp(_, args), [fn, ..._]) =>
          applied_type(info.ctx, fn.typ, args)
        | _ =>
          children
          |> List.find_map(child =>
               child.mode == SliceMatched
                 ? Some(
                     MatchedTyp.tolerant2(
                       MatchedTyp.arrow,
                       info.ctx,
                       child.node.typ,
                     )
                     |> snd,
                   )
                 : None
             )
          |> Option.value(~default=info.slice_source.schema)
        };
      let slice_children = (children, query) =>
        slice_forward(~overlay, info.ctx, info.elab_syn_ty, children, query);
      let dispatch = query => {
        let at_focus = is_focus(focus, id);
        let query =
          focus_override(
            ~overlay,
            info.ctx,
            ~shape=info.elab_syn_ty,
            ~at_focus,
            query,
          );
        if (is_gap(query) && gap_omits_node(~overlay, ~at_focus, id)) {
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
                slice_branches(~overlay, info.ctx, alternatives, query),
                slice_children(
                  List.filter(child => child.mode == SliceOmit, children),
                  gap,
                ),
              );
            } else {
              switch (term) {
              | TypAp(_, args) =>
                switch (kept) {
                | [fn, ..._] =>
                  matched_type_application(
                    ~direction,
                    info.ctx,
                    fn,
                    args,
                    query,
                  )
                | [] => source_result(info, query)
                }
              | _ =>
                children == []
                  ? source_result(info, query)
                  : slice_children(children, query)
              };
            };
          let forward =
            switch (
              List.find_map(
                child => child.mode == SliceOmit ? child.type_source : None,
                children,
              )
            ) {
            | Some(annotation) =>
              let annotation_query =
                fill_shell(
                  info.elab_syn_ty,
                  ascription_query(~overlay, info.elab_syn_ty, forward.psi),
                );
              let (context, omitted) =
                annotation_result(
                  ~direction,
                  ~erase_types=true,
                  ~minimal_aliases=direction == `Ana,
                  m,
                  ~required=forward.gamma,
                  ~has_ctors=context_has_constructor(forward.gamma),
                  info.elab_syn_ty,
                  annotation_query,
                  annotation,
                );
              let omitted =
                Id.Set.union(
                  omitted,
                  signature_omissions(
                    m,
                    info.ctx,
                    info.elab_syn_ty,
                    annotation_query,
                  ),
                );
              {
                ...forward,
                omitted: Id.Set.union(forward.omitted, omitted),
                gamma: minimal_join(~overlay, context, forward.gamma),
              };
            | None => forward
            };
          let forward =
            ascribed_focus_omits(
              ~overlay,
              m,
              ~at_focus,
              ~support,
              forward.gamma,
            )
              ? {
                ...empty_result,
                omitted: Id.Set.add(id, forward.omitted),
                psi: query,
                ana: query,
              }
              : forward;
          let bindings =
            children
            |> List.filter_map(child =>
                 Option.map(
                   (pattern: Info.pat) => {
                     let shape =
                       Typ.weak_head_normalize(info.ctx, pattern.ty);
                     let shape =
                       empty_query(shape) && child.mode == SliceSource
                         ? Typ.weak_head_normalize(
                             info.ctx,
                             child.node.shape,
                           )
                         : shape;
                     let body =
                       binding_demand(
                         info.ctx,
                         child.bindings,
                         shape,
                         forward.gamma,
                       );
                     let parent =
                       if (child.mode != SliceKeep
                           || direction == `Ana
                           && support == ExpressionAscription) {
                         gap;
                       } else {
                         find_any_path(pattern.ty, info.elab_syn_ty)
                         |> Option.map(project(query, _))
                         |> Option.value(~default=gap);
                       };
                     let source = meet(info.ctx, body, parent);
                     let raw =
                       binding_focus_demand(
                         ~overlay,
                         m,
                         info.ctx,
                         ~on_path=Id.Set.mem(child.node.id, path),
                         ~ascribed=child.ascribed,
                         ~probe=() => child.node.dispatch(gap).psi,
                         Pat.rep_id(pattern.user_term),
                         shape,
                         source,
                       );
                     {
                       child,
                       bound: pattern,
                       collapsed:
                         empty_query(raw) && !is_gap(raw) ? gap : raw,
                       source,
                       raw,
                     };
                   },
                   child.pattern,
                 )
               );
          let body_demand =
            List.map(slice => slice.source, bindings)
            |> List.fold_left(meet(info.ctx), gap);
          let source_query = is_gap(body_demand) ? gap : body_demand;
          let deps =
            List.map(
              source => {
                let query =
                  switch (
                    List.find_opt(
                      slice => Id.equal(slice.child.node.id, source.node.id),
                      bindings,
                    )
                  ) {
                  | Some(slice) => slice.source
                  | None => source_query
                  };
                source.node.dispatch(is_gap(query) ? gap : query);
              },
              sources,
            );
          let combined =
            result_join(info.ctx, forward, results_join(info.ctx, deps));
          let path_patterns =
            bindings
            |> List.filter_map(slice =>
                 Id.Set.mem(slice.child.node.id, path)
                   ? Some(Pat.rep_id(slice.bound.user_term)) : None
               )
            |> Id.Set.of_list;
          let canonical = (slice: binding_slice) =>
            List.find_opt(
              other =>
                Id.equal(
                  Pat.rep_id(other.bound.user_term),
                  Pat.rep_id(slice.bound.user_term),
                ),
              bindings,
            )
            |> Option.value(~default=slice);
          let pattern_result =
            bindings
            |> List.map(slice => {
                 let pattern_id = Pat.rep_id(slice.bound.user_term);
                 let result =
                   pattern_result(
                     ~direction,
                     ~erase_types=
                       erase_pattern_types(
                         ~overlay,
                         ~path_patterns,
                         pattern_id,
                       ),
                     m,
                     pattern_id,
                     focused_demand(
                       ~overlay,
                       ~collapsed=slice.collapsed,
                       ~raw=slice.raw,
                     ),
                     combined.gamma,
                   );
                 let demand = canonical(slice);
                 {
                   ...result,
                   omitted:
                     Id.Set.union(
                       result.omitted,
                       binder_omissions(
                         ~module_item=support == ModuleItem,
                         forward.gamma,
                         {
                           ...slice,
                           collapsed: demand.collapsed,
                           raw: demand.raw,
                         },
                       ),
                     ),
                 };
               })
            |> results_join(info.ctx);
          let combined = result_join(info.ctx, pattern_result, combined);
          let alias_source =
            List.find_map(
              child => child.aliases != [] ? child.type_source : None,
              children,
            );
          let omitted =
            Id.Set.union(
              combined.omitted,
              alias_omissions(
                ~source=alias_source,
                ~preserve_parameters=
                  sum_definition([], expose(focus_query)) != None,
                children,
                combined.gamma,
              ),
            );
          let omitted =
            info.slice_group_members != []
            && List.for_all(Id.Set.mem(_, omitted), info.slice_group_members)
              ? Id.Set.add(id, omitted) : omitted;
          let names =
            List.concat_map(
              slice =>
                List.map(
                  (binding: binding) => binding.name,
                  slice.child.bindings,
                ),
              bindings,
            );
          let gamma = gamma_remove(combined.gamma, names);
          {
            omitted,
            gamma: minimal_context(~overlay, m, id, omitted, gamma),
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
  go(root);
};

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
    compile(
      ~overlay=overlay_for(~direction, ~focus, ~query, m),
      m,
      root_info,
    );
  let result = node.dispatch(focused ? gap : query);
  let result =
    direction == `Ana
      ? {
        ...result,
        ana: query,
      }
      : result;
  switch (direction, focus) {
  | (`Syn, Some(id)) when is_gap(query) => {
      ...result,
      omitted: Id.Set.union(result.omitted, focus_shell_ids(m, id)),
    }
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

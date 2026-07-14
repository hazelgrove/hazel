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

type child_mode =
  | Keep
  | Omit
  | Source
  | Track;

type exp_result = (Info.exp, Exp.t, Id.Map.t(Info.t));

type path = list(int);

type node = {
  id: Id.t,
  shape: Typ.t,
  dispatch: Typ.t => result,
};

type child = {
  mode: child_mode,
  node,
};

exception Focus_not_found(Id.t);
exception Wrong_focus_sort;
exception Incompatible_query(Typ.t);

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

let context_for_name = (ctx: Ctx.t, name: string): Ctx.t =>
  switch (Ctx.lookup_var(ctx, name)) {
  | Some(entry) => Ctx.extend(Ctx.empty, Ctx.VarEntry(entry))
  | None =>
    switch (Ctx.lookup_ctr(ctx, name)) {
    | Some(entry) => Ctx.extend(Ctx.empty, Ctx.ConstructorEntry(entry))
    | None => Ctx.empty
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

let rec expose = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(ty)) {
  | Parens(inner)
  | Projector(_, inner) => expose(inner)
  | _ => ty
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

let compatible_paths =
    (ctx: Ctx.t, needle: Typ.t, haystack: Typ.t): list(path) => {
  let rec go = haystack => {
    let haystack = expose(haystack);
    switch (Typ.meet(ctx, expose(needle), haystack)) {
    | Some(_) => [[]]
    | None =>
      typ_children(haystack)
      |> List.mapi((i, child) =>
           List.map(path => [i, ...path], go(child))
         )
      |> List.flatten
    };
  };
  go(haystack);
};

let rec erase_tpat = (pat: TPat.t): TPat.t =>
  switch (pat.term) {
  | Tuple(ps) => {
      ...pat,
      term: Tuple(List.map(erase_tpat, ps)),
    }
  | Parens(inner) => {
      ...pat,
      term: Parens(erase_tpat(inner)),
    }
  | _ => {
      ...pat,
      term: EmptyHole,
    }
  };

let rec empty_query = (query: Typ.t): bool =>
  if (is_gap(query)) {
    true;
  } else {
    let children = typ_children(expose(query));
    children != [] && List.for_all(empty_query, children);
  };

let rec route_query = (ctx, parent: Typ.t, child: Typ.t, query: Typ.t): Typ.t =>
  switch (find_path(child, parent)) {
  | Some(path) => project(query, path)
  | None =>
    switch (Typ.term_of(expose(child)), Typ.term_of(expose(parent))) {
    | (Poly(binder, body), _) =>
      Poly(erase_tpat(binder), route_query(ctx, parent, body, query))
      |> Typ.temp
    | (Arrow(_, _), parent_term)
        when
          switch (parent_term) {
          | Arrow(_, _) => false
          | _ => true
          } =>
      Arrow(gap, query) |> Typ.temp
    | _ =>
      switch (find_path(parent, child)) {
      | Some(path) => lift(child, path, query)
      | None =>
        let children = typ_children(expose(child));
        if (children != []) {
          let routed =
            List.map(route_query(ctx, parent, _, query), children);
          List.for_all(empty_query, routed)
            ? gap : typ_rebuild(child, routed);
        } else {
          switch (compatible_paths(ctx, child, parent)) {
          | [path] => project(query, path)
          | _ => gap
          };
        };
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
    | (Arrow(qa, qb), Arrow(sa, sb)) =>
      Arrow(subtract(ctx, qa, sa), subtract(ctx, qb, sb)) |> Typ.temp
    | (List(q), List(s)) => List(subtract(ctx, q, s)) |> Typ.temp
    | (Prod(qs), Prod(ss)) when List.length(qs) == List.length(ss) =>
      Prod(List.map2(subtract(ctx), qs, ss)) |> Typ.temp
    | (TypTuple(qs), TypTuple(ss)) when List.length(qs) == List.length(ss) =>
      TypTuple(List.map2(subtract(ctx), qs, ss)) |> Typ.temp
    | (TupLabel(ql, q), TupLabel(_, s)) =>
      TupLabel(ql, subtract(ctx, q, s)) |> Typ.temp
    | (TypParamAp(qf, qa), TypParamAp(sf, sa)) =>
      TypParamAp(subtract(ctx, qf, sf), subtract(ctx, qa, sa)) |> Typ.temp
    | _ => Typ.meet(ctx, query, supplied) == None ? query : gap
    };
  };

let ids_of_typ = (actual: Typ.t, query: Typ.t): Id.Set.t => {
  let rec go = (actual, query) =>
    if (is_gap(query)) {
      Id.Set.singleton(Typ.rep_id(actual));
    } else {
      let actual_children = typ_children(actual);
      let query_children = typ_children(query);
      List.length(actual_children) == List.length(query_children)
        ? List.map2(go, actual_children, query_children)
          |> List.fold_left(Id.Set.union, Id.Set.empty)
        : Id.Set.empty;
    };
  go(actual, query);
};

let binding_pat = (term: Exp.term): option(Pat.t) =>
  switch (term) {
  | Let(p, _, _)
  | Fun(p, _, _, _)
  | Theorem(p, _, _)
  | FixF(p, _, _)
  | Forall(p, _) => Some(p)
  | _ => None
  };

let binding_names = (term: Exp.term) =>
  switch (term) {
  | Match(_, rules) =>
    List.concat_map(((pat, _)) => Pat.bound_vars(pat), rules)
  | _ =>
    binding_pat(term)
    |> Option.map(Pat.bound_vars)
    |> Option.value(~default=[])
  };

let rec pattern_demand = (ctx: Ctx.t, pat: Pat.t, gamma: gamma): Typ.t =>
  switch (Pat.term_of(pat)) {
  | Parens(inner)
  | Projector(_, inner)
  | Asc(inner, _) => pattern_demand(ctx, inner, gamma)
  | Var(name) => VarMap.lookup(gamma, name) |> Option.value(~default=gap)
  | Tuple(ps) =>
    Prod(List.map(pattern_demand(ctx, _, gamma), ps)) |> Typ.temp
  | TupLabel(label, value) =>
    TupLabel(
      pattern_demand(ctx, label, gamma),
      pattern_demand(ctx, value, gamma),
    )
    |> Typ.temp
  | Cons(head, _) => List(pattern_demand(ctx, head, gamma)) |> Typ.temp
  | ListLit(ps) =>
    List(
      List.map(pattern_demand(ctx, _, gamma), ps)
      |> List.fold_left(meet(ctx), gap),
    )
    |> Typ.temp
  | Ap(_, payload) => pattern_demand(ctx, payload, gamma)
  | _ => gap
  };

let rec pattern_omissions =
        (pat: Pat.t, required: Typ.t, gamma: gamma): Id.Set.t =>
  if (is_gap(required)) {
    Id.Set.singleton(Pat.rep_id(pat));
  } else {
    switch (Pat.term_of(pat), Typ.term_of(expose(required))) {
    | (Var(name), _) =>
      VarMap.contains(gamma, name)
        ? Id.Set.empty : Id.Set.singleton(Pat.rep_id(pat))
    | (Parens(inner), _)
    | (Projector(_, inner), _) => pattern_omissions(inner, required, gamma)
    | (Asc(inner, annotation), _) =>
      Id.Set.union(
        pattern_omissions(inner, required, gamma),
        ids_of_typ(annotation, required),
      )
    | (Tuple(ps), Prod(qs)) when List.length(ps) == List.length(qs) =>
      List.map2((p, q) => pattern_omissions(p, q, gamma), ps, qs)
      |> List.fold_left(Id.Set.union, Id.Set.empty)
    | (TupLabel(_, value), TupLabel(_, query)) =>
      pattern_omissions(value, query, gamma)
    | (TupLabel(_, value), _) => pattern_omissions(value, required, gamma)
    | (Cons(head, tail), List(query)) =>
      Id.Set.union(
        pattern_omissions(head, query, gamma),
        pattern_omissions(tail, required, gamma),
      )
    | (ListLit(ps), List(query)) =>
      List.map(p => pattern_omissions(p, query, gamma), ps)
      |> List.fold_left(Id.Set.union, Id.Set.empty)
    | (Ap(_, payload), _) => pattern_omissions(payload, required, gamma)
    | (Constructor(_, _), Sum(_)) => Id.Set.empty
    | _ => Id.Set.singleton(Pat.rep_id(pat))
    };
  };

let context_for_constructor =
    (ctx: Ctx.t, name: string, query: Typ.t, shape: Typ.t): Ctx.t => {
  let target =
    ctx.entries
    |> List.find_map(
         fun
         | Ctx.TVarEntry({name: alias, kind: Ctx.Singleton(ty), _}) =>
           Option.bind(Typ.get_sum_constructors(ctx, ty), ctrs =>
             ConstructorMap.get_entry(name, ctrs) == None
               ? None : Some(Var(alias) |> Typ.temp)
           )
         | _ => None,
       )
    |> Option.value(~default=query);
  let entry =
    switch (Ctx.lookup_ctr_for_ana(ctx, name, Some(target))) {
    | Some(_) as entry => entry
    | None =>
      Option.map(
        (entry: Ctx.var_entry) =>
          {
            ...entry,
            typ: shape,
          },
        Ctx.lookup_var(ctx, name),
      )
    };
  let entry =
    Option.value(
      entry,
      ~default={
        name,
        id: Id.invalid,
        typ: shape,
        custom_statics: None,
      },
    );
  let entry: Ctx.var_entry = {
    ...entry,
    typ:
      ConstructorStaticsHelpers.ctr_ana_typ(ctx, target, name)
      |> Option.value(~default=entry.typ),
  };
  Ctx.extend(Ctx.empty, Ctx.ConstructorEntry(entry));
};

let source_result = (info: Info.exp, query: Typ.t): result =>
  if (is_gap(query)) {
    {
      ...empty_result,
      omitted: Id.Set.singleton(Exp.rep_id(info.user_term)),
    };
  } else {
    switch (Exp.term_of(info.user_term)) {
    | Var(name) => {
        ...queried(query),
        gamma: gamma_add(info.ctx, VarMap.empty, name, query),
        context: context_for_name(info.ctx, name),
      }
    | Constructor(name, _) => {
        ...queried(query),
        context: context_for_constructor(info.ctx, name, query, info.ty),
      }
    | _ => {
        ...queried(info.ty),
        ana: query,
      }
    };
  };

let of_info_mode =
  fun
  | Info.SliceKeep => Keep
  | Info.SliceOmit => Omit
  | Info.SliceSource => Source
  | Info.SliceTrack => Track;

let lens = (parent_shape: Typ.t, child_shape: Typ.t): option(Info.slice_lens) =>
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
  let id = Exp.rep_id(parent);
  switch (Id.Map.find_opt(id, m)) {
  | Some(Info.InfoSliceScratch(children)) => (
      List.map(
        (edge: Info.slice_child) => {
          let edge_lens =
            switch (Id.Map.find_opt(edge.child, m)) {
            | Some(Info.InfoExp(info)) =>
              lens(parent_shape, info.elab_syn_ty)
            | _ => None
            };
          {
            ...edge,
            mode:
              edge.mode == Info.SliceTrack && edge_lens == None
                ? Info.SliceOmit
                : edge.mode == Info.SliceTrack ? Info.SliceKeep : edge.mode,
            lens: edge_lens,
          };
        },
        children,
      ),
      Id.Map.remove(id, m),
    )
  | Some(Info.InfoExp({slice_children, _})) => (slice_children, m)
  | _ => ([], m)
  };
};

let record_child =
    (mode, ~parent: Exp.t, (info, elab, m): exp_result): exp_result => {
  let parent_id = Exp.rep_id(parent);
  let child_id = Exp.rep_id(info.user_term);
  if (Id.equal(parent_id, child_id)) {
    (info, elab, m);
  } else {
    let prior =
      switch (Id.Map.find_opt(parent_id, m)) {
      | Some(Info.InfoSliceScratch(children)) => children
      | _ => []
      };
    let edge: Info.slice_child = {
      mode:
        switch (mode) {
        | Keep => Info.SliceKeep
        | Omit => Info.SliceOmit
        | Source => Info.SliceSource
        | Track => Info.SliceTrack
        },
      child: child_id,
      lens: None,
    };
    let prior =
      List.filter(
        (e: Info.slice_child) => !Id.equal(e.child, child_id),
        prior,
      );
    (
      info,
      elab,
      Id.Map.add(parent_id, Info.InfoSliceScratch(prior @ [edge]), m),
    );
  };
};

let keep = (~parent, child, k) => k(record_child(Keep, ~parent, child));
let omit = (~parent, child, k) => k(record_child(Omit, ~parent, child));
let source_child = (~parent, child, k) =>
  k(record_child(Source, ~parent, child));
let track = (~parent, child, k) => k(record_child(Track, ~parent, child));

let rec matched_body =
        (bound: list(string), schema: Typ.t, query: Typ.t)
        : (Typ.t, list((string, Typ.t))) =>
  switch (Typ.term_of(schema)) {
  | Var(name) when List.mem(name, bound) => (schema, [(name, query)])
  | _ =>
    let ss = typ_children(schema);
    let qs = typ_children(query);
    if (ss != [] && List.length(ss) == List.length(qs)) {
      let pairs = List.map2(matched_body(bound), ss, qs);
      (
        typ_rebuild(schema, List.map(fst, pairs)),
        List.concat_map(snd, pairs),
      );
    } else {
      (query, []);
    };
  };

let matched_type_application =
    (ctx: Ctx.t, fn: node, args: Typ.t, query: Typ.t): result => {
  let rec peel = (binders, schema) =>
    switch (Typ.term_of(schema)) {
    | Poly(binder, body) => peel(binders @ TPat.binders_of(binder), body)
    | Parens(inner) => peel(binders, inner)
    | _ => (binders, schema)
    };
  let (binders, schema) = peel([], fn.shape);
  let names = List.filter_map(TPat.tyvar_of_utpat, binders);
  let (matched, constraints) = matched_body(names, schema, query);
  let constraint_for = name =>
    constraints
    |> List.filter_map(((n, ty)) => n == name ? Some(ty) : None)
    |> List.fold_left(meet(ctx), gap);
  let fn_query =
    List.fold_right(
      (binder, body) => Poly(binder, body) |> Typ.temp,
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
      List.length(binders) == List.length(actual_args) ? binders : [],
      List.length(binders) == List.length(actual_args) ? actual_args : [],
    )
    |> List.fold_left(Id.Set.union, Id.Set.empty);
  {
    ...slice,
    omitted: Id.Set.union(slice.omitted, omitted),
    psi: query,
  };
};

let slice_forward =
    (
      ~path=Id.Set.empty,
      ctx: Ctx.t,
      parent_shape: Typ.t,
      children: list(child),
      query: Typ.t,
    )
    : result =>
  children
  |> List.map(child =>
       if (Id.Set.mem(child.node.id, path)) {
         child.node.dispatch(gap);
       } else {
         switch (child.mode) {
         | Omit => {
             ...empty_result,
             omitted: Id.Set.singleton(child.node.id),
           }
         | Source => empty_result
         | Track => empty_result
         | Keep =>
           child.node.dispatch(
             route_query(ctx, parent_shape, child.node.shape, query),
           )
         };
       }
     )
  |> results_join(ctx);

let slice_branches =
    (~path=Id.Set.empty, ctx: Ctx.t, branches: list(node), query: Typ.t)
    : result => {
  let (slices, _) =
    List.fold_left(
      ((slices, residual), branch) => {
        let branch_query =
          Id.Set.mem(branch.id, path)
            ? gap
            : Typ.meet(ctx, branch.shape, residual) == None ? gap : residual;
        let slice = branch.dispatch(branch_query);
        (slices @ [slice], subtract(ctx, residual, slice.psi));
      },
      ([], query),
      branches,
    );
  results_join(ctx, slices);
};

let rec compile =
        (
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
      dispatch: query => source_result(info, query),
    };
  } else {
    let seen = Id.Set.add(id, seen);
    let children =
      info.slice_children
      |> List.filter_map((edge: Info.slice_child) =>
           switch (Id.Map.find_opt(edge.child, m)) {
           | Some(Info.InfoExp(child_info)) =>
             Some({
               mode: of_info_mode(edge.mode),
               node:
                 compile(~seen, ~focus, ~focus_query, ~path, m, child_info),
             })
           | _ => None
           }
         );
    let sources =
      List.filter_map(c => c.mode == Source ? Some(c.node) : None, children);
    let kept =
      List.filter_map(c => c.mode == Keep ? Some(c.node) : None, children);
    let dispatch = query => {
      let at_focus =
        switch (focus) {
        | Some(focus) => Id.equal(focus, id)
        | None => false
        };
      let query = at_focus ? focus_query : query;
      if (is_gap(query) && (at_focus || !Id.Set.mem(id, path))) {
        {
          ...empty_result,
          omitted: Id.Set.singleton(id),
        };
      } else {
        let term = Exp.term_of(info.user_term);
        let forward =
          switch (term) {
          | Fun(_, _, _, _) =>
            switch (kept, Typ.term_of(expose(query))) {
            | ([body], Arrow(_, codomain)) => body.dispatch(codomain)
            | _ =>
              slice_forward(
                ~path,
                info.ctx,
                info.elab_syn_ty,
                children,
                query,
              )
            }
          | TypAp(_, args) =>
            switch (kept) {
            | [fn, ..._] =>
              matched_type_application(info.ctx, fn, args, query)
            | [] => source_result(info, query)
            }
          | If(_, _, _)
          | Match(_, _) =>
            result_join(
              info.ctx,
              slice_branches(~path, info.ctx, kept, query),
              slice_forward(
                ~path,
                info.ctx,
                info.elab_syn_ty,
                List.filter(child => child.mode == Omit, children),
                gap,
              ),
            )
          | _ =>
            children == []
              ? source_result(info, query)
              : slice_forward(
                  ~path,
                  info.ctx,
                  info.elab_syn_ty,
                  children,
                  query,
                )
          };
        let names = binding_names(term);
        let body_demand =
          switch (term, binding_pat(term)) {
          | (Match(_, rules), _) =>
            List.map(
              ((pat, _)) => pattern_demand(info.ctx, pat, forward.gamma),
              rules,
            )
            |> List.fold_left(meet(info.ctx), gap)
          | (_, Some(pat)) => pattern_demand(info.ctx, pat, forward.gamma)
          | (_, None) => gap
          };
        let requested =
          switch (term, Typ.term_of(expose(query))) {
          | (Fun(_, _, _, _), Arrow(domain, _)) =>
            meet(info.ctx, body_demand, domain)
          | _ => empty_query(body_demand) ? gap : body_demand
          };
        let source_query = empty_query(body_demand) ? gap : body_demand;
        let deps =
          List.map(source => source.dispatch(source_query), sources);
        let combined =
          result_join(info.ctx, forward, results_join(info.ctx, deps));
        let omitted =
          switch (term, binding_pat(term)) {
          | (Match(_, rules), _) =>
            rules
            |> List.map(((pat, _)) =>
                 pattern_omissions(
                   pat,
                   {
                     let demand = pattern_demand(info.ctx, pat, forward.gamma);
                     empty_query(demand) ? gap : demand;
                   },
                   forward.gamma,
                 )
               )
            |> List.fold_left(Id.Set.union, combined.omitted)
          | (_, Some(pat)) =>
            Id.Set.union(
              combined.omitted,
              pattern_omissions(pat, requested, forward.gamma),
            )
          | (_, None) => combined.omitted
          };
        {
          ...combined,
          omitted,
          gamma: gamma_remove(combined.gamma, names),
          psi: info.ty,
          ana: query,
        };
      };
    };
    {
      id,
      shape: info.elab_syn_ty,
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

let analysis_slice = (m, root_info: Info.exp, focus, query): result => {
  let path = exp_path(m, focus);
  let base = compile(~focus=Some(focus), ~path, m, root_info).dispatch(gap);
  let focus_info =
    switch (Id.Map.find_opt(focus, m)) {
    | Some(Info.InfoExp(info)) => Some(info)
    | _ => None
    };
  let parent =
    Id.Map.fold(
      (_, info, found) =>
        switch (found, info) {
        | (Some(_), _) => found
        | (None, Info.InfoExp(parent)) =>
          parent.slice_children
          |> List.find_opt((edge: Info.slice_child) =>
               Id.equal(edge.child, focus)
             )
          |> Option.map(edge => (parent, edge))
        | _ => None
        },
      m,
      None,
    );
  let dependencies =
    switch (focus_info, parent) {
    | (Some(focused), Some((parent, edge))) =>
      let source =
        if (edge.mode == Info.SliceSource) {
          [(focus, compile(m, focused).dispatch(query))];
        } else {
          switch (Exp.term_of(focused.user_term)) {
          | Var(_)
          | Constructor(_, _) =>
            let query =
              Typ.meet(focused.ctx, focused.elab_syn_ty, query) != None
                ? query
                : route_query(
                    focused.ctx,
                    parent.elab_syn_ty,
                    focused.elab_syn_ty,
                    query,
                  );
            [(focus, compile(m, focused).dispatch(query))];
          | EmptyHole => [(focus, compile(m, focused).dispatch(query))]
          | _ when Typ.meet(focused.ctx, focused.elab_syn_ty, query) == None => [
              (focus, compile(m, focused).dispatch(focused.elab_syn_ty)),
            ]
          | _ => []
          };
        };
      let checked =
        edge.mode == Info.SliceOmit
          ? parent.slice_children
            |> List.filter_map((sibling: Info.slice_child) =>
                 if (sibling.mode != Info.SliceKeep) {
                   None;
                 } else {
                   switch (Id.Map.find_opt(sibling.child, m)) {
                   | Some(Info.InfoExp(info)) =>
                     switch (find_path(focused.ana, info.elab_syn_ty)) {
                     | Some(path) =>
                       Some((
                         sibling.child,
                         compile(m, info).dispatch(
                           lift(info.elab_syn_ty, path, query),
                         ),
                       ))
                     | None => None
                     }
                   | _ => None
                   };
                 }
               )
          : [];
      source @ checked;
    | _ => []
    };
  let result =
    List.fold_left(
      (result, (id, dependency)) =>
        {
          ...result_join(root_info.ctx, result, dependency),
          omitted:
            Id.Set.union(
              Id.Set.remove(id, result.omitted),
              dependency.omitted,
            ),
        },
      base,
      dependencies,
    );
  let annotation_omissions =
    Id.Map.fold(
      (_, info, omitted) =>
        switch (info) {
        | Info.InfoExp({user_term: {term: Asc(child, annotation), _}, _})
            when Id.Set.mem(Exp.rep_id(child), path) =>
          let annotation_query =
            switch (focus_info) {
            | Some(focused) =>
              switch (find_path(focused.ana, annotation)) {
              | Some(path) => lift(annotation, path, query)
              | None =>
                switch (
                  compatible_paths(focused.ctx, focused.ana, annotation)
                ) {
                | [path] => lift(annotation, path, query)
                | _ => query
                }
              }
            | None => query
            };
          Id.Set.union(omitted, ids_of_typ(annotation, annotation_query));
        | _ => omitted
        },
      m,
      Id.Set.empty,
    );
  let (reopened, binder_omissions) =
    Id.Map.fold(
      (_, info, (reopened, omitted)) =>
        switch (info) {
        | Info.InfoExp(info)
            when Id.Set.mem(Exp.rep_id(info.user_term), path) =>
          switch (binding_pat(Exp.term_of(info.user_term))) {
          | Some(pat) =>
            let gamma =
              info.slice_children
              |> List.exists((edge: Info.slice_child) =>
                   edge.mode == Info.SliceSource
                   && Id.Set.mem(edge.child, path)
                 )
                ? List.fold_left(
                    (gamma, name) => gamma_add(info.ctx, gamma, name, query),
                    VarMap.empty,
                    binding_names(Exp.term_of(info.user_term)),
                  )
                : VarMap.empty;
            (
              Id.Set.remove(Pat.rep_id(pat), reopened),
              Id.Set.union(omitted, pattern_omissions(pat, query, gamma)),
            );
          | None => (reopened, omitted)
          }
        | _ => (reopened, omitted)
        },
      m,
      (result.omitted, Id.Set.empty),
    );
  {
    ...result,
    omitted:
      Id.Set.union(
        Id.Set.union(reopened, annotation_omissions),
        binder_omissions,
      ),
    ana: query,
  };
};

let ids_set = ids =>
  List.fold_left((set, id) => Id.Set.add(id, set), Id.Set.empty, ids);

let focus_shell_ids = (m: Id.Map.t(Info.t), focus: Id.t): Id.Set.t => {
  let rec go = ancestors =>
    switch (ancestors) {
    | [id, ...rest] =>
      switch (Id.Map.find_opt(id, m)) {
      | Some(Info.InfoExp({user_term, _})) =>
        switch (Exp.term_of(user_term)) {
        | Parens(_)
        | Asc(_, _) =>
          Id.Set.union(ids_set(IdTagged.ids(user_term)), go(rest))
        | _ => Id.Set.empty
        }
      | _ => Id.Set.empty
      }
    | [] => Id.Set.empty
    };
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
          && !compatible_query(info.ctx, info.ty, query)) {
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
  let root_id = Exp.rep_id(root_info.user_term);
  switch (focus, direction) {
  | (Some(id), `Ana) when !Id.equal(id, root_id) =>
    analysis_slice(m, root_info, id, query)
  | _ =>
    let focused = direction == `Syn && focus != None;
    let node =
      focused
        ? compile(
            ~focus,
            ~focus_query=query,
            ~path=exp_path(m, Option.get(focus)),
            m,
            root_info,
          )
        : compile(m, root_info);
    let result = node.dispatch(focused ? gap : query);
    direction == `Ana
      ? {
        ...result,
        ana: query,
      }
      : result;
  };
};

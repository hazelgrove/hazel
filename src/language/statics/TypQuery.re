type path = list(int);
type lens = (path, path);

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

let meet = (ctx: Ctx.t, left: Typ.t, right: Typ.t): Typ.t =>
  if (is_gap(left)) {
    right;
  } else if (is_gap(right)) {
    left;
  } else {
    Typ.meet(ctx, left, right) |> Option.value(~default=left);
  };

let rec unconstrained_result = (query: Typ.t): bool =>
  switch (Typ.term_of(query)) {
  | Poly(_, body)
  | Parens(body)
  | Arrow(_, body) => unconstrained_result(body)
  | TypParamAp(_, {term: TypTuple(args), _}) => List.for_all(is_gap, args)
  | TypParamAp(_, arg) => is_gap(arg)
  | _ => is_gap(query)
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

let aligned_children = (left, right) => {
  let left = typ_children(left);
  let right = typ_children(right);
  left != [] && List.length(left) == List.length(right)
    ? Some((left, right)) : None;
};

let same_node = (left: Typ.t, right: Typ.t): bool => {
  let l = Typ.rep_id(left);
  let r = Typ.rep_id(right);
  !Id.equal(l, Id.invalid) && Id.equal(l, r) || left == right;
};

let rec transparent = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(ty)) {
  | Parens(inner)
  | Projector(_, inner) => transparent(inner)
  | _ => ty
  };

let rec expose = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(transparent(ty))) {
  | Unknown(Hole(MultiHole(items))) =>
    switch (
      List.filter_map(
        fun
        | Grammar.Typ(ty) => Some(ty)
        | _ => None,
        items,
      )
    ) {
    | [inner] => expose(inner)
    | _ => transparent(ty)
    }
  | _ => transparent(ty)
  };

let rec find_path_by =
        (~normalize, ~equal, ~right, needle, haystack): option(path) => {
  let needle = normalize(needle);
  let haystack = normalize(haystack);
  if (equal(needle, haystack)) {
    Some([]);
  } else {
    let children =
      typ_children(haystack)
      |> List.mapi((i, child) => (i, child))
      |> (right ? List.rev : (x => x));
    children
    |> List.find_map(((i, child)) =>
         Option.map(
           path => [i, ...path],
           find_path_by(~normalize, ~equal, ~right, needle, child),
         )
       );
  };
};

let find_path =
  find_path_by(~normalize=transparent, ~equal=same_node, ~right=false);
let find_path_right =
  find_path_by(~normalize=x => x, ~equal=same_node, ~right=true);
let find_shape_path =
  find_path_by(~normalize=expose, ~equal=Typ.equal, ~right=false);
let find_any_path = (needle, haystack) =>
  switch (find_path(needle, haystack)) {
  | Some(_) as path => path
  | None => find_shape_path(needle, haystack)
  };

let rec at_path = (query: Typ.t, path: path): option(Typ.t) =>
  switch (path) {
  | [] => Some(expose(query))
  | [i, ...rest] =>
    Option.bind(
      List.nth_opt(typ_children(expose(query)), i),
      at_path(_, rest),
    )
  };

let project = (query, path) =>
  Option.value(at_path(query, path), ~default=gap);

let has_path = (query, path) => at_path(query, path) != None;

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
  lift(child_shape, snd(lens), project(query, fst(lens)));

let lens_up = (lens: lens, parent_shape: Typ.t, supplied: Typ.t): Typ.t =>
  lift(parent_shape, fst(lens), project(supplied, snd(lens)));

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

let rec fill_shell = (shape, query) =>
  if (is_gap(query)) {
    query_shell(shape);
  } else {
    switch (aligned_children(shape, query)) {
    | Some((shape_children, query_children)) =>
      typ_rebuild(
        shape,
        List.map2(fill_shell, shape_children, query_children),
      )
    | None => query
    };
  };

let rec route_query = (parent: Typ.t, child: Typ.t, query: Typ.t): Typ.t =>
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
    };
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
                (p, (c, q)) => route_query(p, c, q),
                ps,
                List.combine(cs, qs),
              )
            : List.map(route_query(parent, _, query), cs);
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
      switch (aligned_children(query, supplied)) {
      | Some((queries, supplied)) =>
        typ_rebuild(query, List.map2(subtract(ctx), queries, supplied))
      | None =>
        let query' = Typ.weak_head_normalize(ctx, query);
        let supplied' = Typ.weak_head_normalize(ctx, supplied);
        if (!Typ.equal(query, query') || !Typ.equal(supplied, supplied')) {
          subtract(ctx, query', supplied');
        } else {
          Typ.meet(ctx, query, supplied) == None ? query : gap;
        };
      }
    };
  };

let query_residual = (ctx, query, supplied) => {
  let query = subtract(ctx, query, supplied);
  empty_query(query) ? gap : query;
};

let matched_query = (ctx, query) => {
  let rec peel = (binders, definition) =>
    switch (Typ.term_of(definition)) {
    | TypFun(pattern, body) =>
      peel(binders @ TPat.binders_of(pattern), body)
    | _ => (binders, definition)
    };
  switch (Typ.term_of(query)) {
  | TypParamAp({term: Var(name), _}, arguments) =>
    switch (Ctx.lookup_tvar(ctx, name)) {
    | Some(Singleton(definition)) =>
      let (binders, body) = peel([], definition);
      let arguments =
        switch (Typ.term_of(arguments)) {
        | TypTuple(arguments) => arguments
        | _ => [arguments]
        };
      let binders = List.filter_map(TPat.tyvar_of_utpat, binders);
      let bindings =
        List.length(binders) == List.length(arguments)
          ? List.combine(binders, arguments) : [];
      let rec go = ty =>
        switch (Typ.term_of(ty)) {
        | Var(name) =>
          List.find_map(
            ((bound, query)) => bound == name ? Some(query) : None,
            bindings,
          )
          |> Option.value(~default=gap)
        | TypParamAp(fn, arg) =>
          let arg = go(arg);
          empty_query(arg) ? gap : TypParamAp(fn, arg) |> Typ.temp;
        | Sum(variants) =>
          Sum(
            List.map(
              fun
              | ConstructorMap.Variant(name, ids, Some(payload)) => {
                  let payload = go(payload);
                  empty_query(payload)
                    ? ConstructorMap.BadEntry(gap)
                    : ConstructorMap.Variant(name, ids, Some(payload));
                }
              | ConstructorMap.Variant(_, _, None)
              | ConstructorMap.BadEntry(_) => ConstructorMap.BadEntry(gap),
              variants,
            ),
          )
          |> Typ.temp
        | _ =>
          let children = typ_children(ty);
          children == [] ? gap : typ_rebuild(ty, List.map(go, children));
        };
      bindings == [] ? query : go(body);
    | _ => query
    }
  | _ => query
  };
};

let query_overlap = (ctx, query, supplied) => {
  let supplied =
    switch (Typ.term_of(query)) {
    | Sum(_) => Typ.weak_head_normalize(ctx, supplied)
    | _ => supplied
    };
  let overlap =
    query_residual(ctx, query, query_residual(ctx, query, supplied));
  !unconstrained_result(query) && unconstrained_result(overlap)
    ? gap : overlap;
};

let matched_overlap = (ctx, query, supplied) => {
  let supplied = Typ.weak_head_normalize(ctx, supplied);
  switch (Typ.term_of(query), Typ.term_of(supplied)) {
  | (Sum(queries), Sum(supplied))
      when List.length(queries) == List.length(supplied) =>
    let children = typ_children(query);
    typ_rebuild(
      query,
      List.map2(
        ((query, supplied), child) =>
          switch (query, supplied) {
          | (
              ConstructorMap.Variant(name, _, _),
              ConstructorMap.Variant(other, _, _),
            )
              when name == other => child
          | _ => gap
          },
        List.combine(queries, supplied),
        children,
      ),
    );
  | _ => query_residual(ctx, query, query_residual(ctx, query, supplied))
  };
};

let ids_of_typ = (actual: Typ.t, query: Typ.t): Id.Set.t => {
  let rec go = (actual, query) =>
    if (is_gap(query)) {
      Id.Set.singleton(Typ.rep_id(actual));
    } else {
      let omitted_entries =
        switch (Typ.term_of(actual), Typ.term_of(query)) {
        | (Sum(actual), Sum(query))
            when List.length(actual) == List.length(query) =>
          List.map2(
            (actual, query) =>
              switch (actual, query) {
              | (
                  ConstructorMap.Variant(_, ann, _),
                  ConstructorMap.BadEntry(q),
                )
                  when is_gap(q) =>
                Id.Set.of_list(ann.ids)
              | _ => Id.Set.empty
              },
            actual,
            query,
          )
          |> List.fold_left(Id.Set.union, Id.Set.empty)
        | _ => Id.Set.empty
        };
      let omitted_children =
        switch (aligned_children(actual, query)) {
        | Some((actual, query)) =>
          List.map2(go, actual, query)
          |> List.fold_left(Id.Set.union, Id.Set.empty)
        | None => Id.Set.empty
        };
      Id.Set.union(omitted_entries, omitted_children);
    };
  go(actual, query);
};

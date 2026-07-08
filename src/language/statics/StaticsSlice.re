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

type sty = {
  shape: Typ.t,
  dispatch: Typ.t => result,
  finalize: unit => result,
};

type node = {
  id: Id.t,
  ids: Id.Set.t,
  ty: sty,
};

type child_mode =
  | Keep
  | Omit
  | Source;

type child = {
  mode: child_mode,
  node,
};

type exp_result = (Info.exp, Exp.t, Id.Map.t(Info.t));

exception Focus_not_found(Id.t);
exception Wrong_focus_sort;
exception Incompatible_query(Typ.t);
exception Contains_focus;

let gap: Typ.t = Typ.temp(Unknown(Hole(EmptyHole)));
let unknown: Typ.t = Typ.temp(Unknown(Internal));

let rec is_gap = (ty: Typ.t): bool =>
  switch (Typ.term_of(ty)) {
  | Parens(inner) => is_gap(inner)
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

let ids_set = (ids: list(Id.t)): Id.Set.t =>
  List.fold_left((acc, id) => Id.Set.add(id, acc), Id.Set.empty, ids);

let typ_all_ids = (ty: Typ.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let note = ids =>
    acc := List.fold_left((s, id) => Id.Set.add(id, s), acc^, ids);
  let _ =
    Typ.map_term(
      ~f_typ=
        (continue, t) => {
          note(IdTagged.ids(t));
          note([Typ.rep_id(t)]);
          continue(t);
        },
      ~f_tpat=
        (continue, tp) => {
          note(IdTagged.ids(tp));
          note([TPat.rep_id(tp)]);
          continue(tp);
        },
      ty,
    );
  acc^;
};

let meet = (ctx: Ctx.t, left: Typ.t, right: Typ.t): Typ.t =>
  switch (Typ.meet(ctx, left, right)) {
  | Some(ty) => ty
  | None => left
  };

let meet_empty = meet(Ctx.empty);

let meet_demands = (left: Typ.t, right: Typ.t): Typ.t =>
  if (is_gap(left)) {
    right;
  } else if (is_gap(right)) {
    left;
  } else {
    meet_empty(left, right);
  };

let exp_subtree_ids = (e: Exp.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let note = ids =>
    acc := List.fold_left((s, id) => Id.Set.add(id, s), acc^, ids);
  ignore(
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          note(IdTagged.ids(e));
          continue(e);
        },
      ~f_pat=
        (continue, p) => {
          note(IdTagged.ids(p));
          continue(p);
        },
      ~f_typ=
        (continue, t) => {
          note(IdTagged.ids(t));
          continue(t);
        },
      ~f_tpat=
        (continue, tp) => {
          note(IdTagged.ids(tp));
          continue(tp);
        },
      e,
    ),
  );
  acc^;
};

let queried_result = (query: Typ.t): result => {
  omitted: Id.Set.empty,
  gamma: VarMap.empty,
  psi: query,
  context: Ctx.empty,
  ana: query,
};

let omitted_node = (id: Id.t): result => {
  omitted: Id.Set.singleton(id),
  gamma: VarMap.empty,
  psi: gap,
  context: Ctx.empty,
  ana: gap,
};

let omitted_nodes = (ids: Id.Set.t): result => {
  omitted: ids,
  gamma: VarMap.empty,
  psi: gap,
  context: Ctx.empty,
  ana: gap,
};

let gamma_add = (gamma: gamma, name: string, ty: Typ.t): gamma =>
  if (is_gap(ty)) {
    gamma;
  } else {
    switch (VarMap.lookup(gamma, name)) {
    | None => VarMap.extend(gamma, (name, ty))
    | Some(existing) =>
      VarMap.update(gamma, name, _ => meet_empty(existing, ty))
    };
  };

let gamma_join = (left: gamma, right: gamma): gamma =>
  List.fold_left(
    (acc, (name, ty)) => gamma_add(acc, name, ty),
    left,
    VarMap.to_list(right),
  );

let context_entry_key = (entry: Ctx.entry): option((string, int)) =>
  switch (entry) {
  | Ctx.VarEntry({name, _}) => Some((name, 0))
  | Ctx.ConstructorEntry({name, _}) => Some((name, 1))
  | Ctx.TVarEntry({name, _}) => Some((name, 2))
  | _ => None
  };

let sliced_variant_meet =
    (
      left: ConstructorMap.variant(Typ.t),
      right: ConstructorMap.variant(Typ.t),
    )
    : ConstructorMap.variant(Typ.t) =>
  switch (left, right) {
  | (Variant(_) as v, BadEntry(b)) when is_gap(b) => v
  | (BadEntry(b), Variant(_) as v) when is_gap(b) => v
  | (Variant(name, ids, Some(l)), Variant(_, _, Some(r))) =>
    Variant(name, ids, Some(meet_empty(l, r)))
  | _ => left
  };

let rec sliced_alias_meet = (left: Typ.t, right: Typ.t): Typ.t =>
  switch (Typ.term_of(left), Typ.term_of(right)) {
  | (Parens(l), _) => sliced_alias_meet(l, right)
  | (_, Parens(r)) => sliced_alias_meet(left, r)
  | (TypFun(binder, l), TypFun(_, r)) =>
    TypFun(binder, sliced_alias_meet(l, r)) |> Typ.temp
  | (Poly(binder, l), Poly(_, r)) =>
    Poly(binder, sliced_alias_meet(l, r)) |> Typ.temp
  | (Rec(binder, l), Rec(_, r)) =>
    Rec(binder, sliced_alias_meet(l, r)) |> Typ.temp
  | (Sum(ls), Sum(rs)) when List.length(ls) == List.length(rs) =>
    Sum(List.map2(sliced_variant_meet, ls, rs)) |> Typ.temp
  | _ => meet_empty(left, right)
  };

let context_entry_join = (left: Ctx.entry, right: Ctx.entry): Ctx.entry =>
  switch (left, right) {
  | (Ctx.VarEntry(l), Ctx.VarEntry(r)) =>
    Ctx.VarEntry({
      ...l,
      typ: meet_empty(l.typ, r.typ),
    })
  | (Ctx.ConstructorEntry(l), Ctx.ConstructorEntry(r)) =>
    Ctx.ConstructorEntry({
      ...l,
      typ: meet_empty(l.typ, r.typ),
    })
  | (
      Ctx.TVarEntry({kind: Ctx.Singleton(l_ty), _} as l),
      Ctx.TVarEntry({kind: Ctx.Singleton(r_ty), _}),
    ) =>
    Ctx.TVarEntry({
      ...l,
      kind: Ctx.Singleton(sliced_alias_meet(l_ty, r_ty)),
    })
  | _ => left
  };

let context_join = (left: Ctx.t, right: Ctx.t): Ctx.t => {
  ...left,
  entries:
    List.fold_left(
      (entries, r_entry) => {
        let key = context_entry_key(r_entry);
        key != None && List.exists(e => context_entry_key(e) == key, entries)
          ? List.map(
              e =>
                context_entry_key(e) == key
                  ? context_entry_join(e, r_entry) : e,
              entries,
            )
          : entries @ [r_entry];
      },
      left.entries,
      right.entries,
    ),
};

let context_join_all = (contexts: list(Ctx.t)): Ctx.t =>
  List.fold_left(context_join, Ctx.empty, contexts);

let result_join = (left: result, right: result): result => {
  omitted: Id.Set.union(left.omitted, right.omitted),
  gamma: gamma_join(left.gamma, right.gamma),
  psi: meet_empty(left.psi, right.psi),
  context: context_join(left.context, right.context),
  ana: meet_empty(left.ana, right.ana),
};

let results_join = (results: list(result)): result =>
  List.fold_left(result_join, empty_result, results);

let demand_of = (names: list(Var.t), gamma): Typ.t =>
  List.fold_left(
    (acc, name) =>
      switch (VarMap.lookup(gamma, name)) {
      | Some(ty) => meet_empty(acc, ty)
      | None => acc
      },
    gap,
    names,
  );

let gamma_discharge = (gamma, names: list(Var.t)): gamma =>
  VarMap.filter(((name, _)) => !List.mem(name, names), gamma);

let with_deps = (base: result, deps: result): result => {
  omitted: Id.Set.union(base.omitted, deps.omitted),
  gamma: gamma_join(base.gamma, deps.gamma),
  psi: base.psi,
  context: context_join(base.context, deps.context),
  ana: base.ana,
};

let with_omitted = (omitted: Id.Set.t, ty: sty): sty => {
  shape: ty.shape,
  dispatch: query => {
    let slice = ty.dispatch(query);
    {
      ...slice,
      omitted: Id.Set.union(omitted, slice.omitted),
    };
  },
  finalize: ty.finalize,
};

let with_self_gap = (~id: Id.t, ty: sty): sty => {
  shape: ty.shape,
  dispatch: query => is_gap(query) ? omitted_node(id) : ty.dispatch(query),
  finalize: ty.finalize,
};

let with_self_gap_ids = (~ids: Id.Set.t, ty: sty): sty => {
  shape: ty.shape,
  dispatch: query =>
    is_gap(query) ? omitted_nodes(ids) : ty.dispatch(query),
  finalize: ty.finalize,
};

let source = (~id: Id.t, shape: Typ.t): sty =>
  with_self_gap(
    ~id,
    {
      shape,
      dispatch: queried_result,
      finalize: () => empty_result,
    },
  );

let context_for_name = (ctx: Ctx.t, name: string): Ctx.t =>
  switch (Ctx.lookup_var(ctx, name)) {
  | Some(entry) => Ctx.extend(Ctx.empty, Ctx.VarEntry(entry))
  | None =>
    switch (Ctx.lookup_ctr(ctx, name)) {
    | Some(entry) => Ctx.extend(Ctx.empty, Ctx.ConstructorEntry(entry))
    | None => Ctx.empty
    }
  };

let rec alias_head = (ty: Typ.t): option(string) =>
  switch (Typ.term_of(ty)) {
  | Parens(inner) => alias_head(inner)
  | Arrow(_, out) => alias_head(out)
  | Poly(_, body)
  | TypFun(_, body) => alias_head(body)
  | Var(name) => Some(name)
  | TypParamAp(callee, _) => alias_head(callee)
  | _ => None
  };

let rec constructor_schema_from_sum =
        (name: string, result_ty: Typ.t, sum_ty: Typ.t): option(Typ.t) =>
  switch (Typ.term_of(sum_ty)) {
  | Parens(inner) => constructor_schema_from_sum(name, result_ty, inner)
  | TypParamAp({term: Var(ctr), _}, payload) when ctr == name =>
    Some(Arrow(payload, result_ty) |> Typ.temp)
  | Sum(variants) =>
    variants
    |> List.find_map(
         fun
         | ConstructorMap.Variant(ctr, _, payload) when ctr == name =>
           Some(
             switch (payload) {
             | Some(payload) => Arrow(payload, result_ty) |> Typ.temp
             | None => result_ty
             },
           )
         | ConstructorMap.Variant(_)
         | ConstructorMap.BadEntry(_) => None,
       )
  | _ => None
  };

let rec strip_gap_arrows = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(ty)) {
  | Arrow(domain, out) when is_gap(domain) => strip_gap_arrows(out)
  | _ => ty
  };

let rec query_constructor_head = (query: Typ.t): option(string) =>
  switch (Typ.term_of(query)) {
  | Parens(inner) => query_constructor_head(inner)
  | Arrow(_, out) => query_constructor_head(out)
  | TypParamAp({term: Var(name), _}, _) => Some(name)
  | _ => None
  };

let rec is_arrow_query = (query: Typ.t): bool =>
  switch (Typ.term_of(query)) {
  | Parens(inner) => is_arrow_query(inner)
  | Arrow(_, _) => true
  | _ => false
  };

let rec alias_constructors = (ty: Typ.t): list(string) =>
  switch (Typ.term_of(ty)) {
  | Parens(t)
  | Rec(_, t)
  | TypFun(_, t)
  | Poly(_, t) => alias_constructors(t)
  | Sum(variants) =>
    List.filter_map(
      fun
      | ConstructorMap.Variant(name, _, _) => Some(name)
      | ConstructorMap.BadEntry(_) => None,
      variants,
    )
  | TypParamAp({term: Var(name), _}, _) => [name]
  | _ => []
  };

let ctor_alias_name = (ctx: Ctx.t, name: string): option(string) =>
  ctx.entries
  |> List.find_map(
       fun
       | Ctx.TVarEntry({name: alias, kind: Ctx.Singleton(alias_shape), _})
           when List.mem(name, alias_constructors(alias_shape)) =>
         Some(alias)
       | _ => None,
     );

let alias_for_constructor =
    (ctx: Ctx.t, name: string): option((string, Typ.t)) =>
  ctx.entries
  |> List.find_map(
       fun
       | Ctx.TVarEntry({name: alias, kind: Ctx.Singleton(alias_shape), _})
           when
             constructor_schema_from_sum(
               name,
               Var(alias) |> Typ.temp,
               alias_shape,
             )
             != None =>
         Some((alias, alias_shape))
       | _ => None,
     );

let constructor_context_query =
    (ctx: Ctx.t, name: string, shape: Typ.t, query: Typ.t): Typ.t =>
  switch (Typ.term_of(shape), Typ.term_of(query)) {
  | (Arrow(shape_in, shape_out), Arrow(query_in, query_out)) =>
    let out =
      switch (Typ.term_of(shape_out), Typ.term_of(query_out)) {
      | (_, Arrow(_, _)) => strip_gap_arrows(query_out)
      | (Arrow(_, _), _) => query_out
      | _ => query_out
      };
    Arrow(is_gap(query_in) ? shape_in : query_in, out) |> Typ.temp;
  | (Arrow(shape_in, _), _) => Arrow(shape_in, query) |> Typ.temp
  | _ =>
    switch (query_constructor_head(query), alias_for_constructor(ctx, name)) {
    | (Some(query_name), Some((alias, alias_shape))) when query_name == name =>
      switch (
        constructor_schema_from_sum(
          name,
          Var(alias) |> Typ.temp,
          alias_shape,
        )
      ) {
      | Some(schema) => schema
      | None => query
      }
    | _ =>
      switch (alias_head(query)) {
      | Some(alias) =>
        switch (Ctx.lookup_alias(ctx, alias)) {
        | Some(sum_ty) =>
          switch (
            constructor_schema_from_sum(
              name,
              strip_gap_arrows(query),
              sum_ty,
            )
          ) {
          | Some(schema) => schema
          | None => query
          }
        | None => query
        }
      | None => query
      }
    }
  };

let exp_constructor_head = (exp: Exp.t): option(string) =>
  switch (Exp.term_of(exp)) {
  | Constructor(name, _) => Some(name)
  | Ap(_, fn, _) =>
    switch (Exp.term_of(fn)) {
    | Constructor(name, _) => Some(name)
    | _ => None
    }
  | _ => None
  };

let rec minimal_sum_for_constructor =
        (~preserve_payload=false, ~name: string, shape: Typ.t): option(Typ.t) =>
  switch (Typ.term_of(shape)) {
  | Rec(_, body) =>
    minimal_sum_for_constructor(~preserve_payload, ~name, body)
  | TypFun(binder, body) =>
    switch (minimal_sum_for_constructor(~preserve_payload, ~name, body)) {
    | Some(body) => Some(TypFun(binder, body) |> Typ.temp)
    | None => None
    }
  | Sum(variants) =>
    Some(
      Sum(
        List.map(
          fun
          | ConstructorMap.Variant(ctr, ann, payload) when ctr == name => {
              let payload =
                switch (payload, preserve_payload) {
                | (Some(payload), true) =>
                  switch (Typ.term_of(payload)) {
                  | Var(_) => Some(payload)
                  | _ => Some(gap)
                  }
                | (Some(_), false) => Some(gap)
                | (None, _) => None
                };
              ConstructorMap.Variant(ctr, ann, payload);
            }
          | ConstructorMap.Variant(_)
          | ConstructorMap.BadEntry(_) => ConstructorMap.BadEntry(gap),
          variants,
        ),
      )
      |> Typ.temp,
    )
  | _ => None
  };

let alias_context_for_constructor =
    (ctx: Ctx.t, name: string, query: Typ.t, shape: Typ.t): Ctx.t => {
  let alias_and_shape =
    switch (alias_head(query)) {
    | Some(alias) =>
      Some((
        alias,
        switch (Ctx.lookup_alias(ctx, alias)) {
        | Some(alias_shape) => alias_shape
        | None => shape
        },
      ))
    | None =>
      ctx.entries
      |> List.find_map(
           fun
           | Ctx.TVarEntry({
               name: alias,
               kind: Ctx.Singleton(alias_shape),
               _,
             })
               when minimal_sum_for_constructor(~name, alias_shape) != None =>
             Some((alias, alias_shape))
           | _ => None,
         )
    };
  let shape =
    switch (alias_and_shape) {
    | Some((_, alias_shape)) =>
      minimal_sum_for_constructor(~name, alias_shape)
    | None => minimal_sum_for_constructor(~name, shape)
    };
  switch (alias_and_shape, shape) {
  | (Some((alias, _)), Some(ty)) =>
    Ctx.extend_alias(Ctx.empty, alias, Id.invalid, ty)
  | _ => Ctx.empty
  };
};

let constructor_entry_context =
    (ctx: Ctx.t, name: string, query: Typ.t): Ctx.t =>
  switch (Ctx.lookup_ctr_for_ana(ctx, name, Some(query))) {
  | Some(entry) =>
    let typ =
      query_constructor_head(query) == Some(name) ? entry.typ : query;
    Ctx.extend(
      Ctx.empty,
      Ctx.ConstructorEntry({
        ...entry,
        typ,
      }),
    );
  | None =>
    switch (Ctx.lookup_var(ctx, name)) {
    | Some(entry) =>
      let typ =
        query_constructor_head(query) == Some(name) ? entry.typ : query;
      Ctx.extend(
        Ctx.empty,
        Ctx.ConstructorEntry({
          ...entry,
          typ,
        }),
      );
    | None =>
      Ctx.extend(
        Ctx.empty,
        Ctx.ConstructorEntry({
          name,
          id: Id.invalid,
          typ: query,
          custom_statics: None,
        }),
      )
    }
  };

let context_for_constructor =
    (ctx: Ctx.t, name: string, shape: Typ.t, query: Typ.t, alias_query: Typ.t)
    : Ctx.t => {
  let ctor_ctx = constructor_entry_context(ctx, name, query);
  context_join_all([
    alias_context_for_constructor(ctx, name, alias_query, shape),
    ctor_ctx,
  ]);
};

let explicit_constructor_alias_context = (ctx: Ctx.t, name: string): Ctx.t =>
  ctx.entries
  |> List.find_map(
       fun
       | Ctx.TVarEntry({name: alias, kind: Ctx.Singleton(alias_shape), _})
           when
             minimal_sum_for_constructor(
               ~preserve_payload=true,
               ~name,
               alias_shape,
             )
             != None =>
         Some((alias, alias_shape))
       | _ => None,
     )
  |> (
    fun
    | Some((alias, alias_shape)) =>
      switch (
        minimal_sum_for_constructor(
          ~preserve_payload=true,
          ~name,
          alias_shape,
        )
      ) {
      | Some(ty) => Ctx.extend_alias(Ctx.empty, alias, Id.invalid, ty)
      | None => Ctx.empty
      }
    | None => Ctx.empty
  );

let assume = (ctx: Ctx.t, name: string, shape: Typ.t): sty => {
  shape,
  dispatch: query => {
    omitted: Id.Set.empty,
    gamma: gamma_add(VarMap.empty, name, query),
    psi: query,
    context: context_for_name(ctx, name),
    ana: query,
  },
  finalize: () => empty_result,
};

let constructor = (ctx: Ctx.t, name: string, shape: Typ.t): sty =>
  with_self_gap(
    ~id=Id.invalid,
    {
      shape,
      dispatch: query => {
        let context_query =
          constructor_context_query(ctx, name, shape, query);
        {
          omitted: Id.Set.empty,
          gamma: VarMap.empty,
          psi: query,
          context:
            context_for_constructor(ctx, name, shape, context_query, query),
          ana: query,
        };
      },
      finalize: () => empty_result,
    },
  );

let var = (~id: Id.t, ~entry: sty): sty =>
  with_self_gap(
    ~id,
    {
      shape: entry.shape,
      dispatch: query => entry.dispatch(query),
      finalize: entry.finalize,
    },
  );

let prod = (children: list(sty)): sty => {
  shape: Prod(List.map(child => child.shape, children)) |> Typ.temp,
  dispatch: query => {
    let label_payload = (name: string, queries: list(Typ.t)): option(Typ.t) =>
      List.find_map(
        query =>
          switch (Typ.term_of(query)) {
          | TupLabel({term: Label(label), _}, payload) when label == name =>
            Some(payload)
          | _ => None
          },
        queries,
      );
    let query_for_child = (queries: list(Typ.t), child: sty): Typ.t =>
      switch (Typ.term_of(child.shape)) {
      | TupLabel({term: Label(name), _}, _) =>
        Option.value(label_payload(name, queries), ~default=gap)
      | _ => gap
      };
    let rec split_query = query =>
      switch (Typ.term_of(query)) {
      | Parens(inner) => split_query(inner)
      | Prod(queries) when List.length(queries) == List.length(children) => queries
      | Prod([{term: TupLabel({term: Label(_), _}, _), _} as single]) =>
        List.map(_ => single, children)
      | Prod(queries) =>
        List.map(child => query_for_child(queries, child), children)
      | _ when List.length(children) == 1 => [query]
      | _ => List.map(_ => gap, children)
      };
    let queries = split_query(query);
    let slices =
      List.map2((child, query) => child.dispatch(query), children, queries);
    let psi =
      is_gap(query)
        ? gap : Prod(List.map(slice => slice.psi, slices)) |> Typ.temp;
    {
      ...results_join(slices),
      psi,
    };
  },
  finalize: () => empty_result,
};

let tup_label = (~shape: Typ.t, child: sty): sty => {
  shape,
  dispatch: query => {
    let child_query =
      switch (Typ.term_of(query)) {
      | TupLabel(_, payload) => payload
      | _ => query
      };
    let slice = child.dispatch(child_query);
    let psi =
      switch (Typ.term_of(query)) {
      | TupLabel(label, _) when !is_gap(query) =>
        TupLabel(label, slice.psi) |> Typ.temp
      | _ => slice.psi
      };
    {
      ...slice,
      psi,
    };
  },
  finalize: () => empty_result,
};

let dot_receiver_query =
    (term: Exp.term, receiver_shape: Typ.t, query: Typ.t): Typ.t =>
  switch (term, Typ.term_of(receiver_shape)) {
  | (Dot(_, {term: Label(name), _}), Prod(fields)) =>
    Prod(
      List.map(
        field =>
          switch (Typ.term_of(field)) {
          | TupLabel({term: Label(label), _} as label_ty, _)
              when label == name =>
            TupLabel(label_ty, query) |> Typ.temp
          | _ => gap
          },
        fields,
      ),
    )
    |> Typ.temp
  | _ => query
  };

let dot = (~shape: Typ.t, ~term: Exp.term, receiver: sty): sty => {
  shape,
  dispatch: query =>
    receiver.dispatch(dot_receiver_query(term, receiver.shape, query)),
  finalize: () => empty_result,
};

let rec strip_typ_parens = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(ty)) {
  | Parens(inner) => strip_typ_parens(inner)
  | _ => ty
  };

let module_field_query = (name: string, query: Typ.t): option(Typ.t) =>
  switch (Typ.term_of(strip_typ_parens(query))) {
  | Prod(fields) =>
    List.find_map(
      field =>
        switch (Typ.term_of(field)) {
        | TupLabel({term: Label(label), _}, payload) when label == name =>
          Some(payload)
        | _ => None
        },
      fields,
    )
  | _ => None
  };

let rec pat_annotation = (pat: Pat.t): option(Typ.t) =>
  switch (Pat.term_of(pat)) {
  | Parens(inner) => pat_annotation(inner)
  | Asc(_, ty) => Some(ty)
  | _ => None
  };

let rec typ_omissions = (actual: Typ.t, query: Typ.t): Id.Set.t =>
  if (is_gap(query)) {
    ids_set(IdTagged.ids(actual));
  } else {
    switch (Typ.term_of(actual), Typ.term_of(query)) {
    | (Parens(actual), Parens(query)) => typ_omissions(actual, query)
    | (Parens(actual), _) => typ_omissions(actual, query)
    | (Arrow(actual_in, actual_out), Arrow(query_in, query_out)) =>
      Id.Set.union(
        typ_omissions(actual_in, query_in),
        typ_omissions(actual_out, query_out),
      )
    | (Prod(actuals), Prod(queries))
    | (TypTuple(actuals), TypTuple(queries))
        when List.length(actuals) == List.length(queries) =>
      List.map2(typ_omissions, actuals, queries)
      |> List.fold_left(Id.Set.union, Id.Set.empty)
    | (List(actual), List(query)) => typ_omissions(actual, query)
    | (TupLabel(_, actual), TupLabel(_, query)) =>
      typ_omissions(actual, query)
    | (TypParamAp(actual_fn, actual_arg), TypParamAp(query_fn, query_arg)) =>
      Id.Set.union(
        typ_omissions(actual_fn, query_fn),
        typ_omissions(actual_arg, query_arg),
      )
    | (Sig(items), Prod(queries))
        when List.length(items) == List.length(queries) =>
      List.map2(sig_item_omissions, items, queries)
      |> List.fold_left(Id.Set.union, Id.Set.empty)
    | (Sig(items), Sig(query_items))
        when List.length(items) == List.length(query_items) =>
      List.map2(
        (item: Sig.t, query_item: Sig.t) =>
          switch (item.term, query_item.term) {
          | (SigLet(_), SigLet(qp)) =>
            switch (pat_annotation(qp)) {
            | Some(qty) => sig_item_omissions(item, qty)
            | None => Id.Set.empty
            }
          | (SigType(_, t), SigType(_, qt)) => typ_omissions(t, qt)
          | _ => Id.Set.empty
          },
        items,
        query_items,
      )
      |> List.fold_left(Id.Set.union, Id.Set.empty)
    | _ => Id.Set.empty
    };
  }
and sig_item_omissions = (item: Sig.t, query: Typ.t): Id.Set.t =>
  if (is_gap(query)) {
    ids_set(IdTagged.ids(item));
  } else {
    switch (item.term, Typ.term_of(query)) {
    | (SigLet(p), TupLabel(_, payload)) =>
      switch (pat_annotation(p)) {
      | Some(actual) => typ_omissions(actual, payload)
      | None => Id.Set.empty
      }
    | (SigLet(p), _) =>
      switch (pat_annotation(p)) {
      | Some(actual) => typ_omissions(actual, query)
      | None => Id.Set.empty
      }
    | (SigType(_, t), _) => typ_omissions(t, query)
    | _ => Id.Set.empty
    };
  };

let asc = (~shape: Typ.t, ~term: Exp.term, child: sty): sty => {
  shape,
  dispatch: query => {
    let child_query =
      switch (term) {
      | Asc(exp, _) =>
        switch (exp_constructor_head(exp)) {
        | Some(_) => query
        | None => gap
        }
      | _ => gap
      };
    let slice = child.dispatch(child_query);
    let omitted =
      switch (term) {
      | Asc(_, ty) => typ_omissions(ty, query)
      | _ => Id.Set.empty
      };
    {
      ...slice,
      omitted: Id.Set.union(slice.omitted, omitted),
    };
  },
  finalize: () => empty_result,
};

let list_lit = (~shape: Typ.t, children: list(sty)): sty => {
  shape,
  dispatch: query => {
    let inner_query =
      switch (Typ.term_of(query)) {
      | List(inner) => inner
      | _ => gap
      };
    let slices = List.map(child => child.dispatch(inner_query), children);
    let psi =
      is_gap(query) ? gap : List(results_join(slices).psi) |> Typ.temp;
    {
      ...results_join(slices),
      psi,
    };
  },
  finalize: () => empty_result,
};

let cons = (~shape: Typ.t, head: sty): sty => {
  shape,
  dispatch: query => {
    let inner_query =
      switch (Typ.term_of(query)) {
      | List(inner) => inner
      | _ => gap
      };
    let slice = head.dispatch(inner_query);
    let psi = is_gap(query) ? gap : List(slice.psi) |> Typ.temp;
    {
      ...slice,
      psi,
    };
  },
  finalize: () => empty_result,
};

let list_concat = (~shape: Typ.t, left: sty): sty => {
  shape,
  dispatch: query => {
    let left_query =
      switch (Typ.term_of(query)) {
      | List(inner) when is_gap(inner) => gap
      | _ => query
      };
    left.dispatch(left_query);
  },
  finalize: () => empty_result,
};

let rec tpat_gap = (binder: TPat.t): TPat.t =>
  switch (binder.term) {
  | Tuple(binders) => {
      ...binder,
      term: Tuple(List.map(tpat_gap, binders)),
    }
  | Parens(inner) => {
      ...binder,
      term: Parens(tpat_gap(inner)),
    }
  | _ => {
      ...binder,
      term: EmptyHole,
    }
  };

let instantiate_poly_body = (binder: TPat.t, body: Typ.t): Typ.t => {
  let binders = TPat.binders_of(binder);
  Typ.subst_many(List.map(_ => gap, binders), binders, body);
};

let rec leading_poly_shape = (shape: Typ.t): (list(TPat.t), Typ.t) =>
  switch (Typ.term_of(shape)) {
  | Parens(inner) => leading_poly_shape(inner)
  | Poly(binder, body) =>
    let (binders, body) =
      leading_poly_shape(instantiate_poly_body(binder, body));
    ([binder, ...binders], body);
  | _ => ([], shape)
  };

let wrap_poly_query = (binders: list(TPat.t), query: Typ.t): Typ.t =>
  List.fold_right(
    (binder, query) => Poly(tpat_gap(binder), query) |> Typ.temp,
    binders,
    query,
  );

let ap_fn_query = (fn: sty, query: Typ.t): Typ.t => {
  let (binders, _) = leading_poly_shape(fn.shape);
  wrap_poly_query(binders, Arrow(gap, query) |> Typ.temp);
};

let ap_shape = (~shape: Typ.t, fn: sty): Typ.t =>
  if (is_gap(shape)) {
    let (_, fn_shape) = leading_poly_shape(fn.shape);
    switch (Typ.term_of(fn_shape)) {
    | Arrow(_, out) => out
    | _ => shape
    };
  } else {
    shape;
  };

let ap = (~shape: Typ.t, fn: sty): sty => {
  shape: ap_shape(~shape, fn),
  dispatch: query => fn.dispatch(ap_fn_query(fn, query)),
  finalize: () => empty_result,
};

let rec typ_contains = (needle: Typ.t, haystack: Typ.t): bool =>
  if (is_gap(haystack)) {
    false;
  } else if (Typ.fast_equal(needle, haystack)) {
    true;
  } else {
    switch (Typ.term_of(haystack)) {
    | Parens(inner)
    | List(inner)
    | TupLabel(_, inner) => typ_contains(needle, inner)
    | TypParamAp(callee, arg) =>
      typ_contains(needle, callee) || typ_contains(needle, arg)
    | Arrow(left, right) =>
      typ_contains(needle, left) || typ_contains(needle, right)
    | Prod(items)
    | TypTuple(items) => List.exists(typ_contains(needle), items)
    | Sum(variants) =>
      List.exists(
        fun
        | ConstructorMap.Variant(_, _, Some(payload)) =>
          typ_contains(needle, payload)
        | ConstructorMap.Variant(_, _, None) => false
        | ConstructorMap.BadEntry(entry) => typ_contains(needle, entry),
        variants,
      )
    | Poly(_, body)
    | TypFun(_, body)
    | Rec(_, body) => typ_contains(needle, body)
    | _ => false
    };
  };

let typ_args = (arg: Typ.t): list(Typ.t) =>
  switch (Typ.term_of(arg)) {
  | TypTuple(args) => args
  | _ => [arg]
  };

let omitted_type_args = (args: Typ.t, query: Typ.t): Id.Set.t =>
  typ_args(args)
  |> List.filter(arg => !typ_contains(arg, query))
  |> List.map(arg => ids_set(IdTagged.ids(arg)))
  |> List.fold_left(Id.Set.union, Id.Set.empty);

let binder_name = (binder: TPat.t): option(string) =>
  TPat.tyvar_of_utpat(binder);

let binder_is_used = (used: list((string, bool)), binder: TPat.t): bool =>
  switch (binder_name(binder)) {
  | Some(name) =>
    switch (List.assoc_opt(name, used)) {
    | Some(used) => used
    | None => true
    }
  | None => true
  };

let rec mask_tpat = (used: list((string, bool)), binder: TPat.t): TPat.t =>
  switch (binder.term) {
  | Tuple(binders) => {
      ...binder,
      term: Tuple(List.map(mask_tpat(used), binders)),
    }
  | Parens(inner) => {
      ...binder,
      term: Parens(mask_tpat(used, inner)),
    }
  | _ when !binder_is_used(used, binder) => {
      ...binder,
      term: EmptyHole,
    }
  | _ => binder
  };

let mask_poly_body = (used: list((TPat.t, bool)), body: Typ.t): Typ.t =>
  List.fold_left(
    (body, (binder, used)) => used ? body : Typ.subst(gap, binder, body),
    body,
    used,
  );

let rec schema_query = (schema: Typ.t, args: Typ.t, query: Typ.t): Typ.t =>
  switch (Typ.term_of(schema)) {
  | Poly(binder, body) =>
    let arg_list = typ_args(args);
    let binders = TPat.binders_of(binder);
    let mask = (taken: list(Typ.t), body: Typ.t): Typ.t => {
      let used_flags = List.map(arg => typ_contains(arg, query), taken);
      let used_by_binder = List.combine(binders, used_flags);
      let used_by_name =
        used_by_binder
        |> List.filter_map(((binder, used)) =>
             switch (binder_name(binder)) {
             | Some(name) => Some((name, used))
             | None => None
             }
           );
      Poly(
        mask_tpat(used_by_name, binder),
        mask_poly_body(used_by_binder, body),
      )
      |> Typ.temp;
    };
    let n = List.length(binders);
    if (List.length(arg_list) == n) {
      mask(arg_list, body);
    } else if (n >= 1 && List.length(arg_list) > n) {
      let taken = List.filteri((i, _) => i < n, arg_list);
      let rest = List.filteri((i, _) => i >= n, arg_list);
      let rest_args =
        switch (rest) {
        | [only] => only
        | _ => TypTuple(rest) |> Typ.temp
        };
      mask(taken, schema_query(body, rest_args, query));
    } else {
      schema;
    };
  | _ => schema
  };

let rec poly_binders_body = (ty: Typ.t): (list(TPat.t), Typ.t) =>
  switch (Typ.term_of(ty)) {
  | Parens(inner) => poly_binders_body(inner)
  | Poly(binder, body) =>
    let (rest, inner) = poly_binders_body(body);
    (TPat.binders_of(binder) @ rest, inner);
  | _ => ([], ty)
  };

let typ_ap_shape =
    (~ctx: Ctx.t, ~shape: Typ.t, ~term: Exp.term, fn: sty): Typ.t =>
  switch (term) {
  | TypAp(fn_exp, args) =>
    let (binders, body) = poly_binders_body(fn.shape);
    let arg_list = typ_args(args);
    if (List.length(binders) == List.length(arg_list) && binders != []) {
      meet_empty(shape, Typ.subst_many(arg_list, binders, body));
    } else {
      switch (Exp.term_of(fn_exp)) {
      | Constructor(name, _) =>
        switch (ctor_alias_name(ctx, name)) {
        | Some(alias) =>
          Arrow(gap, TypParamAp(Var(alias) |> Typ.temp, args) |> Typ.temp)
          |> Typ.temp
        | None => shape
        }
      | _ => shape
      };
    };
  | _ => shape
  };

let typ_ap = (~ctx: Ctx.t, ~shape: Typ.t, ~term: Exp.term, fn: sty): sty => {
  shape: typ_ap_shape(~ctx, ~shape, ~term, fn),
  dispatch: query => {
    let schema =
      switch (term) {
      | TypAp(_, args) => schema_query(fn.shape, args, query)
      | _ => fn.shape
      };
    let slice = fn.dispatch(schema);
    let omitted =
      switch (term) {
      | TypAp(_, args) => omitted_type_args(args, query)
      | _ => Id.Set.empty
      };
    let context =
      switch (term) {
      | TypAp(fn_exp, _) when !is_gap(query) =>
        switch (Exp.term_of(fn_exp)) {
        | Constructor(name, _) =>
          context_join(
            slice.context,
            explicit_constructor_alias_context(ctx, name),
          )
        | _ => slice.context
        }
      | _ => slice.context
      };
    {
      ...slice,
      omitted: Id.Set.union(slice.omitted, omitted),
      context,
    };
  },
  finalize: () => empty_result,
};

let deferred_domain_queries = (query: Typ.t, count: int): list(Typ.t) =>
  switch (Typ.term_of(query)) {
  | Arrow(domain, _) =>
    switch (Typ.term_of(domain)) {
    | Prod(domains) when List.length(domains) == count => domains
    | _ when count == 1 => [domain]
    | _ => List.init(count, _ => gap)
    }
  | _ => List.init(count, _ => gap)
  };

let query_codomain = (query: Typ.t): Typ.t =>
  switch (Typ.term_of(query)) {
  | Arrow(_, out) => out
  | _ => query
  };

let rec align_deferred_queries =
        (args: list(Exp.t), deferred: list(Typ.t)): list(Typ.t) =>
  switch (args, deferred) {
  | ([], _) => []
  | ([arg, ...args], [query, ...deferred]) when Exp.is_deferral(arg) => [
      query,
      ...align_deferred_queries(args, deferred),
    ]
  | ([arg, ...args], []) when Exp.is_deferral(arg) => [
      gap,
      ...align_deferred_queries(args, []),
    ]
  | ([_, ...args], deferred) => [
      gap,
      ...align_deferred_queries(args, deferred),
    ]
  };

let deferred_fn_query =
    (fn: sty, arg_queries: list(Typ.t), query: Typ.t): Typ.t => {
  let (binders, fn_shape) = leading_poly_shape(fn.shape);
  let out = query_codomain(query);
  let domain =
    switch (Typ.term_of(fn_shape), arg_queries) {
    | (Arrow({term: Prod(_), _}, _), [_, ..._] as queries) =>
      Prod(queries) |> Typ.temp
    | (Arrow(_, _), [query]) => query
    | _ => gap
    };
  wrap_poly_query(binders, Arrow(domain, out) |> Typ.temp);
};

let deferred_ap = (~shape: Typ.t, ~term: Exp.term, children: list(sty)): sty => {
  shape,
  dispatch: query => {
    switch (term, children) {
    | (DeferredAp(_, args), [fn, ...arg_children]) =>
      let deferred_count = List.filter(Exp.is_deferral, args) |> List.length;
      let arg_queries =
        align_deferred_queries(
          args,
          deferred_domain_queries(query, deferred_count),
        );
      let fn_slice = fn.dispatch(deferred_fn_query(fn, arg_queries, query));
      let arg_slices =
        List.map2(
          (arg_child, arg_query) => arg_child.dispatch(arg_query),
          arg_children,
          arg_queries,
        );
      let result = results_join([fn_slice, ...arg_slices]);
      {
        ...result,
        psi: query,
        ana: query,
      };
    | _ => empty_result
    };
  },
  finalize: () => empty_result,
};

let rec branch_query_all_gap = (ty: Typ.t): bool =>
  is_gap(ty)
  || (
    switch (Typ.term_of(ty)) {
    | Parens(t)
    | TupLabel(_, t)
    | List(t) => branch_query_all_gap(t)
    | Prod(ts)
    | TypTuple(ts) => ts != [] && List.for_all(branch_query_all_gap, ts)
    | Arrow(a, b) => branch_query_all_gap(a) && branch_query_all_gap(b)
    | TypParamAp(_, args) => branch_query_all_gap(args)
    | Sum(variants) =>
      variants != []
      && List.for_all(
           fun
           | ConstructorMap.Variant(_, _, None) => false
           | ConstructorMap.Variant(_, _, Some(t)) => branch_query_all_gap(t)
           | ConstructorMap.BadEntry(t) => branch_query_all_gap(t),
           variants,
         )
    | _ => false
    }
  );

let collapse_gap_query = (ty: Typ.t): Typ.t =>
  branch_query_all_gap(ty) ? gap : ty;

let rec typ_param_ap_parts = (ty: Typ.t): option((Typ.t, list(Typ.t))) =>
  switch (Typ.term_of(ty)) {
  | Parens(inner) => typ_param_ap_parts(inner)
  | TypParamAp(fn, arg) =>
    switch (typ_param_ap_parts(fn)) {
    | Some((head, args)) => Some((head, args @ typ_args(arg)))
    | None => Some((fn, typ_args(arg)))
    }
  | _ => None
  };

let rebuild_typ_param_ap = (head: Typ.t, args: list(Typ.t)): Typ.t =>
  switch (args) {
  | [arg] => TypParamAp(head, arg) |> Typ.temp
  | _ => TypParamAp(head, TypTuple(args) |> Typ.temp) |> Typ.temp
  };

let typ_constructor_head = (ty: Typ.t): option(string) =>
  switch (typ_param_ap_parts(ty)) {
  | Some(({term: Var(name), _}, _)) => Some(name)
  | _ => None
  };

let rec split_branch_query = (supplied: Typ.t, query: Typ.t): (Typ.t, Typ.t) =>
  if (is_gap(query)) {
    (gap, gap);
  } else if (is_gap(supplied)) {
    (gap, query);
  } else {
    switch (typ_param_ap_parts(supplied), typ_param_ap_parts(query)) {
    | (Some((s_head, s_args)), Some((q_head, q_args)))
        when
          Typ.fast_equal(s_head, q_head)
          && List.length(s_args) == List.length(q_args) =>
      let pairs = List.map2(split_branch_query, s_args, q_args);
      let taken_args = List.map(fst, pairs);
      let residual_args = List.map(snd, pairs);
      let residual =
        List.for_all(branch_query_all_gap, residual_args)
          ? gap : rebuild_typ_param_ap(q_head, residual_args);
      let taken =
        List.for_all(branch_query_all_gap, taken_args) && !is_gap(residual)
          ? gap : rebuild_typ_param_ap(q_head, taken_args);
      (taken, residual);
    | _ =>
      switch (Typ.term_of(supplied), Typ.term_of(query)) {
      | (_, Sum(variants)) =>
        switch (typ_constructor_head(supplied)) {
        | Some(name) =>
          switch (split_sum_variant_query(name, supplied, variants)) {
          | Some(split) => split
          | None => (query, gap)
          }
        | None => (query, gap)
        }
      | (Poly(_, _), _) =>
        let (_, body) = leading_poly_shape(supplied);
        split_branch_query(body, query);
      | (Parens(inner), _) => split_branch_query(inner, query)
      | (_, Parens(inner)) => split_branch_query(supplied, inner)
      | (Prod(ss), Prod(qs)) when List.length(ss) == List.length(qs) =>
        split_branch_query_list(ss, qs, ts => Prod(ts) |> Typ.temp)
      | (TypTuple(ss), TypTuple(qs)) when List.length(ss) == List.length(qs) =>
        split_branch_query_list(ss, qs, ts => TypTuple(ts) |> Typ.temp)
      | (Arrow(s_in, s_out), Arrow(q_in, q_out)) =>
        let (t_in, r_in) = split_branch_query(s_in, q_in);
        let (t_out, r_out) = split_branch_query(s_out, q_out);
        (
          collapse_gap_query(Arrow(t_in, t_out) |> Typ.temp),
          collapse_gap_query(Arrow(r_in, r_out) |> Typ.temp),
        );
      | (List(s), List(q)) =>
        let (taken, residual) = split_branch_query(s, q);
        (
          collapse_gap_query(List(taken) |> Typ.temp),
          collapse_gap_query(List(residual) |> Typ.temp),
        );
      | (TupLabel(_, s), TupLabel(label, q)) =>
        let (taken, residual) = split_branch_query(s, q);
        (
          collapse_gap_query(TupLabel(label, taken) |> Typ.temp),
          collapse_gap_query(TupLabel(label, residual) |> Typ.temp),
        );
      | _ => (query, gap)
      }
    };
  }
and split_branch_query_list =
    (ss: list(Typ.t), qs: list(Typ.t), rebuild: list(Typ.t) => Typ.t)
    : (Typ.t, Typ.t) => {
  let pairs = List.map2(split_branch_query, ss, qs);
  (
    collapse_gap_query(rebuild(List.map(fst, pairs))),
    collapse_gap_query(rebuild(List.map(snd, pairs))),
  );
}
and split_sum_variant_query =
    (
      name: string,
      supplied: Typ.t,
      variants: list(ConstructorMap.variant(Typ.t)),
    )
    : option((Typ.t, Typ.t)) => {
  let entries =
    List.map(
      variant =>
        switch (variant) {
        | ConstructorMap.Variant(ctr, ann, Some(payload)) when ctr == name =>
          let (taken, residual) = split_branch_query(supplied, payload);
          (
            true,
            branch_query_all_gap(taken)
              ? ConstructorMap.BadEntry(gap)
              : ConstructorMap.Variant(ctr, ann, Some(taken)),
            branch_query_all_gap(residual)
              ? ConstructorMap.BadEntry(gap)
              : ConstructorMap.Variant(ctr, ann, Some(residual)),
          );
        | ConstructorMap.Variant(ctr, _, None) when ctr == name => (
            true,
            variant,
            ConstructorMap.BadEntry(gap),
          )
        | ConstructorMap.Variant(_)
        | ConstructorMap.BadEntry(_) => (
            false,
            ConstructorMap.BadEntry(gap),
            variant,
          )
        },
      variants,
    );
  List.exists(((matched, _, _)) => matched, entries)
    ? Some((
        collapse_gap_query(
          Sum(List.map(((_, taken, _)) => taken, entries)) |> Typ.temp,
        ),
        collapse_gap_query(
          Sum(List.map(((_, _, residual)) => residual, entries))
          |> Typ.temp,
        ),
      ))
    : None;
};

let constructors_in_context = (ctx: Ctx.t): list(string) =>
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({name, _}) => Some(name)
    | _ => None,
    ctx.entries,
  );

let branch_sum_query =
    (branch: sty, variants: list(ConstructorMap.variant(Typ.t)))
    : option((Typ.t, Typ.t)) =>
  switch (
    branch.dispatch(Sum(variants) |> Typ.temp).context
    |> constructors_in_context
  ) {
  | [name] =>
    switch (split_sum_variant_query(name, branch.shape, variants)) {
    | Some(split) => Some(split)
    | None => Some((gap, Sum(variants) |> Typ.temp))
    }
  | _ => None
  };

let split_branch_sty_query = (branch: sty, query: Typ.t): (Typ.t, Typ.t) =>
  switch (Typ.term_of(query)) {
  | Sum(variants) =>
    switch (branch_sum_query(branch, variants)) {
    | Some(split) => split
    | None => split_branch_query(branch.shape, query)
    }
  | _ => split_branch_query(branch.shape, query)
  };

let branch_queries =
    (~sum_split=false, branches: list(sty), query: Typ.t): list(Typ.t) => {
  let split = (branch: sty, residual) =>
    sum_split
      ? split_branch_sty_query(branch, residual)
      : split_branch_query(branch.shape, residual);
  let (taken_rev, _) =
    List.fold_left(
      ((acc, residual), branch: sty) => {
        let (taken, rest) = split(branch, residual);
        ([taken, ...acc], rest);
      },
      ([], query),
      branches,
    );
  let takens = List.rev(taken_rev);
  switch (takens) {
  | [_, ...rest] when !is_gap(query) && List.for_all(is_gap, takens) => [
      query,
      ...rest,
    ]
  | _ => takens
  };
};

let rec strip_poly_query = (query: Typ.t): Typ.t =>
  switch (Typ.term_of(query)) {
  | Parens(inner) => strip_poly_query(inner)
  | Poly(_, body) => strip_poly_query(body)
  | _ => query
  };

let typ_abs = (~shape: Typ.t, body: sty): sty => {
  shape,
  dispatch: query => body.dispatch(strip_poly_query(query)),
  finalize: () => empty_result,
};

let if_ = (~shape: Typ.t, children: list(sty)): sty => {
  shape,
  dispatch: query =>
    switch (children) {
    | [cond, cons] =>
      results_join([
        cond.dispatch(Atom(Bool) |> Typ.temp),
        cons.dispatch(query),
      ])
    | [cond, ...branches] =>
      results_join([
        cond.dispatch(Atom(Bool) |> Typ.temp),
        ...List.map2(
             (branch: sty, branch_query) => branch.dispatch(branch_query),
             branches,
             branch_queries(~sum_split=true, branches, query),
           ),
      ])
    | _ => empty_result
    },
  finalize: () => empty_result,
};

let node_of = (~id, ~ids, ty): node => {
  id,
  ids,
  ty,
};

let to_info_mode =
  fun
  | Keep => Info.SliceKeep
  | Omit => Info.SliceOmit
  | Source => Info.SliceSource;

let of_info_mode =
  fun
  | Info.SliceKeep => Keep
  | Info.SliceOmit => Omit
  | Info.SliceSource => Source;

let take_children = (~parent: Exp.t, m: Id.Map.t(Info.t)) => {
  let parent_id = Exp.rep_id(parent);
  switch (Id.Map.find_opt(parent_id, m)) {
  | Some(Info.InfoSliceScratch(children)) => (
      children,
      Id.Map.remove(parent_id, m),
    )
  | Some(Info.InfoExp({slice_children, _})) => (slice_children, m)
  | _ => ([], m)
  };
};

let record_child =
    (mode: child_mode, ~parent: Exp.t, (info, elab, m): exp_result)
    : exp_result => {
  let parent_id = Exp.rep_id(parent);
  let child_id = Exp.rep_id(info.user_term);
  if (Id.equal(parent_id, child_id)) {
    (info, elab, m);
  } else {
    let child_edge: Info.slice_child = {
      mode: to_info_mode(mode),
      child: child_id,
    };
    let prior =
      (
        switch (Id.Map.find_opt(parent_id, m)) {
        | Some(Info.InfoSliceScratch(children)) => children
        | _ => []
        }
      )
      |> List.filter((child: Info.slice_child) =>
           !Id.equal(child.child, child_id)
         );
    let m =
      Id.Map.add(parent_id, Info.InfoSliceScratch(prior @ [child_edge]), m);
    (info, elab, m);
  };
};

let keep = (~parent: Exp.t, child: exp_result, k: exp_result => 'a): 'a =>
  k(record_child(Keep, ~parent, child));

let omit = (~parent: Exp.t, child: exp_result, k: exp_result => 'a): 'a =>
  k(record_child(Omit, ~parent, child));

let source_child =
    (~parent: Exp.t, child: exp_result, k: exp_result => 'a): 'a =>
  k(record_child(Source, ~parent, child));

let child_tys = (mode, children: list(child)) =>
  children
  |> List.filter_map((child: child) =>
       child.mode == mode ? Some(child.node.ty) : None
     );

let omitted_ids = (children: list(child)) =>
  children
  |> List.filter((child: child) => child.mode == Omit)
  |> List.fold_left(
       (ids, child: child) => Id.Set.union(ids, child.node.ids),
       Id.Set.empty,
     );

let source_ids_of_children = (children: list(child)) =>
  children
  |> List.filter((child: child) => child.mode == Source)
  |> List.fold_left(
       (ids, child: child) => Id.Set.union(ids, child.node.ids),
       Id.Set.empty,
     );

let binding_pat = (term: Exp.term): option(Pat.t) =>
  switch (term) {
  | Let(p, _, _)
  | Fun(p, _, _, _)
  | Theorem(p, _, _)
  | Forall(p, _) => Some(p)
  | ModuleExp(mp, _, _) => Some(ExpandModule.mpat_to_pat(mp))
  | _ => None
  };

let binding_names = (term: Exp.term): list(Var.t) =>
  switch (binding_pat(term)) {
  | Some(p) => Pat.bound_vars(p)
  | None => []
  };

let rec pat_all_ids = (pat: Pat.t): list(Id.t) =>
  IdTagged.ids(pat)
  @ (
    switch (Pat.term_of(pat)) {
    | Parens(p)
    | Projector(_, p)
    | Asc(p, _) => pat_all_ids(p)
    | Tuple(ps)
    | ListLit(ps) => List.concat_map(pat_all_ids, ps)
    | Cons(a, b)
    | Ap(a, b)
    | TupLabel(a, b) => pat_all_ids(a) @ pat_all_ids(b)
    | _ => []
    }
  );

let pat_subtree_ids = (pat: Pat.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let note = ids =>
    acc := List.fold_left((s, id) => Id.Set.add(id, s), acc^, ids);
  ignore(
    Pat.map_term(
      ~f_pat=
        (continue, p) => {
          note(IdTagged.ids(p));
          continue(p);
        },
      ~f_typ=
        (continue, t) => {
          note(IdTagged.ids(t));
          continue(t);
        },
      ~f_tpat=
        (continue, tp) => {
          note(IdTagged.ids(tp));
          continue(tp);
        },
      pat,
    ),
  );
  acc^;
};

let module_binding_names = (item: Mod.t): list(string) =>
  switch (item.term) {
  | ModLet(p, _) => Pat.bound_vars(p)
  | ModuleMod(mp, _) => ExpandModule.mpat_names(mp)
  | _ => []
  };

let module_item_value_ids = (item: Mod.t): Id.Set.t =>
  switch (item.term) {
  | ModExp(e)
  | ModuleMod(_, e) => exp_subtree_ids(e)
  | ModLet(_, _)
  | ModType(_)
  | EmptyHole
  | Invalid(_)
  | MultiHole(_) => Id.Set.empty
  };

let module_item_adjustment =
    (items: list(Mod.t), query: Typ.t): (Id.Set.t, Id.Set.t) =>
  List.fold_left(
    ((add, remove), item) => {
      let mentioned =
        List.exists(
          name => module_field_query(name, query) != None,
          module_binding_names(item),
        );
      if (mentioned) {
        switch (item.term) {
        | ModLet(p, _) => (
            add,
            Id.Set.union(remove, ids_set(pat_all_ids(p))),
          )
        | _ => (add, Id.Set.union(remove, module_item_value_ids(item)))
        };
      } else {
        switch (module_binding_names(item)) {
        | [] => (add, remove)
        | _ => (Id.Set.add(Mod.rep_id(item), add), remove)
        };
      };
    },
    (Id.Set.empty, Id.Set.empty),
    items,
  );

let module_ = (~shape: Typ.t, ~term: Exp.term, child: sty): sty => {
  shape,
  dispatch: query => {
    let slice = child.dispatch(query);
    switch (term) {
    | Module(items) =>
      let (add, remove) = module_item_adjustment(items, query);
      {
        ...slice,
        omitted: Id.Set.union(Id.Set.diff(slice.omitted, remove), add),
      };
    | _ => slice
    };
  },
  finalize: () => empty_result,
};

let rec projected_module_field =
        (module_names: list(string), body: Exp.t): option(string) =>
  switch (Exp.term_of(body)) {
  | Parens(inner) => projected_module_field(module_names, inner)
  | Dot(
      {term: Var(module_name) | Constructor(module_name, None), _},
      {term: Label(field), _},
    )
      when List.mem(module_name, module_names) =>
    Some(field)
  | _ => None
  };

let module_items_query =
    (items: list(Mod.t), field: string, query: Typ.t): Typ.t => {
  let fields =
    items
    |> List.concat_map(item =>
         module_binding_names(item)
         |> List.map(name =>
              name == field
                ? TupLabel(Label(name) |> Typ.temp, query) |> Typ.temp : gap
            )
       );
  switch (fields) {
  | [] => gap
  | _ => Prod(fields) |> Typ.temp
  };
};

let module_exp_demand = (term: Exp.term, query: Typ.t, fallback: Typ.t): Typ.t =>
  switch (term) {
  | ModuleExp(mp, {term: Module(items), _}, body) =>
    switch (projected_module_field(ExpandModule.mpat_names(mp), body)) {
    | Some(field) => module_items_query(items, field, query)
    | None => fallback
    }
  | _ => fallback
  };

let module_exp_item_adjustment =
    (term: Exp.term, query: Typ.t): (Id.Set.t, Id.Set.t) =>
  switch (term) {
  | ModuleExp(_, {term: Module(items), _}, _) =>
    module_item_adjustment(items, query)
  | _ => (Id.Set.empty, Id.Set.empty)
  };

let apply_omission_adjustment =
    (result: result, (add, remove): (Id.Set.t, Id.Set.t)): result => {
  ...result,
  omitted: Id.Set.union(Id.Set.diff(result.omitted, remove), add),
};

/* The body's demand on the bound variables, shaped like the binding pattern:
   each variable leaf carries its required type (from the body's gamma), and
   composite patterns rebuild the matching product/list structure. The result
   is the reverse-direction query against which the definition and pattern are
   sliced. */
let rec pattern_demand = (pat: Pat.t, gamma: gamma): Typ.t =>
  switch (Pat.term_of(pat)) {
  | Parens(p)
  | Asc(p, _) => pattern_demand(p, gamma)
  | Var(x) =>
    switch (VarMap.lookup(gamma, x)) {
    | Some(ty) => ty
    | None => gap
    }
  | Tuple(ps) =>
    Prod(List.map(p => pattern_demand(p, gamma), ps)) |> Typ.temp
  | TupLabel(_, p) => pattern_demand(p, gamma)
  | Cons(hd, _) => List(pattern_demand(hd, gamma)) |> Typ.temp
  | ListLit(ps) =>
    List(
      List.fold_left(
        (acc, p) => meet_empty(acc, pattern_demand(p, gamma)),
        gap,
        ps,
      ),
    )
    |> Typ.temp
  | Ap(f, payload) =>
    switch (Pat.term_of(f)) {
    /* Function-definition pattern `f(x)`: the bound name is the function `f`;
       the parameter is demanded by the definition, not the body. */
    | Var(name) =>
      switch (VarMap.lookup(gamma, name)) {
      | Some(ty) => ty
      | None => gap
      }
    | _ => pattern_demand(payload, gamma)
    }
  | _ => gap
  };

/* Omit a pattern's variable binders, retaining type annotations. */
let rec pat_omit_keeping_ann = (pat: Pat.t): Id.Set.t =>
  switch (Pat.term_of(pat)) {
  | Parens(p)
  | Asc(p, _) => pat_omit_keeping_ann(p)
  | _ => ids_set(IdTagged.ids(pat))
  };

let rec demand_is_gap = (ty: Typ.t): bool =>
  is_gap(ty)
  || (
    switch (Typ.term_of(ty)) {
    | Prod(ts)
    | TypTuple(ts) => List.for_all(demand_is_gap, ts)
    | List(inner)
    | TupLabel(_, inner) => demand_is_gap(inner)
    | _ => false
    }
  );

/* Per-leaf pattern omission: a variable leaf whose shaped demand is a gap is
   replaced by a pattern hole; composite patterns recurse positionally. A fully
   gapped demand is handled by the source's self-gap (whole pattern omitted), so
   here we only gap the unused leaves of a partially demanded pattern. */
let rec pattern_omissions = (pat: Pat.t, demand: Typ.t): Id.Set.t =>
  if (is_gap(demand)) {
    ids_set(pat_all_ids(pat));
  } else {
    switch (Pat.term_of(pat), Typ.term_of(demand)) {
    | (Parens(p), _) => pattern_omissions(p, demand)
    | (Asc(p, _), _) => pattern_omissions(p, demand)
    | (Tuple(ps), Prod(ds)) when List.length(ps) == List.length(ds) =>
      List.map2(pattern_omissions, ps, ds)
      |> List.fold_left(Id.Set.union, Id.Set.empty)
    | (TupLabel(_, p), TupLabel(_, d)) => pattern_omissions(p, d)
    | (TupLabel(_, p), _) => pattern_omissions(p, demand)
    | (Cons(hd, tl), List(d)) =>
      Id.Set.union(pattern_omissions(hd, d), pattern_omissions(tl, demand))
    | (ListLit(ps), List(d)) =>
      List.map(p => pattern_omissions(p, d), ps)
      |> List.fold_left(Id.Set.union, Id.Set.empty)
    | (Ap(f, payload), _) =>
      switch (Pat.term_of(f)) {
      | Var(_) => pat_omit_keeping_ann(payload)
      | _ => pattern_omissions(payload, demand)
      }
    | (Atom(_), _) => ids_set(pat_all_ids(pat))
    | _ => Id.Set.empty
    };
  };

let binding_demand = (term: Exp.term, gamma: gamma): Typ.t =>
  switch (binding_pat(term)) {
  | Some(p) =>
    let demand = pattern_demand(p, gamma);
    demand_is_gap(demand) ? gap : demand;
  | None => demand_of(binding_names(term), gamma)
  };

let pattern_source = (~id: Id.t, pat: Pat.t, shape: Typ.t): sty =>
  with_self_gap(
    ~id,
    {
      shape,
      dispatch: query => {
        let leaf_omitted = pattern_omissions(pat, query);
        let ann_omitted =
          switch (pat_annotation(pat)) {
          | Some(actual) => typ_omissions(actual, query)
          | None => Id.Set.empty
          };
        {
          ...queried_result(query),
          omitted: Id.Set.union(leaf_omitted, ann_omitted),
        };
      },
      finalize: () => empty_result,
    },
  );

let binding_source = (m: Id.Map.t(Info.t), info: Info.exp): list(child) =>
  switch (binding_pat(Exp.term_of(info.user_term))) {
  | Some(p) =>
    let pid = Pat.rep_id(p);
    let shape =
      switch (Id.Map.find_opt(pid, m)) {
      | Some(Info.InfoPat({ty, _})) => ty
      | _ =>
        switch (Typ.term_of(info.ty)) {
        | Arrow(dom, _) => dom
        | _ => unknown
        }
      };
    [
      {
        mode: Source,
        node:
          node_of(
            ~id=pid,
            ~ids=ids_set(pat_all_ids(p)),
            pattern_source(~id=pid, p, shape),
          ),
      },
    ];
  | None => []
  };

let used_constructors = (ctx: Ctx.t): list(string) =>
  List.filter_map(
    fun
    | Ctx.ConstructorEntry({name, _}) => Some(name)
    | _ => None,
    ctx.entries,
  );

/* Ids of the sum variants of an alias body whose constructors are not used by
   the slice (so they reconstruct as holes: `None + Some(A)` -> `? + Some(A)`). */
let rec unused_variant_ids = (used: list(string), ty: Typ.t): Id.Set.t =>
  switch (Typ.term_of(ty)) {
  | Parens(t)
  | Rec(_, t)
  | TypFun(_, t)
  | Poly(_, t) => unused_variant_ids(used, t)
  | Sum(variants) =>
    List.fold_left(
      (acc, variant) =>
        switch (variant) {
        | ConstructorMap.Variant(name, ann, _) when !List.mem(name, used) =>
          Id.Set.union(acc, ids_set(ann.ids))
        | _ => acc
        },
      Id.Set.empty,
      variants,
    )
  | _ => Id.Set.empty
  };

let rec typ_free_vars = (ty: Typ.t): list(string) =>
  switch (Typ.term_of(ty)) {
  | Var(name) => [name]
  | Parens(t)
  | List(t)
  | TupLabel(_, t)
  | TypFun(_, t)
  | Poly(_, t)
  | Rec(_, t) => typ_free_vars(t)
  | Arrow(a, b)
  | TypParamAp(a, b) => typ_free_vars(a) @ typ_free_vars(b)
  | Prod(ts)
  | TypTuple(ts) => List.concat_map(typ_free_vars, ts)
  | Sum(variants) =>
    List.concat_map(
      fun
      | ConstructorMap.Variant(_, _, Some(p)) => typ_free_vars(p)
      | _ => [],
      variants,
    )
  | _ => []
  };

let rec typ_var_ids = (name: string, ty: Typ.t): Id.Set.t =>
  switch (Typ.term_of(ty)) {
  | Var(n) when n == name => Id.Set.singleton(Typ.rep_id(ty))
  | TypFun(binder, _) when TPat.tyvar_of_utpat(binder) == Some(name) => Id.Set.empty
  | Parens(t)
  | List(t)
  | TupLabel(_, t)
  | TypFun(_, t)
  | Poly(_, t)
  | Rec(_, t) => typ_var_ids(name, t)
  | Arrow(a, b)
  | TypParamAp(a, b) =>
    Id.Set.union(typ_var_ids(name, a), typ_var_ids(name, b))
  | Prod(ts)
  | TypTuple(ts) =>
    List.fold_left(
      (acc, t) => Id.Set.union(acc, typ_var_ids(name, t)),
      Id.Set.empty,
      ts,
    )
  | Sum(variants) =>
    List.fold_left(
      (acc, variant) =>
        switch (variant) {
        | ConstructorMap.Variant(_, _, Some(p))
        | ConstructorMap.BadEntry(p) =>
          Id.Set.union(acc, typ_var_ids(name, p))
        | ConstructorMap.Variant(_, _, None) => acc
        },
      Id.Set.empty,
      variants,
    )
  | _ => Id.Set.empty
  };

/* Type variables referenced by the still-used (kept) sum variants. */
let rec kept_variant_tyvars = (used: list(string), ty: Typ.t): list(string) =>
  switch (Typ.term_of(ty)) {
  | Parens(t)
  | Rec(_, t)
  | TypFun(_, t)
  | Poly(_, t) => kept_variant_tyvars(used, t)
  | Sum(variants) =>
    List.concat_map(
      fun
      | ConstructorMap.Variant(name, _, Some(p)) when List.mem(name, used) =>
        typ_free_vars(p)
      | _ => [],
      variants,
    )
  | _ => []
  };

/* `typfun` binders that no kept variant references are omitted (`typfun ? -> …`,
   holeable now that reconstruct handles TPat). */
let rec unused_binder_ids = (used: list(string), ty: Typ.t): Id.Set.t =>
  switch (Typ.term_of(ty)) {
  | Parens(t)
  | Rec(_, t)
  | Poly(_, t) => unused_binder_ids(used, t)
  | TypFun(binder, body) =>
    let rest = unused_binder_ids(used, body);
    switch (TPat.tyvar_of_utpat(binder)) {
    | Some(name) when !List.mem(name, kept_variant_tyvars(used, ty)) =>
      rest
      |> Id.Set.union(_, ids_set(IdTagged.ids(binder)))
      |> Id.Set.add(TPat.rep_id(binder), _)
      |> Id.Set.union(_, typ_var_ids(name, body))
    | _ => rest
    };
  | _ => Id.Set.empty
  };

let rec unused_binder_names = (used: list(string), ty: Typ.t): list(string) =>
  switch (Typ.term_of(ty)) {
  | Parens(t)
  | Rec(_, t)
  | Poly(_, t) => unused_binder_names(used, t)
  | TypFun(binder, body) =>
    let rest = unused_binder_names(used, body);
    switch (TPat.tyvar_of_utpat(binder)) {
    | Some(name) when !List.mem(name, kept_variant_tyvars(used, ty)) => [
        name,
        ...rest,
      ]
    | _ => rest
    };
  | _ => []
  };

let unused_binder_occurrence_ids =
    (used: list(string), demand: Typ.t, original: Typ.t): Id.Set.t =>
  unused_binder_names(used, demand)
  |> List.fold_left(
       (acc, name) => Id.Set.union(acc, typ_var_ids(name, original)),
       Id.Set.empty,
     );

let rec omitted_binder_occurrence_ids =
        (omitted: Id.Set.t, ty: Typ.t): Id.Set.t =>
  switch (Typ.term_of(ty)) {
  | Parens(t)
  | Rec(_, t)
  | Poly(_, t) => omitted_binder_occurrence_ids(omitted, t)
  | TypFun(binder, body) =>
    let rest = omitted_binder_occurrence_ids(omitted, body);
    switch (TPat.tyvar_of_utpat(binder)) {
    | Some(name)
        when
          Id.Set.mem(TPat.rep_id(binder), omitted)
          || List.exists(
               id => Id.Set.mem(id, omitted),
               IdTagged.ids(binder),
             ) =>
      Id.Set.union(rest, typ_var_ids(name, body))
    | _ => rest
    };
  | _ => Id.Set.empty
  };

let alias_used_in_term =
    (m: Id.Map.t(Info.t), omitted: Id.Set.t, names: list(string)): bool =>
  Id.Map.exists(
    (id, info) => {
      let (is_ctor, ancestors) =
        switch (info) {
        | Info.InfoExp({user_term, ancestors, _}) => (
            switch (Exp.term_of(user_term)) {
            | Constructor(n, _) => List.mem(n, names)
            | _ => false
            },
            ancestors,
          )
        | Info.InfoPat({user_term, ancestors, _}) => (
            switch (Pat.term_of(user_term)) {
            | Constructor(n, _) => List.mem(n, names)
            | _ => false
            },
            ancestors,
          )
        | _ => (false, [])
        };
      is_ctor
      && !List.exists(a => Id.Set.mem(a, omitted), [id, ...ancestors]);
    },
    m,
  );

let ty_alias = (~shape: Typ.t, ~term: Exp.term, body: sty): sty => {
  shape,
  dispatch: query => {
    let slice = body.dispatch(query);
    switch (term) {
    | TyAlias(typat, utyp, _) =>
      let used = used_constructors(slice.context);
      let binder_demand =
        switch (TPat.tyvar_of_utpat(typat)) {
        | Some(alias) =>
          switch (Ctx.lookup_alias(slice.context, alias)) {
          | Some(ty) => ty
          | None => utyp
          }
        | None => utyp
        };
      let omit =
        Id.Set.union(
          unused_variant_ids(used, utyp),
          Id.Set.union(
            Id.Set.union(
              unused_binder_ids(used, binder_demand),
              unused_binder_occurrence_ids(used, binder_demand, utyp),
            ),
            omitted_binder_occurrence_ids(slice.omitted, utyp),
          ),
        );
      {
        ...slice,
        omitted: Id.Set.union(slice.omitted, omit),
      };
    | _ => slice
    };
  },
  finalize: () => empty_result,
};

let alias_shadowed =
    (m: Id.Map.t(Info.t), alias_id: Id.t, names: list(string)): bool =>
  names != []
  && List.for_all(
       name =>
         Id.Map.exists(
           (_, info) =>
             switch (info) {
             | Info.InfoExp({user_term, ancestors, _}) =>
               switch (Exp.term_of(user_term)) {
               | TyAlias(_, utyp2, _) =>
                 List.mem(name, alias_constructors(utyp2))
                 && List.mem(alias_id, ancestors)
               | _ => false
               }
             | _ => false
             },
           m,
         ),
       names,
     );

let alias_name_referenced =
    (m: Id.Map.t(Info.t), omitted: Id.Set.t, name: string): bool =>
  Id.Map.exists(
    (id, info) =>
      switch (info) {
      | Info.InfoTyp({user_term, ancestors, _}) =>
        switch (Typ.term_of(user_term)) {
        | Var(n) =>
          n == name
          && !List.exists(a => Id.Set.mem(a, omitted), [id, ...ancestors])
        | _ => false
        }
      | _ => false
      },
    m,
  );

let omit_unused_aliases = (m: Id.Map.t(Info.t), omitted: Id.Set.t): Id.Set.t =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (info) {
      | Info.InfoExp({user_term, _}) =>
        switch (Exp.term_of(user_term)) {
        | TyAlias(typat, utyp, _) =>
          let names = alias_constructors(utyp);
          let alias_id = Exp.rep_id(user_term);
          let ctor_alive =
            names != []
            && alias_used_in_term(m, acc, names)
            && !alias_shadowed(m, alias_id, names);
          let name_referenced =
            switch (TPat.tyvar_of_utpat(typat)) {
            | Some(n) => alias_name_referenced(m, acc, n)
            | None => false
            };
          !ctor_alive && !name_referenced
            ? Id.Set.union(
                acc,
                ids_set(IdTagged.ids(typat) @ IdTagged.ids(utyp)),
              )
            : acc;
        | _ => acc
        }
      | _ => acc
      },
    m,
    omitted,
  );

let close_omitted_alias_binders =
    (m: Id.Map.t(Info.t), omitted: Id.Set.t): Id.Set.t =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (info) {
      | Info.InfoExp({user_term, _}) =>
        switch (Exp.term_of(user_term)) {
        | TyAlias(_, utyp, _) =>
          Id.Set.union(acc, omitted_binder_occurrence_ids(acc, utyp))
        | _ => acc
        }
      | _ => acc
      },
    m,
    omitted,
  );

let match_branch_slice =
    ((pat, branch, branch_query): (Pat.t, sty, Typ.t))
    : (result, Typ.t, Id.Set.t) => {
  let slice = branch.dispatch(branch_query);
  if (is_gap(branch_query)) {
    (slice, gap, ids_set(IdTagged.ids(pat)));
  } else {
    let demand = {
      let d = pattern_demand(pat, slice.gamma);
      demand_is_gap(d) ? gap : d;
    };
    let discharged = {
      ...slice,
      gamma: gamma_discharge(slice.gamma, Pat.bound_vars(pat)),
    };
    (discharged, demand, pattern_omissions(pat, demand));
  };
};

let match_ = (~shape: Typ.t, ~term: Exp.term, children: list(sty)): sty => {
  shape,
  dispatch: query => {
    switch (term) {
    | Match(_, [_, ..._] as rules)
        when List.length(children) == List.length(rules) + 1 =>
      let (scrut, branches) = (List.hd(children), List.tl(children));
      let pats = List.map(fst, rules);
      let sliced =
        List.map2(
          (pat, (branch, branch_query)) =>
            match_branch_slice((pat, branch, branch_query)),
          pats,
          List.combine(branches, branch_queries(branches, query)),
        );
      let branch_slices = List.map(((slice, _, _)) => slice, sliced);
      let scrut_demand =
        sliced
        |> List.map(((_, demand, _)) => demand)
        |> List.filter(d => !is_gap(d))
        |> List.fold_left(meet_empty, gap);
      let scrut_slice = scrut.dispatch(scrut_demand);
      let pattern_ids =
        sliced
        |> List.map(((_, _, ids)) => ids)
        |> List.fold_left(Id.Set.union, Id.Set.empty);
      let combined = results_join(branch_slices);
      {
        omitted:
          Id.Set.union(
            Id.Set.union(combined.omitted, scrut_slice.omitted),
            pattern_ids,
          ),
        gamma: gamma_join(combined.gamma, scrut_slice.gamma),
        context: context_join(combined.context, scrut_slice.context),
        psi: combined.psi,
        ana: combined.ana,
      };
    | _ =>
      let (scrut, branch_slice) =
        switch (children) {
        | [scrut, branch] => (Some(scrut), branch.dispatch(query))
        | [branch] => (None, branch.dispatch(query))
        | _ => (None, empty_result)
        };
      switch (term) {
      | Match(_, [(pat, _), ...rest]) =>
        let names = Pat.bound_vars(pat);
        let demand = {
          let d = pattern_demand(pat, branch_slice.gamma);
          demand_is_gap(d) ? gap : d;
        };
        let scrut_slice =
          switch (scrut) {
          | Some(s) => s.dispatch(demand)
          | None => empty_result
          };
        let other_pattern_ids =
          rest
          |> List.map(((p, _)) => ids_set(IdTagged.ids(p)))
          |> List.fold_left(Id.Set.union, Id.Set.empty);
        let combined = results_join([scrut_slice, branch_slice]);
        {
          omitted:
            Id.Set.union(
              Id.Set.union(combined.omitted, pattern_omissions(pat, demand)),
              other_pattern_ids,
            ),
          gamma: gamma_discharge(combined.gamma, names),
          context: combined.context,
          psi: branch_slice.psi,
          ana: branch_slice.ana,
        };
      | _ => branch_slice
      };
    };
  },
  finalize: () => empty_result,
};

let source_ids = (e: Exp.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let collect = ids =>
    acc := List.fold_left((s, id) => Id.Set.add(id, s), acc^, ids);
  let rec collect_mpat_roots = (mp: MPat.t): unit => {
    collect(IdTagged.ids(mp));
    switch (mp.term) {
    | Asc(inner, _) => collect_mpat_roots(inner)
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Var(_) => ()
    };
  };
  let collect_mod_roots = (m: Mod.t): unit => {
    collect(IdTagged.ids(m));
    switch (m.term) {
    | ModuleMod(mp, _) => collect_mpat_roots(mp)
    | EmptyHole
    | Invalid(_)
    | ModExp(_)
    | ModLet(_, _)
    | ModType(_, _)
    | MultiHole(_) => ()
    };
  };
  let collect_sig_roots = (s: Sig.t): unit => collect(IdTagged.ids(s));
  let collect_term:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, term) => {
      collect(IdTagged.ids(term));
      continue(term);
    };
  let collect_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t =
    (continue, tp) => {
      collect(IdTagged.ids(tp));
      collect([TPat.rep_id(tp)]);
      continue(tp);
    };
  let collect_exp: (Exp.t => Exp.t, Exp.t) => Exp.t =
    (continue, exp) => {
      collect(IdTagged.ids(exp));
      switch (Exp.term_of(exp)) {
      | Module(items) => List.iter(collect_mod_roots, items)
      | ModuleExp(mp, _, _) => collect_mpat_roots(mp)
      | _ => ()
      };
      continue(exp);
    };
  let collect_typ: (Typ.t => Typ.t, Typ.t) => Typ.t =
    (continue, typ) => {
      collect(IdTagged.ids(typ));
      collect([Typ.rep_id(typ)]);
      switch (Typ.term_of(typ)) {
      | Sig(items) => List.iter(collect_sig_roots, items)
      | _ => ()
      };
      continue(typ);
    };
  let collect_any: (Any.t => Any.t, Any.t) => Any.t =
    (continue, any) => {
      switch (any) {
      | Mod(m) => collect_mod_roots(m)
      | Sig(s) => collect_sig_roots(s)
      | MPat(mp) => collect_mpat_roots(mp)
      | Exp(_)
      | Pat(_)
      | Typ(_)
      | TPat(_)
      | Rul(_)
      | Drv(_)
      | Any () => ()
      };
      continue(any);
    };
  ignore(
    Exp.map_term(
      ~f_exp=collect_exp,
      ~f_pat=collect_term,
      ~f_typ=collect_typ,
      ~f_tpat=collect_tpat,
      ~f_rul=collect_term,
      ~f_any=collect_any,
      e,
    ),
  );
  acc^;
};

let rec unused_binder_name_ids = (used: string => bool, p: Pat.t): Id.Set.t =>
  switch (Pat.term_of(p)) {
  | Parens(inner)
  | Asc(inner, _) => unused_binder_name_ids(used, inner)
  | Var(name) => used(name) ? Id.Set.empty : ids_set(IdTagged.ids(p))
  | Tuple(ps)
  | ListLit(ps) =>
    List.fold_left(
      (acc, p) => Id.Set.union(acc, unused_binder_name_ids(used, p)),
      Id.Set.empty,
      ps,
    )
  | TupLabel(_, p) => unused_binder_name_ids(used, p)
  | Cons(hd, tl) =>
    Id.Set.union(
      unused_binder_name_ids(used, hd),
      unused_binder_name_ids(used, tl),
    )
  | Ap(_, payload) => unused_binder_name_ids(used, payload)
  | _ => Id.Set.empty
  };

let unreferenced_param_ids = (term: Exp.term, gamma: gamma): Id.Set.t =>
  switch (binding_pat(term)) {
  | Some(p) =>
    unused_binder_name_ids(
      name =>
        switch (VarMap.lookup(gamma, name)) {
        | Some(ty) => !is_gap(ty)
        | None => false
        },
      p,
    )
  | None => Id.Set.empty
  };

let rec node_of_exp_info =
        (~seen=Id.Set.empty, m: Id.Map.t(Info.t), info: Info.exp): node => {
  let id = Exp.rep_id(info.user_term);
  let ids = source_ids(info.user_term);
  let seen = Id.Set.add(id, seen);
  let children =
    info.slice_children
    |> List.filter_map((child_edge: Info.slice_child) =>
         if (Id.Set.mem(child_edge.child, seen)) {
           None;
         } else {
           switch (Id.Map.find_opt(child_edge.child, m)) {
           | Some(Info.InfoExp(child_info)) =>
             let node = node_of_exp_info(~seen, m, child_info);
             let node =
               is_gap(node.ty.shape) && !is_gap(child_info.elab_syn_ty)
                 ? {
                   ...node,
                   ty: {
                     ...node.ty,
                     shape: child_info.elab_syn_ty,
                   },
                 }
                 : node;
             Some({
               mode: of_info_mode(child_edge.mode),
               node,
             });
           | _ => None
           };
         }
       );
  let ty =
    switch (Exp.term_of(info.user_term)) {
    | Var(name) =>
      let typ =
        switch (Ctx.lookup_var(info.ctx, name)) {
        | Some({typ, _}) => typ
        | None => unknown
        };
      var(~id, ~entry=assume(info.ctx, name, typ));
    | Constructor(name, _) =>
      let typ =
        switch (Ctx.lookup_ctr(info.ctx, name)) {
        | Some({typ, _}) => typ
        | None => info.ty
        };
      var(~id, ~entry=constructor(info.ctx, name, typ));
    | term =>
      let children = binding_source(m, info) @ children;
      let kept = child_tys(Keep, children);
      let sources = child_tys(Source, children);
      let omitted =
        Id.Set.diff(
          omitted_ids(children),
          source_ids_of_children(children),
        );
      let base =
        switch (Typ.term_of(info.ty), sources, kept) {
        | (_, [domain], [codomain])
            when
              switch (term) {
              | Fun(_, _, _, _) => true
              | _ => false
              } => {
            shape: Arrow(domain.shape, codomain.shape) |> Typ.temp,
            dispatch: query => {
              let (q_domain, q_codomain) =
                switch (Typ.term_of(query)) {
                | Arrow(q_domain, q_codomain) => (q_domain, q_codomain)
                | _ => (gap, gap)
                };
              let codomain_slice = codomain.dispatch(q_codomain);
              let demand =
                meet_demands(
                  q_domain,
                  binding_demand(term, codomain_slice.gamma),
                );
              let domain_slice = {
                let slice = domain.dispatch(demand);
                {
                  ...slice,
                  omitted:
                    Id.Set.union(
                      slice.omitted,
                      unreferenced_param_ids(term, codomain_slice.gamma),
                    ),
                };
              };
              let psi =
                is_gap(query)
                  ? gap
                  : Arrow(domain_slice.psi, codomain_slice.psi) |> Typ.temp;
              with_deps(
                {
                  ...codomain_slice,
                  gamma:
                    gamma_discharge(
                      codomain_slice.gamma,
                      binding_names(term),
                    ),
                  psi,
                },
                domain_slice,
              );
            },
            finalize: () => empty_result,
          }
        | (_, [_, ..._], []) => {
            shape: info.ty,
            dispatch: query => {
              let deps =
                sources |> List.map(s => s.dispatch(gap)) |> results_join;
              {
                ...queried_result(query),
                omitted: deps.omitted,
                gamma: deps.gamma,
                context: deps.context,
              };
            },
            finalize: () => empty_result,
          }
        | (_, [_, ..._], [only]) => {
            shape: info.ty,
            dispatch: query => {
              let kept_slice = only.dispatch(query);
              let demand =
                module_exp_demand(
                  term,
                  query,
                  binding_demand(term, kept_slice.gamma),
                );
              let deps =
                sources |> List.map(s => s.dispatch(demand)) |> results_join;
              with_deps(
                {
                  ...kept_slice,
                  gamma:
                    gamma_discharge(kept_slice.gamma, binding_names(term)),
                },
                deps,
              )
              |> apply_omission_adjustment(
                   _,
                   module_exp_item_adjustment(term, demand),
                 );
            },
            finalize: () => empty_result,
          }
        | (_, [], [_, ..._] as kept)
            when
              switch (term) {
              | DeferredAp(_, _) => true
              | _ => false
              } =>
          deferred_ap(~shape=info.ty, ~term, kept)
        | (_, [], [_, ..._] as kept)
            when
              switch (term) {
              | If(_, _, _) => true
              | _ => false
              } =>
          if_(~shape=info.ty, kept)
        | (_, [], [_, ..._] as kept)
            when
              switch (term) {
              | Match(_, _) => true
              | _ => false
              } =>
          match_(~shape=info.ty, ~term, kept)
        | (_, [], [_] as kept)
        | (_, [], [_, ..._] as kept)
            when
              switch (term) {
              | ListLit(_) => true
              | _ => false
              } =>
          list_lit(~shape=info.ty, kept)
        | (_, [], [only])
            when
              switch (term) {
              | Cons(_, _) => true
              | _ => false
              } =>
          cons(~shape=info.ty, only)
        | (_, [], [only])
            when
              switch (term) {
              | ListConcat(_, _) => true
              | _ => false
              } =>
          list_concat(~shape=info.ty, only)
        | (_, [], [only])
            when
              switch (term) {
              | Ap(_, _, _) => true
              | _ => false
              } =>
          ap(~shape=info.ty, only)
        | (_, [], [only])
            when
              switch (term) {
              | TypAp(_, _) => true
              | _ => false
              } =>
          typ_ap(~ctx=info.ctx, ~shape=info.ty, ~term, only)
        | (_, [], [_] as kept)
            when
              switch (term) {
              | Tuple(_) => true
              | _ => false
              } =>
          prod(kept)
        | (_, [], [only])
            when
              switch (term) {
              | TupLabel(_, _) => true
              | _ => false
              } =>
          tup_label(~shape=info.ty, only)
        | (_, [], [only])
            when
              switch (term) {
              | Dot(_, _) => true
              | _ => false
              } =>
          dot(~shape=info.ty, ~term, only)
        | (_, [], [only])
            when
              switch (term) {
              | Asc(_, _) => true
              | _ => false
              } =>
          asc(~shape=info.ty, ~term, only)
        | (_, [], [only])
            when
              switch (term) {
              | TyAlias(_, _, _) => true
              | _ => false
              } =>
          ty_alias(~shape=info.ty, ~term, only)
        | (_, [], [only])
            when
              switch (term) {
              | TypAbs(_, _, _) => true
              | _ => false
              } =>
          typ_abs(~shape=info.ty, only)
        | (_, [], [only])
            when
              switch (term) {
              | Module(_) => true
              | _ => false
              } =>
          module_(~shape=info.ty, ~term, only)
        | (_, [], [only]) => only
        | (_, [], [_, ..._] as kept) => prod(kept)
        | _ => source(~id, info.ty)
        };
      with_self_gap_ids(~ids, with_omitted(omitted, base));
    };
  node_of(~id, ~ids, ty);
};

let node_of_exp_result = ((info, _, m): exp_result): node =>
  node_of_exp_info(m, info);

let dispatch = (root: exp_result, query: Typ.t): result =>
  node_of_exp_result(root).ty.dispatch(query);

let dispatch_focus =
    (m: Id.Map.t(Info.t), id: Id.t, query: Typ.t): option(result) =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(Info.InfoExp(info)) =>
    Some(node_of_exp_info(m, info).ty.dispatch(query))
  | _ => None
  };

let focus_subtree_ids = (m: Id.Map.t(Info.t), id: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(Info.InfoExp({user_term, _})) => source_ids(user_term)
  | _ => Id.Set.singleton(id)
  };

let info_ancestors =
    (info: Info.t)
    : option(
        (
          list(Id.t),
          [
            | `Exp
            | `Pat
            | `Typ
            | `Other
          ],
        ),
      ) =>
  switch (info) {
  | Info.InfoExp({ancestors, _}) => Some((ancestors, `Exp))
  | Info.InfoPat({ancestors, _}) => Some((ancestors, `Pat))
  | Info.InfoTyp({ancestors, _}) => Some((ancestors, `Typ))
  | Info.InfoTPat({ancestors, _})
  | Info.InfoMod({ancestors, _})
  | Info.InfoSig({ancestors, _})
  | Info.InfoMPat({ancestors, _}) => Some((ancestors, `Other))
  | _ => None
  };

let focus_path = (m: Id.Map.t(Info.t), focus: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
  | Some((ancestors, _)) => ids_set([focus, ...ancestors])
  | None => Id.Set.singleton(focus)
  };

let term_token_ids = (m: Id.Map.t(Info.t), id: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(Info.InfoExp({user_term, _})) => ids_set(IdTagged.ids(user_term))
  | Some(Info.InfoPat({user_term, _})) => ids_set(IdTagged.ids(user_term))
  | Some(Info.InfoTyp({user_term, _})) => ids_set(IdTagged.ids(user_term))
  | _ => Id.Set.singleton(id)
  };

let path_token_ids = (m: Id.Map.t(Info.t), path: Id.Set.t): Id.Set.t =>
  Id.Set.fold(
    (id, acc) => Id.Set.union(acc, term_token_ids(m, id)),
    path,
    path,
  );

let on_path = (path: Id.Set.t, id: Id.t): bool => Id.Set.mem(id, path);

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
  switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
  | Some((ancestors, _)) => go(ancestors)
  | None => Id.Set.empty
  };
};

let exp_child_ids = (exp: Exp.t): list(Id.t) =>
  switch (Exp.term_of(exp)) {
  | DynamicErrorHole(e, _)
  | ProofObject(e)
  | Forall(_, e)
  | FixF(_, e, _)
  | TyAlias(_, _, e)
  | Use(_, e)
  | TypAp(e, _)
  | Test(e)
  | Closure(_, e)
  | Parens(e)
  | Projector(_, e)
  | UnOp(_, e)
  | Asc(e, _) => [Exp.rep_id(e)]
  | ListLit(es)
  | Tuple(es) => List.map(Exp.rep_id, es)
  | Fun(p, e, _, _) => [Pat.rep_id(p), Exp.rep_id(e)]
  | TypAbs(_, e, _) => [Exp.rep_id(e)]
  | TupLabel(label, e)
  | TupleExtension(label, e)
  | Dot(label, e)
  | Ap(_, label, e)
  | Seq(label, e)
  | Cons(label, e)
  | ListConcat(label, e)
  | BinOp(_, label, e) => [Exp.rep_id(label), Exp.rep_id(e)]
  | Let(p, e1, e2)
  | Theorem(p, e1, e2) => [Pat.rep_id(p), Exp.rep_id(e1), Exp.rep_id(e2)]
  | DeferredAp(e, es) => [Exp.rep_id(e), ...List.map(Exp.rep_id, es)]
  | If(e1, e2, e3) => [Exp.rep_id(e1), Exp.rep_id(e2), Exp.rep_id(e3)]
  | HintedTest(e, h) => [Exp.rep_id(e), Exp.rep_id(h)]
  | Filter(Filter({pat, _}), e) => [Exp.rep_id(pat), Exp.rep_id(e)]
  | Filter(_, e) => [Exp.rep_id(e)]
  | Match(e, rules) => [
      Exp.rep_id(e),
      ...List.flatten(
           List.map(((p, e)) => [Pat.rep_id(p), Exp.rep_id(e)], rules),
         ),
    ]
  | EmptyHole
  | Invalid(_)
  | Atom(_)
  | DrvQuote(_)
  | Constructor(_)
  | Label(_)
  | ExplicitNonlabel
  | Deferral(_)
  | Var(_)
  | LivelitName(_)
  | Undefined
  | MultiHole(_)
  | BuiltinFun(_)
  | Module(_)
  | ModuleExp(_, _, _) => []
  };

let pat_child_ids = (pat: Pat.t): list(Id.t) =>
  switch (Pat.term_of(pat)) {
  | ListLit(ps)
  | Tuple(ps) => List.map(Pat.rep_id, ps)
  | Ap(p1, p2)
  | Cons(p1, p2)
  | TupLabel(p1, p2) => [Pat.rep_id(p1), Pat.rep_id(p2)]
  | Parens(p)
  | Projector(_, p)
  | Asc(p, _) => [Pat.rep_id(p)]
  | EmptyHole
  | Invalid(_)
  | Wild
  | Atom(_)
  | Constructor(_)
  | Label(_)
  | Var(_)
  | ExplicitNonlabel
  | MultiHole(_) => []
  };

/* `keep_head` is set when omitting structural siblings within the focused
   pattern's own application (the constructor `A` of `A(x)`, label `a` of
   `(a=x)`), where the head must be retained; it is unset for wholesale-omitted
   patterns (a match-branch pattern, a let-pattern sibling). Annotations are
   retained either way. */
let rec pat_side_omit = (~keep_head: bool, pat: Pat.t): Id.Set.t =>
  switch (Pat.term_of(pat)) {
  | Parens(p)
  | Asc(p, _) => pat_side_omit(~keep_head, p)
  | Constructor(_, _)
  | Label(_)
  | ExplicitNonlabel when keep_head => Id.Set.empty
  | _ => ids_set(IdTagged.ids(pat))
  };

let child_omit =
    (~keep_head: bool, m: Id.Map.t(Info.t), child_id: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(child_id, m)) {
  | Some(Info.InfoPat({user_term, _})) =>
    pat_side_omit(~keep_head, user_term)
  | Some(Info.InfoExp({user_term, _})) when keep_head =>
    switch (Exp.term_of(user_term)) {
    | Constructor(_, _)
    | Label(_)
    | ExplicitNonlabel => Id.Set.empty
    | _ => Id.Set.singleton(child_id)
    }
  | _ => Id.Set.singleton(child_id)
  };

let child_side_ids =
    (path: Id.Set.t, m: Id.Map.t(Info.t), id: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(Info.InfoExp({user_term, _})) =>
    let keep_head =
      switch (Exp.term_of(user_term)) {
      | Ap(_, _, _) => true
      | _ => false
      };
    exp_child_ids(user_term)
    |> List.filter(child_id => !on_path(path, child_id))
    |> List.map(child_omit(~keep_head, m))
    |> List.fold_left(Id.Set.union, Id.Set.empty);
  | Some(Info.InfoPat({user_term, _})) =>
    pat_child_ids(user_term)
    |> List.filter(child_id => !on_path(path, child_id))
    |> List.map(child_omit(~keep_head=true, m))
    |> List.fold_left(Id.Set.union, Id.Set.empty)
  | _ => Id.Set.empty
  };

let side_ids_for_revealed_path =
    (path: Id.Set.t, m: Id.Map.t(Info.t)): Id.Set.t =>
  Id.Set.fold(
    (id, acc) => Id.Set.union(acc, child_side_ids(path, m, id)),
    path,
    Id.Set.empty,
  );

let child_head_reveal = (m: Id.Map.t(Info.t), child_id: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(child_id, m)) {
  | Some(Info.InfoPat({user_term, _})) =>
    let rec go = (pat: Pat.t): Id.Set.t =>
      switch (Pat.term_of(pat)) {
      | Parens(p) => Id.Set.union(ids_set(IdTagged.ids(pat)), go(p))
      | Asc(p, ty) =>
        ids_set(IdTagged.ids(pat))
        |> Id.Set.union(typ_all_ids(ty))
        |> Id.Set.union(go(p))
      | Constructor(_, _)
      | Label(_)
      | ExplicitNonlabel => ids_set(IdTagged.ids(pat))
      | _ => Id.Set.empty
      };
    go(user_term);
  | Some(Info.InfoExp({user_term, _})) =>
    switch (Exp.term_of(user_term)) {
    | Constructor(_, _)
    | Label(_)
    | ExplicitNonlabel => ids_set(IdTagged.ids(user_term))
    | _ => Id.Set.empty
    }
  | _ => Id.Set.empty
  };

let child_reveal_ids =
    (path: Id.Set.t, m: Id.Map.t(Info.t), id: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(id, m)) {
  | Some(Info.InfoExp({user_term, _})) =>
    switch (Exp.term_of(user_term)) {
    | Ap(_, _, _) =>
      exp_child_ids(user_term)
      |> List.filter(child_id => !on_path(path, child_id))
      |> List.map(child_head_reveal(m))
      |> List.fold_left(Id.Set.union, Id.Set.empty)
    | _ => Id.Set.empty
    }
  | Some(Info.InfoPat({user_term, _})) =>
    pat_child_ids(user_term)
    |> List.filter(child_id => !on_path(path, child_id))
    |> List.map(child_head_reveal(m))
    |> List.fold_left(Id.Set.union, Id.Set.empty)
  | _ => Id.Set.empty
  };

let reveal_ids_for_path = (path: Id.Set.t, m: Id.Map.t(Info.t)): Id.Set.t =>
  Id.Set.fold(
    (id, acc) => Id.Set.union(acc, child_reveal_ids(path, m, id)),
    path,
    Id.Set.empty,
  );

let exp_contains_focus = (focus: Id.t, exp: Exp.t): bool =>
  switch (
    Exp.map_term(
      ~f_exp=
        (continue, exp) =>
          Id.equal(Exp.rep_id(exp), focus)
            ? raise(Contains_focus) : continue(exp),
      ~f_pat=
        (continue, pat) =>
          Id.equal(Pat.rep_id(pat), focus)
            ? raise(Contains_focus) : continue(pat),
      ~f_typ=
        (continue, typ) =>
          Id.equal(Typ.rep_id(typ), focus)
            ? raise(Contains_focus) : continue(typ),
      exp,
    )
  ) {
  | exception Contains_focus => true
  | _ => false
  };

let pat_contains_focus = (focus: Id.t, pat: Pat.t): bool =>
  switch (
    Pat.map_term(
      ~f_pat=
        (continue, pat) =>
          Id.equal(Pat.rep_id(pat), focus)
            ? raise(Contains_focus) : continue(pat),
      ~f_typ=
        (continue, typ) =>
          Id.equal(Typ.rep_id(typ), focus)
            ? raise(Contains_focus) : continue(typ),
      pat,
    )
  ) {
  | exception Contains_focus => true
  | _ => false
  };

let match_rule_contains_focus = (focus: Id.t, (p, e): (Pat.t, Exp.t)): bool =>
  pat_contains_focus(focus, p) || exp_contains_focus(focus, e);

let match_sibling_branch_ids = (focus: Id.t, m: Id.Map.t(Info.t)): Id.Set.t =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (info) {
      | Info.InfoExp({user_term: {term: Match(_, rules), _}, _})
          when List.exists(match_rule_contains_focus(focus), rules) =>
        List.fold_left(
          (acc, (p, e)) =>
            match_rule_contains_focus(focus, (p, e))
              ? acc
              : acc
                |> Id.Set.union(ids_set(pat_all_ids(p)))
                |> Id.Set.union(source_ids(e)),
          acc,
          rules,
        )
      | _ => acc
      },
    m,
    Id.Set.empty,
  );

let match_focus_scrut_result =
    (focus: Id.t, m: Id.Map.t(Info.t), result: result): result => {
  let ancestors =
    switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
    | Some((ancestors, _)) => ancestors
    | None => []
    };
  List.fold_left(
    (result, ancestor) =>
      switch (Id.Map.find_opt(ancestor, m)) {
      | Some(Info.InfoExp({user_term: {term: Match(scrut, rules), _}, _})) =>
        switch (List.find_opt(match_rule_contains_focus(focus), rules)) {
        | Some((p, _)) =>
          let names = Pat.bound_vars(p);
          let demand = {
            let d = pattern_demand(p, result.gamma);
            demand_is_gap(d) ? gap : d;
          };
          let scrut_slice =
            switch (dispatch_focus(m, Exp.rep_id(scrut), demand)) {
            | Some(slice) => slice
            | None => empty_result
            };
          let omitted =
            result.omitted
            |> Id.Set.diff(_, source_ids(scrut))
            |> Id.Set.diff(_, ids_set(pat_all_ids(p)))
            |> Id.Set.union(_, scrut_slice.omitted)
            |> Id.Set.union(_, pattern_omissions(p, demand));
          {
            omitted,
            gamma:
              gamma_join(
                gamma_discharge(result.gamma, names),
                scrut_slice.gamma,
              ),
            context: context_join(result.context, scrut_slice.context),
            psi: result.psi,
            ana: result.ana,
          };
        | None => result
        }
      | _ => result
      },
    result,
    ancestors,
  );
};

let annotation_ids = (p: Pat.t): Id.Set.t =>
  switch (pat_annotation(p)) {
  | Some(ty) => typ_all_ids(ty)
  | None => Id.Set.empty
  };

let annotation_omissions = (p: Pat.t, demand: Typ.t): Id.Set.t =>
  switch (pat_annotation(p)) {
  | Some(ty) => typ_omissions(ty, demand)
  | None => Id.Set.empty
  };

let binding_def_result =
    (m: Id.Map.t(Info.t), result: result, p: Pat.t, def: Exp.t): result => {
  let names = Pat.bound_vars(p);
  let demand = {
    let d = pattern_demand(p, result.gamma);
    is_gap(d) ? gap : d;
  };
  let def_ids = source_ids(def);
  let pat_ids = ids_set(pat_all_ids(p));
  if (is_gap(demand)) {
    {
      ...result,
      omitted:
        result.omitted |> Id.Set.union(def_ids) |> Id.Set.union(pat_ids),
    };
  } else {
    let def_slice =
      switch (dispatch_focus(m, Exp.rep_id(def), demand)) {
      | Some(slice) => slice
      | None => empty_result
      };
    {
      omitted:
        result.omitted
        |> Id.Set.diff(_, def_ids)
        |> Id.Set.diff(_, pat_ids)
        |> Id.Set.diff(_, annotation_ids(p))
        |> Id.Set.union(_, def_slice.omitted)
        |> Id.Set.union(_, pattern_omissions(p, demand))
        |> Id.Set.union(_, annotation_omissions(p, demand)),
      gamma:
        gamma_join(gamma_discharge(result.gamma, names), def_slice.gamma),
      context: context_join(result.context, def_slice.context),
      psi: result.psi,
      ana: result.ana,
    };
  };
};

let fun_param_result = (result: result, p: Pat.t): result => {
  let names = Pat.bound_vars(p);
  let demand = {
    let d = pattern_demand(p, result.gamma);
    is_gap(d) ? gap : d;
  };
  if (is_gap(demand)) {
    result;
  } else {
    {
      ...result,
      omitted:
        result.omitted
        |> Id.Set.diff(_, ids_set(pat_all_ids(p)))
        |> Id.Set.diff(_, annotation_ids(p))
        |> Id.Set.union(_, pattern_omissions(p, demand))
        |> Id.Set.union(_, annotation_omissions(p, demand)),
      gamma: gamma_discharge(result.gamma, names),
    };
  };
};

let binding_overlay =
    (
      ~except: list(Var.t)=[],
      focus: Id.t,
      m: Id.Map.t(Info.t),
      result: result,
    )
    : result => {
  let ancestors =
    switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
    | Some((ancestors, _)) => ancestors
    | None => []
    };
  List.fold_left(
    (result, ancestor) =>
      switch (Id.Map.find_opt(ancestor, m)) {
      | Some(Info.InfoExp({user_term, _})) =>
        switch (Exp.term_of(user_term)) {
        | Let(p, def, body)
        | Theorem(p, def, body)
            when
              exp_contains_focus(focus, body)
              && !exp_contains_focus(focus, def)
              && !pat_contains_focus(focus, p) =>
          binding_def_result(
            m,
            {
              ...result,
              gamma: gamma_discharge(result.gamma, except),
            },
            p,
            def,
          )
        | Fun(p, body, _, _)
            when
              exp_contains_focus(focus, body)
              && !pat_contains_focus(focus, p) =>
          fun_param_result(
            {
              ...result,
              gamma: gamma_discharge(result.gamma, except),
            },
            p,
          )
        | _ => result
        }
      | _ => result
      },
    result,
    ancestors,
  );
};

let rec pattern_binder_ids = (keep: string => bool, p: Pat.t): Id.Set.t =>
  switch (Pat.term_of(p)) {
  | Parens(inner)
  | Asc(inner, _) => pattern_binder_ids(keep, inner)
  | Var(name) => keep(name) ? ids_set(IdTagged.ids(p)) : Id.Set.empty
  | Tuple(ps)
  | ListLit(ps) =>
    List.fold_left(
      (acc, p) => Id.Set.union(acc, pattern_binder_ids(keep, p)),
      Id.Set.empty,
      ps,
    )
  | TupLabel(_, p) => pattern_binder_ids(keep, p)
  | Cons(hd, tl) =>
    Id.Set.union(pattern_binder_ids(keep, hd), pattern_binder_ids(keep, tl))
  | Ap(_, payload) => pattern_binder_ids(keep, payload)
  | _ => Id.Set.empty
  };

let pat_type_ids = (pat: Pat.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let note = ids =>
    acc := List.fold_left((s, id) => Id.Set.add(id, s), acc^, ids);
  ignore(
    Pat.map_term(
      ~f_typ=
        (continue, t) => {
          note(IdTagged.ids(t));
          continue(t);
        },
      ~f_tpat=
        (continue, tp) => {
          note(IdTagged.ids(tp));
          continue(tp);
        },
      pat,
    ),
  );
  acc^;
};

let def_referenced_names =
    (def: Exp.t, bound: list(string), omitted: Id.Set.t): list(string) => {
  let acc = ref([]);
  ignore(
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Var(name)
              when
                List.mem(name, bound)
                && !Id.Set.mem(Exp.rep_id(e), omitted)
                && !List.mem(name, acc^) =>
            acc := [name, ...acc^]
          | _ => ()
          };
          continue(e);
        },
      def,
    ),
  );
  acc^;
};

let binding_def_focus_overlay =
    (focus: Id.t, m: Id.Map.t(Info.t), query: Typ.t, result: result): result => {
  let ancestors =
    switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
    | Some((ancestors, _)) => ancestors
    | None => []
    };
  List.fold_left(
    (result, ancestor) =>
      switch (Id.Map.find_opt(ancestor, m)) {
      | Some(Info.InfoExp({user_term, _})) =>
        switch (Exp.term_of(user_term)) {
        | Let(p, def, _)
        | Theorem(p, def, _)
            when
              exp_contains_focus(focus, def) && !pat_contains_focus(focus, p) =>
          let referenced =
            def_referenced_names(def, Pat.bound_vars(p), result.omitted);
          let reveal =
            pat_subtree_ids(p)
            |> Id.Set.diff(_, pat_type_ids(p))
            |> Id.Set.diff(
                 _,
                 pattern_binder_ids(name => !List.mem(name, referenced), p),
               );
          {
            ...result,
            omitted: Id.Set.diff(result.omitted, reveal),
          };
        | FixF(p, body, _)
            when
              exp_contains_focus(focus, body)
              && !pat_contains_focus(focus, p) => {
            ...result,
            omitted:
              result.omitted
              |> Id.Set.diff(_, exp_subtree_ids(body))
              |> Id.Set.diff(_, pat_subtree_ids(p))
              |> Id.Set.union(_, pattern_omissions(p, query)),
          }
        | _ => result
        }
      | _ => result
      },
    result,
    ancestors,
  );
};

let projection_focus_overlay =
    (focus: Id.t, m: Id.Map.t(Info.t), result: result): result => {
  let ancestors =
    switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
    | Some((ancestors, _)) => ancestors
    | None => []
    };
  List.fold_left(
    (result, ancestor) =>
      switch (Id.Map.find_opt(ancestor, m)) {
      | Some(Info.InfoExp({user_term, _})) =>
        switch (Exp.term_of(user_term)) {
        | Dot(receiver, label) when exp_contains_focus(focus, receiver) => {
            ...result,
            omitted: Id.Set.diff(result.omitted, exp_subtree_ids(label)),
          }
        | _ => result
        }
      | _ => result
      },
    result,
    ancestors,
  );
};

let cons_tail_focus_overlay =
    (focus: Id.t, m: Id.Map.t(Info.t), result: result): result => {
  let ancestors =
    switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
    | Some((ancestors, _)) => ancestors
    | None => []
    };
  List.fold_left(
    (result, ancestor) =>
      switch (Id.Map.find_opt(ancestor, m)) {
      | Some(Info.InfoExp({user_term, _})) =>
        switch (Exp.term_of(user_term)) {
        | Cons(_, tail) when exp_contains_focus(focus, tail) => {
            ...result,
            omitted: Id.Set.diff(result.omitted, exp_subtree_ids(tail)),
          }
        | _ => result
        }
      | _ => result
      },
    result,
    ancestors,
  );
};

let typabs_focus_overlay =
    (focus: Id.t, m: Id.Map.t(Info.t), result: result): result =>
  switch (Id.Map.find_opt(focus, m)) {
  | Some(Info.InfoExp({user_term, _})) =>
    switch (Exp.term_of(user_term)) {
    | TypAbs(_, body, _) =>
      let shell_ids =
        Id.Set.diff(exp_subtree_ids(user_term), exp_subtree_ids(body));
      {
        ...result,
        omitted: Id.Set.diff(result.omitted, shell_ids),
      };
    | _ => result
    }
  | _ => result
  };

let referenced_type_vars = (omitted: Id.Set.t, e: Exp.t): list(string) => {
  let acc = ref([]);
  ignore(
    Exp.map_term(
      ~f_typ=
        (continue, t) => {
          switch (Typ.term_of(t)) {
          | Var(name) when !Id.Set.mem(Typ.rep_id(t), omitted) =>
            acc := [name, ...acc^]
          | _ => ()
          };
          continue(t);
        },
      e,
    ),
  );
  acc^;
};

let typabs_binder_overlay =
    (focus: Id.t, m: Id.Map.t(Info.t), root: Exp.t, result: result): result => {
  let refs = referenced_type_vars(result.omitted, root);
  let ancestors =
    switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
    | Some((ancestors, _)) => ancestors
    | None => []
    };
  List.fold_left(
    (result, ancestor) =>
      switch (Id.Map.find_opt(ancestor, m)) {
      | Some(Info.InfoExp({user_term, _})) =>
        switch (Exp.term_of(user_term)) {
        | TypAbs(tpat, body, _) when exp_contains_focus(focus, body) =>
          switch (TPat.head_name_of(tpat)) {
          | Some(name) when List.mem(name, refs) =>
            let shell_ids =
              Id.Set.diff(
                exp_subtree_ids(user_term),
                exp_subtree_ids(body),
              );
            {
              ...result,
              omitted: Id.Set.diff(result.omitted, shell_ids),
            };
          | _ => result
          }
        | _ => result
        }
      | _ => result
      },
    result,
    ancestors,
  );
};

let mod_contains_focus = (focus: Id.t, item: Mod.t): bool =>
  switch (item.term) {
  | ModLet(p, e) =>
    pat_contains_focus(focus, p) || exp_contains_focus(focus, e)
  | ModExp(e)
  | ModuleMod(_, e) => exp_contains_focus(focus, e)
  | ModType(_)
  | EmptyHole
  | Invalid(_)
  | MultiHole(_) => false
  };

let module_label_has_focus =
    (focus: Id.t, name: string, items: list(Mod.t)): bool =>
  List.exists(
    item =>
      List.mem(name, module_binding_names(item))
      && mod_contains_focus(focus, item),
    items,
  );

let combine_omission_adjustment =
    (
      (add_a, remove_a): (Id.Set.t, Id.Set.t),
      (add_b, remove_b): (Id.Set.t, Id.Set.t),
    )
    : (Id.Set.t, Id.Set.t) => (
  Id.Set.union(add_a, add_b),
  Id.Set.union(remove_a, remove_b),
);

let module_focus_adjustment =
    (focus: Id.t, items: list(Mod.t)): (Id.Set.t, Id.Set.t) =>
  List.fold_left(
    ((add, remove), item) =>
      if (mod_contains_focus(focus, item)) {
        switch (item.term) {
        | ModLet(p, _) => (
            add,
            Id.Set.union(remove, ids_set(pat_all_ids(p))),
          )
        | _ => (add, remove)
        };
      } else {
        switch (module_binding_names(item)) {
        | [] => (add, remove)
        | _ => (Id.Set.add(Mod.rep_id(item), add), remove)
        };
      },
    (Id.Set.empty, Id.Set.empty),
    items,
  );

let module_focus_adjustments =
    (focus: Id.t, m: Id.Map.t(Info.t)): (Id.Set.t, Id.Set.t) =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (info) {
      | Info.InfoExp({user_term: {term: Module(items), _}, _})
          when List.exists(mod_contains_focus(focus), items) =>
        combine_omission_adjustment(
          acc,
          module_focus_adjustment(focus, items),
        )
      | _ => acc
      },
    m,
    (Id.Set.empty, Id.Set.empty),
  );

let focus_container_path = (focus: Id.t, m: Id.Map.t(Info.t)): Id.Set.t =>
  Id.Map.fold(
    (id, info, acc) =>
      switch (info) {
      | Info.InfoExp({user_term, _})
          when exp_contains_focus(focus, user_term) =>
        Id.Set.add(id, acc)
      | Info.InfoPat({user_term, _})
          when pat_contains_focus(focus, user_term) =>
        Id.Set.add(id, acc)
      | _ => acc
      },
    m,
    Id.Set.empty,
  );

let rec exp_annotation_query =
        (
          path: Id.Set.t,
          focus: Id.t,
          exp: Exp.t,
          annotation: Typ.t,
          focus_query: Typ.t,
        )
        : Typ.t =>
  if (Id.equal(Exp.rep_id(exp), focus)) {
    focus_query;
  } else {
    switch (Exp.term_of(exp), Typ.term_of(annotation)) {
    | (Parens(inner), _) =>
      exp_annotation_query(path, focus, inner, annotation, focus_query)
    | (Asc(inner, _), _) =>
      exp_annotation_query(path, focus, inner, annotation, focus_query)
    | (_, Parens(inner)) =>
      exp_annotation_query(path, focus, exp, inner, focus_query)
    | (Tuple(es), Prod(ts)) when List.length(es) == List.length(ts) =>
      Prod(
        List.map2(
          (e, ty) =>
            exp_contains_focus(focus, e) || on_path(path, Exp.rep_id(e))
              ? exp_annotation_query(path, focus, e, ty, focus_query) : gap,
          es,
          ts,
        ),
      )
      |> Typ.temp
    | (TupLabel(_, e), TupLabel(label, ty)) =>
      TupLabel(
        label,
        exp_contains_focus(focus, e) || on_path(path, Exp.rep_id(e))
          ? exp_annotation_query(path, focus, e, ty, focus_query) : gap,
      )
      |> Typ.temp
    | (Module(items), Prod(fields)) =>
      Prod(
        List.map(
          field =>
            switch (Typ.term_of(field)) {
            | TupLabel({term: Label(name), _} as label, _)
                when module_label_has_focus(focus, name, items) =>
              TupLabel(label, focus_query) |> Typ.temp
            | TupLabel(label, _) => TupLabel(label, gap) |> Typ.temp
            | _ => gap
            },
          fields,
        ),
      )
      |> Typ.temp
    | (ListLit(_), List(_))
    | (Cons(_, _), List(_)) => List(focus_query) |> Typ.temp
    | (Fun(p, body, _, _), Arrow(dom, cod)) =>
      Arrow(
        pat_contains_focus(focus, p) || on_path(path, Pat.rep_id(p))
          ? pat_annotation_query(path, focus, p, dom, focus_query) : gap,
        exp_contains_focus(focus, body) || on_path(path, Exp.rep_id(body))
          ? exp_annotation_query(path, focus, body, cod, focus_query) : gap,
      )
      |> Typ.temp
    | _ => focus_query
    };
  }
and pat_annotation_query =
    (
      path: Id.Set.t,
      focus: Id.t,
      pat: Pat.t,
      annotation: Typ.t,
      focus_query: Typ.t,
    )
    : Typ.t =>
  if (Id.equal(Pat.rep_id(pat), focus)) {
    focus_query;
  } else {
    switch (Pat.term_of(pat), Typ.term_of(annotation)) {
    | (Parens(inner), _) =>
      pat_annotation_query(path, focus, inner, annotation, focus_query)
    | (Asc(inner, _), _) =>
      pat_annotation_query(path, focus, inner, annotation, focus_query)
    | (_, Parens(inner)) =>
      pat_annotation_query(path, focus, pat, inner, focus_query)
    | (Tuple(ps), Prod(ts)) when List.length(ps) == List.length(ts) =>
      Prod(
        List.map2(
          (p, ty) =>
            pat_contains_focus(focus, p) || on_path(path, Pat.rep_id(p))
              ? pat_annotation_query(path, focus, p, ty, focus_query) : gap,
          ps,
          ts,
        ),
      )
      |> Typ.temp
    | (TupLabel(_, p), TupLabel(label, ty)) =>
      TupLabel(
        label,
        pat_contains_focus(focus, p) || on_path(path, Pat.rep_id(p))
          ? pat_annotation_query(path, focus, p, ty, focus_query) : gap,
      )
      |> Typ.temp
    | (ListLit(_), List(_))
    | (Cons(_, _), List(_)) => List(focus_query) |> Typ.temp
    | _ => focus_query
    };
  };

let rec pat_ctor_head = (pat: Pat.t): option(string) =>
  switch (Pat.term_of(pat)) {
  | Parens(p)
  | Asc(p, _) => pat_ctor_head(p)
  | Constructor(name, _) => Some(name)
  | Ap(f, _) => pat_ctor_head(f)
  | _ => None
  };

let constructor_alias_count = (ctx: Ctx.t, name: string): int =>
  List.length(
    List.filter(
      fun
      | Ctx.TVarEntry({kind: Ctx.Singleton(shape), _}) =>
        constructor_schema_from_sum(name, gap, shape) != None
      | _ => false,
      ctx.entries,
    ),
  );

let annotation_adjustment = (ty: Typ.t, query: Typ.t): (Id.Set.t, Id.Set.t) => {
  let add = typ_omissions(ty, query);
  let remove = Id.Set.diff(typ_all_ids(ty), add);
  (add, remove);
};

let rec pat_annotation_shell_ids = (pat: Pat.t): Id.Set.t =>
  switch (Pat.term_of(pat)) {
  | Parens(p)
  | Asc(p, _) => Id.Set.add(Pat.rep_id(pat), pat_annotation_shell_ids(p))
  | _ => Id.Set.empty
  };

let annotation_adjustments_for_path =
    (path: Id.Set.t, focus: Id.t, m: Id.Map.t(Info.t), focus_query: Typ.t)
    : (Id.Set.t, Id.Set.t) =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (info) {
      | Info.InfoExp({user_term, ctx, _}) =>
        switch (Exp.term_of(user_term)) {
        | Asc(e, ty)
            when
              exp_contains_focus(focus, e) || on_path(path, Exp.rep_id(e)) =>
          combine_omission_adjustment(
            acc,
            annotation_adjustment(
              ty,
              exp_annotation_query(path, focus, e, ty, focus_query),
            ),
          )
        | Let(pat, def, _)
        | Theorem(pat, def, _)
            when
              pat_annotation(pat) != None
              && (
                exp_contains_focus(focus, def)
                || on_path(path, Exp.rep_id(def))
              ) =>
          switch (pat_annotation(pat)) {
          | Some(ty) =>
            let redundant =
              switch (exp_constructor_head(def)) {
              | Some(name) => constructor_alias_count(ctx, name) <= 1
              | None => false
              };
            let adjustment =
              redundant
                ? annotation_adjustment(ty, gap)
                : annotation_adjustment(
                    ty,
                    exp_annotation_query(path, focus, def, ty, focus_query),
                  );
            combine_omission_adjustment(
              acc,
              combine_omission_adjustment(
                adjustment,
                (Id.Set.empty, pat_annotation_shell_ids(pat)),
              ),
            );
          | None => acc
          }
        | _ => acc
        }
      | Info.InfoPat({user_term, ctx, _}) =>
        switch (Pat.term_of(user_term)) {
        | Asc(p, ty)
            when
              (pat_contains_focus(focus, p) || on_path(path, Pat.rep_id(p)))
              && (
                switch (pat_ctor_head(p)) {
                | Some(name) => constructor_alias_count(ctx, name) <= 1
                | None => false
                }
              ) =>
          /* Redundant constructor-pattern annotation: omit it entirely. */
          combine_omission_adjustment(acc, annotation_adjustment(ty, gap))
        | Asc(p, ty)
            when
              pat_contains_focus(focus, p) || on_path(path, Pat.rep_id(p)) =>
          combine_omission_adjustment(
            acc,
            annotation_adjustment(
              ty,
              pat_annotation_query(path, focus, p, ty, focus_query),
            ),
          )
        | _ => acc
        }
      | _ => acc
      },
    m,
    (Id.Set.empty, Id.Set.empty),
  );

let focus_should_omit = (m: Id.Map.t(Info.t), focus: Id.t): bool =>
  switch (Id.Map.find_opt(focus, m)) {
  | Some(Info.InfoPat(_)) => true
  | Some(Info.InfoExp({user_term, ctx, _})) =>
    switch (Exp.term_of(user_term)) {
    | Var(name) =>
      switch (Ctx.lookup_var(ctx, name)) {
      | Some({id, _}) =>
        switch (Id.Map.find_opt(id, m)) {
        | Some(Info.InfoPat(_)) => true
        | _ => false
        }
      | None => false
      }
    | Constructor(_, _) => false
    | _ => true
    }
  | _ => false
  };

let analysis_focus_query =
    (m: Id.Map.t(Info.t), focus: Id.t, query: Typ.t): Typ.t =>
  switch (Id.Map.find_opt(focus, m)) {
  | Some(Info.InfoExp({user_term: {term: Var(_), _}, ty, _})) =>
    switch (Typ.term_of(ty), Typ.term_of(query)) {
    | (Arrow(_, _), Arrow(_, _)) => query
    | (Arrow(_, _), _) => Arrow(gap, query) |> Typ.temp
    | _ => query
    }
  | _ => query
  };

let gamma_add_exp_var = (gamma: gamma, exp: Exp.t, ty: Typ.t): gamma =>
  switch (Exp.term_of(exp)) {
  | Var(name) => gamma_add(gamma, name, ty)
  | _ => gamma
  };

let analysis_edge_gamma =
    (path: Id.Set.t, m: Id.Map.t(Info.t), query: Typ.t): gamma =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (info) {
      | Info.InfoExp({user_term: {term: Ap(_, fn, arg), _}, _}) =>
        if (on_path(path, Exp.rep_id(arg))) {
          gamma_add_exp_var(acc, fn, Arrow(query, gap) |> Typ.temp);
        } else if (on_path(path, Exp.rep_id(fn))) {
          gamma_add_exp_var(acc, fn, Arrow(gap, query) |> Typ.temp);
        } else {
          acc;
        }
      | _ => acc
      },
    m,
    VarMap.empty,
  );

let used_term_constructors =
    (m: Id.Map.t(Info.t), omitted: Id.Set.t): list(string) =>
  Id.Map.fold(
    (id, info, acc) => {
      let (name, ancestors) =
        switch (info) {
        | Info.InfoExp({user_term, ancestors, _}) => (
            switch (Exp.term_of(user_term)) {
            | Constructor(n, _) => Some(n)
            | _ => None
            },
            ancestors,
          )
        | Info.InfoPat({user_term, ancestors, _}) => (
            switch (Pat.term_of(user_term)) {
            | Constructor(n, _) => Some(n)
            | _ => None
            },
            ancestors,
          )
        | _ => (None, [])
        };
      switch (name) {
      | Some(n)
          when !List.exists(a => Id.Set.mem(a, omitted), [id, ...ancestors]) => [
          n,
          ...acc,
        ]
      | _ => acc
      };
    },
    m,
    [],
  );

let reveal_used_aliases = (m: Id.Map.t(Info.t), result: result): result => {
  let used = used_term_constructors(m, result.omitted);
  let omitted =
    Id.Map.fold(
      (_, info, omitted) =>
        switch (info) {
        | Info.InfoExp({user_term, _}) =>
          switch (Exp.term_of(user_term)) {
          | TyAlias(typat, utyp, _) =>
            let names = alias_constructors(utyp);
            let alias_id = Exp.rep_id(user_term);
            let variant_used = List.exists(n => List.mem(n, used), names);
            let ctor_live =
              variant_used && !alias_shadowed(m, alias_id, names);
            let name_ref =
              switch (TPat.tyvar_of_utpat(typat)) {
              | Some(n) => alias_name_referenced(m, omitted, n)
              | None => false
              };
            let binder_demand =
              switch (TPat.tyvar_of_utpat(typat)) {
              | Some(alias) =>
                switch (Ctx.lookup_alias(result.context, alias)) {
                | Some(ty) => ty
                | None => utyp
                }
              | None => utyp
              };
            if (ctor_live || variant_used && name_ref) {
              omitted
              |> Id.Set.diff(
                   _,
                   Id.Set.union(
                     ids_set(IdTagged.ids(typat)),
                     typ_all_ids(utyp),
                   ),
                 )
              |> Id.Set.union(_, unused_variant_ids(used, utyp))
              |> Id.Set.union(_, unused_binder_ids(used, binder_demand))
              |> Id.Set.union(
                   _,
                   unused_binder_occurrence_ids(used, binder_demand, utyp),
                 )
              |> Id.Set.union(
                   _,
                   omitted_binder_occurrence_ids(omitted, utyp),
                 );
            } else if (name_ref) {
              omitted
              |> Id.Set.diff(_, ids_set(IdTagged.ids(typat)))
              |> Id.Set.union(_, typ_all_ids(utyp));
            } else {
              omitted;
            };
          | _ => omitted
          }
        | _ => omitted
        },
      m,
      result.omitted,
    );
  {
    ...result,
    omitted,
  };
};

let rec ap_spine = (e: Exp.t): (Exp.t, list(Exp.t)) =>
  switch (Exp.term_of(e)) {
  | Ap(_, fn, arg) =>
    let (head, args) = ap_spine(fn);
    (head, args @ [arg]);
  | _ => (e, [])
  };

let rec head_type_args = (e: Exp.t): list(Typ.t) =>
  switch (Exp.term_of(e)) {
  | TypAp(fn, args) => head_type_args(fn) @ typ_args(args)
  | Ap(_, fn, _) => head_type_args(fn)
  | _ => []
  };

let rec head_var = (e: Exp.t): option(string) =>
  switch (Exp.term_of(e)) {
  | Var(name) => Some(name)
  | TypAp(fn, _)
  | Ap(_, fn, _)
  | Parens(fn) => head_var(fn)
  | _ => None
  };

let binding_def_of = (m: Id.Map.t(Info.t), name: string): option(Exp.t) =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (acc) {
      | Some(_) => acc
      | None =>
        switch (info) {
        | Info.InfoExp({user_term, _}) =>
          switch (Exp.term_of(user_term)) {
          | Let(p, def, _) when List.mem(name, Pat.bound_vars(p)) =>
            Some(def)
          | _ => None
          }
        | _ => None
        }
      },
    m,
    None,
  );

let rec def_value_params = (def: Exp.t): list(Pat.t) =>
  switch (Exp.term_of(def)) {
  | TypAbs(_, body, _) => def_value_params(body)
  | Parens(e) => def_value_params(e)
  | Fun(p, body, _, _) => [p, ...def_value_params(body)]
  | _ => []
  };

let rec def_value_param_nodes = (def: Exp.t): list((Pat.t, Exp.t)) =>
  switch (Exp.term_of(def)) {
  | TypAbs(_, body, _) => def_value_param_nodes(body)
  | Parens(e) => def_value_param_nodes(e)
  | Fun(p, body, _, _) => [(p, def), ...def_value_param_nodes(body)]
  | _ => []
  };

let rec def_typabs_binders = (def: Exp.t): list(TPat.t) =>
  switch (Exp.term_of(def)) {
  | TypAbs(tpat, body, _) => [tpat, ...def_typabs_binders(body)]
  | Parens(e) => def_typabs_binders(e)
  | _ => []
  };

let rec def_body = (def: Exp.t): Exp.t =>
  switch (Exp.term_of(def)) {
  | TypAbs(_, body, _)
  | Fun(_, body, _, _)
  | Parens(body) => def_body(body)
  | _ => def
  };

let kept_ann_type_vars = (ann: Typ.t, query: Typ.t): list(string) => {
  let omit = typ_omissions(ann, query);
  let cover = ref(omit);
  ignore(
    Typ.map_term(
      ~f_typ=
        (continue, t) => {
          if (Id.Set.mem(Typ.rep_id(t), omit)) {
            cover := Id.Set.union(cover^, typ_all_ids(t));
          };
          continue(t);
        },
      ann,
    ),
  );
  let acc = ref([]);
  ignore(
    Typ.map_term(
      ~f_typ=
        (continue, t) => {
          switch (Typ.term_of(t)) {
          | Var(name) when !Id.Set.mem(Typ.rep_id(t), cover^) =>
            acc := [name, ...acc^]
          | _ => ()
          };
          continue(t);
        },
      ann,
    ),
  );
  acc^;
};

let def_minimal_omissions =
    (m: Id.Map.t(Info.t), head: Exp.t, slot: int, query: Typ.t): Id.Set.t =>
  switch (head_var(head) |> Option.bind(_, binding_def_of(m))) {
  | Some(def) =>
    let param_nodes = def_value_param_nodes(def);
    let params = List.map(fst, param_nodes);
    let focus_ann =
      List.nth_opt(params, slot) |> Option.bind(_, pat_annotation);
    let slot_omit =
      switch (focus_ann) {
      | Some(ann) => typ_omissions(ann, query)
      | None => Id.Set.empty
      };
    let kept_vars =
      switch (focus_ann) {
      | Some(ann) => kept_ann_type_vars(ann, query)
      | None => []
      };
    let other_params_omit =
      param_nodes
      |> List.mapi((i, (p, fn)) =>
           if (i == slot) {
             Id.Set.empty;
           } else if (i > slot) {
             source_ids(fn);
           } else {
             Id.Set.union(pat_subtree_ids(p), annotation_ids(p));
           }
         )
      |> List.fold_left(Id.Set.union, Id.Set.empty);
    let binder_omit =
      def_typabs_binders(def)
      |> List.filter(b =>
           switch (TPat.tyvar_of_utpat(b)) {
           | Some(name) => !List.mem(name, kept_vars)
           | None => true
           }
         )
      |> List.map(b => ids_set(IdTagged.ids(b)))
      |> List.fold_left(Id.Set.union, Id.Set.empty);
    slot_omit
    |> Id.Set.union(_, other_params_omit)
    |> Id.Set.union(_, binder_omit)
    |> Id.Set.union(_, source_ids(def_body(def)));
  | None => Id.Set.empty
  };

let unused_type_arg_ids = (head: Exp.t, query: Typ.t): Id.Set.t =>
  head_type_args(head)
  |> List.filter(arg => !typ_contains(arg, query))
  |> List.map(arg => typ_all_ids(arg))
  |> List.fold_left(Id.Set.union, Id.Set.empty);

let ana_arg_edge_result =
    (focus: Id.t, m: Id.Map.t(Info.t), query: Typ.t, result: result): result => {
  let ancestors =
    switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
    | Some((ancestors, _)) => ancestors
    | None => []
    };
  let arg_fns =
    List.filter_map(
      ancestor =>
        switch (Id.Map.find_opt(ancestor, m)) {
        | Some(Info.InfoExp({user_term: {term: Ap(_, fn, arg), _}, _}))
            when
              Id.equal(Exp.rep_id(arg), focus)
              || exp_contains_focus(focus, arg) =>
          Some(fn)
        | _ => None
        },
      ancestors,
    );
  switch (arg_fns) {
  | [fn, ..._] =>
    let (head, _) = ap_spine(fn);
    let fn_slice =
      switch (
        dispatch_focus(m, Exp.rep_id(fn), Arrow(query, gap) |> Typ.temp)
      ) {
      | Some(slice) => slice
      | None => empty_result
      };
    {
      omitted:
        result.omitted
        |> Id.Set.diff(_, source_ids(fn))
        |> Id.Set.union(_, fn_slice.omitted)
        |> Id.Set.union(_, unused_type_arg_ids(head, query)),
      gamma: gamma_join(result.gamma, fn_slice.gamma),
      context: context_join(result.context, fn_slice.context),
      psi: result.psi,
      ana: result.ana,
    };
  | [] => result
  };
};

let ana_param_annotation_overlay =
    (focus: Id.t, m: Id.Map.t(Info.t), query: Typ.t, result: result): result => {
  let ancestors =
    switch (Id.Map.find_opt(focus, m) |> Option.bind(_, info_ancestors)) {
    | Some((ancestors, _)) => ancestors
    | None => []
    };
  let nearest_fn =
    List.find_map(
      ancestor =>
        switch (Id.Map.find_opt(ancestor, m)) {
        | Some(Info.InfoExp({user_term: {term: Ap(_, fn, arg), _}, _}))
            when
              Id.equal(Exp.rep_id(arg), focus)
              || exp_contains_focus(focus, arg) =>
          Some(fn)
        | _ => None
        },
      ancestors,
    );
  switch (nearest_fn) {
  | Some(fn) =>
    let (head, before) = ap_spine(fn);
    {
      ...result,
      omitted:
        Id.Set.union(
          result.omitted,
          def_minimal_omissions(m, head, List.length(before), query),
        ),
    };
  | None => result
  };
};

let analysis_overlay =
    (
      ~focus: option(Id.t),
      m: Id.Map.t(Info.t),
      query: Typ.t,
      result: result,
    )
    : result =>
  switch (focus) {
  | None => result
  | Some(focus_id) =>
    let path =
      Id.Set.union(
        focus_path(m, focus_id),
        focus_container_path(focus_id, m),
      );
    let module_adjustment = module_focus_adjustments(focus_id, m);
    let with_analysis_adjustments = result => {
      let edge_gamma = analysis_edge_gamma(path, m, query);
      let annotation_adjustment =
        annotation_adjustments_for_path(path, focus_id, m, query);
      {
        ...result,
        gamma: VarMap.is_empty(edge_gamma) ? result.gamma : edge_gamma,
      }
      |> apply_omission_adjustment(_, annotation_adjustment)
      |> apply_omission_adjustment(_, module_adjustment);
    };
    let omitted_path_ancestors =
      Id.Set.remove(focus_id, Id.Set.inter(result.omitted, path));
    let focus_in_binding_def =
      Id.Map.exists(
        (_, info) =>
          switch (info) {
          | Info.InfoExp({user_term, _}) =>
            switch (Exp.term_of(user_term)) {
            | Let(_, def, _)
            | Theorem(_, def, _) => on_path(path, Exp.rep_id(def))
            | _ => false
            }
          | _ => false
          },
        m,
      );
    if (Id.Set.is_empty(omitted_path_ancestors) && !focus_in_binding_def) {
      with_analysis_adjustments(result);
    } else {
      let omitted = Id.Set.diff(result.omitted, path_token_ids(m, path));
      let omitted =
        Id.Set.union(omitted, side_ids_for_revealed_path(path, m));
      let omitted = Id.Set.diff(omitted, reveal_ids_for_path(path, m));
      let omitted =
        focus_should_omit(m, focus_id)
          ? Id.Set.add(focus_id, omitted) : omitted;
      with_analysis_adjustments({
        ...result,
        omitted,
      });
    };
  };

let validate_focus =
    (
      ~focus: option(Id.t),
      ~direction: direction,
      m: Id.Map.t(Info.t),
      query,
    ) =>
  switch (focus) {
  | None => ()
  | Some(id) =>
    switch (Id.Map.find_opt(id, m)) {
    | None => raise(Focus_not_found(id))
    | Some(info) =>
      switch (info) {
      | Info.InfoPat(_) when direction == `Syn => raise(Wrong_focus_sort)
      | Info.InfoExp({user_term, _})
          when
            direction == `Syn
            && (
              switch (Exp.term_of(user_term)) {
              | Constructor(_, _) => true
              | _ => false
              }
            ) =>
        ()
      | Info.InfoExp({user_term, _})
          when
            direction == `Syn
            && (
              switch (
                exp_constructor_head(user_term),
                query_constructor_head(query),
              ) {
              | (Some(exp_head), Some(query_head)) => exp_head == query_head
              | _ => false
              }
            ) =>
        ()
      | Info.InfoExp({user_term: {term: Atom(_), _}, _})
          when direction == `Syn && is_arrow_query(query) =>
        raise(Incompatible_query(query))
      | Info.InfoExp({marks: [_, ..._], _}) when direction == `Syn => ()
      | Info.InfoExp({elab_syn_ty, ctx, _}) when direction == `Syn =>
        if (!is_gap(query) && Typ.meet(ctx, elab_syn_ty, query) == None) {
          raise(Incompatible_query(query));
        }
      | _ => ()
      }
    }
  };

let with_run = (f: unit => 'a): 'a => f();

let slice =
    (
      ~focus: option(Id.t)=None,
      ~direction: direction=`Syn,
      root: exp_result,
      query: Typ.t,
    )
    : result => {
  let (root_info, _, m) = root;
  let src_ids = source_ids(root_info.user_term);
  validate_focus(~focus, ~direction, m, query);
  let whole_focus =
    switch (focus) {
    | None => true
    | Some(id) => Id.equal(id, Exp.rep_id(root_info.user_term))
    };
  let root_query = whole_focus ? query : gap;
  let result = dispatch(root, root_query);
  let result =
    switch (focus) {
    | Some(id) =>
      let focus_query =
        direction == `Ana ? analysis_focus_query(m, id, query) : query;
      switch (dispatch_focus(m, id, focus_query)) {
      | Some(focused) =>
        direction == `Ana
          ? {
            omitted: Id.Set.union(result.omitted, focused.omitted),
            gamma: gamma_join(result.gamma, focused.gamma),
            context: context_join(result.context, focused.context),
            psi: focused.psi,
            ana: focused.ana,
          }
          : {
            omitted:
              Id.Set.union(
                Id.Set.diff(result.omitted, focus_subtree_ids(m, id)),
                focused.omitted,
              ),
            gamma: focused.gamma,
            context: focused.context,
            psi: focused.psi,
            ana: focused.ana,
          }
      | None => result
      };
    | None => result
    };
  let result =
    switch (focus) {
    | Some(focus_id) when direction == `Syn && !whole_focus =>
      let path = focus_path(m, focus_id);
      let omitted =
        is_gap(query)
          ? Id.Set.union(
              Id.Set.diff(
                result.omitted,
                path_token_ids(m, Id.Set.remove(focus_id, path)),
              ),
              Id.Set.union(
                focus_subtree_ids(m, focus_id),
                focus_shell_ids(m, focus_id),
              ),
            )
          : Id.Set.diff(result.omitted, path_token_ids(m, path));
      let omitted =
        Id.Set.union(omitted, match_sibling_branch_ids(focus_id, m));
      match_focus_scrut_result(
        focus_id,
        m,
        {
          ...result,
          omitted,
        },
      )
      |> binding_overlay(focus_id, m)
      |> typabs_binder_overlay(focus_id, m, root_info.user_term)
      |> reveal_used_aliases(m);
    | _ => result
    };
  let result =
    direction == `Syn && is_gap(query) && whole_focus
      ? {
        ...result,
        omitted: src_ids,
      }
      : result;
  let result =
    direction == `Ana ? analysis_overlay(~focus, m, query, result) : result;
  let result =
    switch (focus) {
    | Some(focus_id) when direction == `Ana && !whole_focus =>
      let except =
        switch (Id.Map.find_opt(focus_id, m)) {
        | Some(Info.InfoExp({user_term: {term: Var(name), _}, _})) => [
            name,
          ]
        | _ => []
        };
      let focus_is_exp =
        switch (Id.Map.find_opt(focus_id, m)) {
        | Some(Info.InfoExp(_)) => true
        | _ => false
        };
      let arg_edge = r =>
        focus_is_exp ? ana_arg_edge_result(focus_id, m, query, r) : r;
      let param_edge = r =>
        focus_is_exp
          ? ana_param_annotation_overlay(focus_id, m, query, r) : r;
      arg_edge(result)
      |> binding_def_focus_overlay(focus_id, m, query)
      |> projection_focus_overlay(focus_id, m)
      |> cons_tail_focus_overlay(focus_id, m)
      |> typabs_focus_overlay(focus_id, m)
      |> binding_overlay(~except, focus_id, m)
      |> param_edge
      |> reveal_used_aliases(m);
    | _ => result
    };
  let rec omit_unused_aliases_fix = (omitted: Id.Set.t): Id.Set.t => {
    let omitted' = omit_unused_aliases(m, omitted);
    Id.Set.equal(omitted', omitted)
      ? omitted : omit_unused_aliases_fix(omitted');
  };
  let result = {
    ...result,
    omitted:
      close_omitted_alias_binders(
        m,
        omit_unused_aliases_fix(result.omitted),
      ),
  };
  let result = {
    ...result,
    omitted: Id.Set.inter(result.omitted, src_ids),
  };
  direction == `Ana
    ? {
      ...result,
      ana: query,
    }
    : result;
};

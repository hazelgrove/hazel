open Info;
open TypQuery;
open DemandCtx;

exception Focus_not_found(Id.t);
exception Wrong_focus_sort;
exception Incompatible_query(Typ.t);

type direction = [
  | `Syn
  | `Ana
];

let local_binding = (m, path, binding: Ctx.var_entry) =>
  switch (Id.Map.find_opt(binding.id, m)) {
  | Some(Info.InfoPat(pattern)) =>
    List.exists(Id.Set.mem(_, path), pattern.ancestors)
  | _ => false
  };

let pattern_focus_demand = (m, root, focus, shape, query) =>
  switch (focus) {
  | Some(id) =>
    switch (Id.Map.find_opt(id, m)) {
    | Some(Info.InfoPat(info))
        when
          Id.equal(id, root) || List.exists(Id.equal(root), info.ancestors) =>
      let demand = route_query(info.ana, shape, query);
      let demand =
        empty_query(demand) ? route_query(query, shape, query) : demand;
      meet(info.ctx, demand, query_shell(shape));
    | _ => gap
    }
  | None => gap
  };

let is_focus = (focus, id) =>
  Option.map(Id.equal(id), focus) |> Option.value(~default=false);

type overlay = {
  direction,
  focus: option(Id.t),
  focus_query: Typ.t,
  path: Id.Set.t,
  pattern_focus: bool,
};

let exp_path = (m: Id.Map.t(Info.t), focus: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(focus, m)) {
  | Some(Info.InfoExp({ancestors, _}))
  | Some(Info.InfoPat({ancestors, _})) =>
    Id.Set.add(focus, Id.Set.of_list(ancestors))
  | _ => Id.Set.singleton(focus)
  };

let overlay_for = (~direction, ~focus, ~query, m: Id.Map.t(Info.t)): overlay => {
  direction,
  focus,
  focus_query: focus == None ? gap : query,
  path:
    switch (focus) {
    | Some(id) => exp_path(m, id)
    | None => Id.Set.empty
    },
  pattern_focus:
    switch (focus) {
    | Some(id) =>
      switch (Id.Map.find_opt(id, m)) {
      | Some(Info.InfoPat(_)) => true
      | _ => false
      }
    | None => false
    },
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
        Id.Set.union(go(rest), Id.Set.of_list(IdTagged.ids(e)))
      | _ => Id.Set.empty
      }
    | [] => Id.Set.empty;
  switch (Id.Map.find_opt(focus, m)) {
  | Some(Info.InfoExp({ancestors, _})) => go(ancestors)
  | _ => Id.Set.empty
  };
};

let type_context = (~minimal, m, root, omitted) => {
  let rec owned =
    fun
    | [ancestor, ...ancestors] =>
      switch (Id.Map.find_opt(ancestor, m)) {
      | Some(Info.InfoExp(_)) => Id.equal(ancestor, root)
      | _ => !Id.Set.mem(ancestor, omitted) && owned(ancestors)
      }
    | [] => false;
  Id.Map.fold(
    (id, info, context) =>
      switch (info) {
      | Info.InfoTyp({user_term, ctx, ancestors, _})
          when !Id.Set.mem(id, omitted) && owned(ancestors) =>
        switch (Typ.term_of(user_term)) {
        | Var(name) =>
          switch (tvar_entry(ctx, name)) {
          | None => context
          | Some({kind: Singleton(_), _} as entry) when minimal =>
            Ctx.extend(context, Ctx.TVarEntry(minimal_tvar(entry)))
          | Some(entry) => Ctx.extend(context, Ctx.TVarEntry(entry))
          }
        | _ => context
        }
      | _ => context
      },
    m,
    Ctx.empty,
  );
};

let context_with_types = (~minimal, m, root, omitted, context) => {
  let types = type_context(~minimal, m, root, omitted);
  minimal && !context_has_constructor(context)
    ? context_join(types, context) : context_join(context, types);
};

let minimal_context = (~overlay, m, root, omitted, gamma) =>
  overlay.direction == `Ana
    ? context_with_types(~minimal=true, m, root, omitted, gamma) : gamma;

let rec signature_omissions = (m, ctx, actual, query) => {
  let omitted =
    switch (Id.Map.find_opt(Typ.rep_id(actual), m)) {
    | Some(Info.InfoSig(_))
        when empty_query(query_residual(ctx, query, query_shell(actual))) =>
      Id.Set.singleton(Typ.rep_id(actual))
    | _ => Id.Set.empty
    };
  switch (aligned_children(actual, query)) {
  | Some((actual, query)) =>
    List.map2(signature_omissions(m, ctx), actual, query)
    |> List.fold_left(Id.Set.union, omitted)
  | None => omitted
  };
};

let ascription_result =
    (~overlay, m, ctx, ~root, parent_shape, ~psi, ~gamma): (Id.Set.t, Ctx.t) => {
  let {direction, path, focus_query, _} = overlay;
  let annotation_query =
    direction == `Ana && !Id.Set.is_empty(path)
      ? Option.map(
          path => lift(parent_shape, path, focus_query),
          find_shape_path(focus_query, parent_shape),
        )
        |> Option.value(~default=psi)
      : psi;
  let annotation_query = fill_shell(parent_shape, annotation_query);
  let omitted =
    Id.Set.union(
      ids_of_typ(parent_shape, annotation_query),
      signature_omissions(m, ctx, parent_shape, annotation_query),
    );
  (
    omitted,
    context_with_types(~minimal=direction == `Ana, m, root, omitted, gamma),
  );
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

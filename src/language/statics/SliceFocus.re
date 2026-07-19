open Info;
open TypQuery;

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

let exp_path = (m: Id.Map.t(Info.t), focus: Id.t): Id.Set.t =>
  switch (Id.Map.find_opt(focus, m)) {
  | Some(Info.InfoExp({ancestors, _}))
  | Some(Info.InfoPat({ancestors, _})) =>
    Id.Set.add(focus, Id.Set.of_list(ancestors))
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
        Id.Set.union(go(rest), Id.Set.of_list(IdTagged.ids(e)))
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

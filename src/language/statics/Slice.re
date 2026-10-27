// Public surface of the slicing calculus.

exception Focus_not_found(Id.t);
exception Wrong_focus_sort;
exception Incompatible_query(Typ.t);

let gap = Typ.gap;
let is_gap = Typ.is_gap;

type result = {
  omitted: Id.Set.t,
  gamma: Ctx.t,
  psi: Typ.t,
  ana: Typ.t,
};

type direction = [ | `Syn | `Ana];

let nothing = (~psi=gap, ~ana=gap, ()): result => {
  omitted: Id.Set.empty,
  gamma: Ctx.empty,
  psi,
  ana,
};

let descent = (info_map: StaticsBase.Map.t, focus: Id.t): list(Id.t) =>
  switch (StaticsBase.Map.lookup(focus, info_map)) {
  | None => []
  | Some(info) =>
    switch (List.rev(Info.ancestors_of(info))) {
    | [] => [focus]
    | [_root, ...below] => below @ [focus]
    }
  };

let slice =
    (
      ~info_map: StaticsBase.Map.t,
      ~root_exp: Exp.t,
      ~focus: option(Id.t)=None,
      ~direction: direction=`Syn,
      query: Typ.t,
    )
    : result => {
  let root_id = Exp.rep_id(root_exp);
  let focus_id = Option.value(~default=root_id, focus);
  let focus_info =
    switch (StaticsBase.Map.lookup(focus_id, info_map)) {
    | None => raise(Focus_not_found(focus_id))
    | Some(info) => info
    };
  switch (focus_info) {
  | Info.InfoExp({ctx, ty, ana, elab_syn_ty, _}) =>
    let asks = (actual: Typ.t) =>
      Typ.is_askable(ctx, actual, query)
      || Typ.is_askable(ctx, Typ.weak_head_normalize(ctx, actual), query);
    let askable = (actual: Typ.t) => !Typ.is_gap(actual) && asks(actual);
    if (!asks(ty) && !List.exists(askable, [elab_syn_ty, ana])) {
      raise(Incompatible_query(query));
    };
    let env: SliceSyn.env = {
      info_map,
      focus: focus_id,
    };
    let descent = focus_id == root_id ? [] : descent(info_map, focus_id);
    let r =
      switch (direction) {
      | `Syn => SliceSyn.focused(env, root_exp, descent, query)
      | `Ana =>
        descent == []
          ? SliceSyn.syn(env, root_exp, query)
          : SliceSyn.focused_ana(env, root_exp, descent, query)
      };
    {
      omitted: r.omitted,
      gamma: r.gamma,
      psi: r.psi,
      ana: direction == `Ana ? query : gap,
    };
  | Info.InfoPat({ctx, ty, ancestors, user_term, _}) =>
    switch (direction) {
    | `Syn => raise(Wrong_focus_sort)
    | `Ana =>
      if (!Typ.is_askable(ctx, ty, query)) {
        raise(Incompatible_query(query));
      };
      switch (
        List.find_opt(
          id => StaticsBase.Map.lookup_exp(id, info_map) != None,
          ancestors,
        )
      ) {
      | None => nothing(~ana=query, ())
      | Some(binder) =>
        let env: SliceSyn.env = {
          info_map,
          focus: binder,
        };
        let r =
          SliceSyn.focused_ana_pat(
            env,
            root_exp,
            binder == root_id ? [] : descent(info_map, binder),
            ~pat_focus=focus_id,
            ~pat_focus_var=
              switch (Pat.term_of(user_term)) {
              | Var(x) => Some(x)
              | _ => None
              },
            query,
          );
        {
          omitted: r.omitted,
          gamma: r.gamma,
          psi: r.psi,
          ana: query,
        };
      };
    }
  | _ =>
    switch (direction) {
    | `Syn => raise(Wrong_focus_sort)
    | `Ana => nothing(~ana=query, ())
    }
  };
};

let focus_shell_ids = (info_map: StaticsBase.Map.t, focus: Id.t): Id.Set.t => {
  let rec go = ancestors =>
    switch (ancestors) {
    | [] => Id.Set.empty
    | [id, ...rest] =>
      switch (StaticsBase.Map.lookup_exp(id, info_map)) {
      | Some({user_term, _}) =>
        switch (Exp.term_of(user_term)) {
        | Parens(_)
        | Asc(_, _) =>
          List.fold_left(
            (acc, id) => Id.Set.add(id, acc),
            go(rest),
            IdTagged.ids(user_term),
          )
        | _ => Id.Set.empty
        }
      | None => Id.Set.empty
      }
    };
  switch (StaticsBase.Map.lookup_exp(focus, info_map)) {
  | Some({ancestors, _}) => go(ancestors)
  | None => Id.Set.empty
  };
};

open Util.OptUtil.Syntax;
module Info = Info;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(Info.t);

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type errors = Id.Map.t(Info.error);

  let empty = Id.Map.empty;
  let lookup = Id.Map.find_opt;
  let filter = Id.Map.filter;

  let error_ids = (info_map: t): list(Id.t) =>
    Id.Map.fold(
      (id, info, acc) =>
        /* Second clause is to eliminate non-representative ids,
         * which will not be found in the measurements map */
        Info.is_error(info) && id == Info.id_of(info) ? [id, ...acc] : acc,
      info_map,
      [],
    );
  let warning_ids = (info_map: t): list(Id.t) =>
    Id.Map.fold(
      (id, info, acc) =>
        acc
        |> (
          Info.is_warning(info) && id == Info.id_of(info)
            ? List.cons(id) : Fun.id
        ),
      info_map,
      [],
    );

  let errors = (map: t): list((Id.t, Info.error)) =>
    Id.Map.fold(
      (id, info: Info.t, acc) =>
        Option.to_list(Info.error_of(info) |> Option.map(x => (id, x)))
        @ acc,
      map,
      [],
    );

  let collect_errors = (map: t): errors =>
    Id.Map.filter_map(
      (_: Uuidm.t, info: Info.t) => {Info.error_of(info)},
      map,
    );

  /* The ids of binding sites for for all references in term with `id` */
  let refs_in = (m: t, id: Id.t): Binding.s =>
    switch (lookup(id, m)) {
    | Some(InfoExp({co_ctx, ctx, _})) =>
      co_ctx
      |> Util.VarMap.to_list
      |> List.map(((n, _)) => Ctx.binding_of(ctx, n))
    | _ => []
    };

  let bound_in = (m: t, id: Id.t): Binding.s =>
    switch (lookup(id, m)) {
    | Some(InfoPat({term, _})) => Pat.bindings(term)
    | _ => []
    };

  let parent_ci_of = (map: t, id: Id.t): option(Info.t) => {
    let* ci = lookup(id, map);
    let* parent_id = Info.parent_id_of(ci);
    lookup(parent_id, map);
  };

  let parent_term_of = (map: t, id: Id.t): option(Any.t) => {
    let* ci = parent_ci_of(map, id);
    Info.any_of(ci);
  };

  /* Starting from a binding site id (possibly inside a deep pattern),
   * climb ancestor ids to find the enclosing let, and return
   * the id of its body expression. */
  let enclosing_let_of_binding =
      (~statics: t, ~binding_id: Id.t): option(Id.t) => {
    open Util.OptUtil.Syntax;
    let* ci_binder = lookup(binding_id, statics);
    let rec climb = (ancs: list(Id.t)): option(Id.t) =>
      switch (ancs) {
      | [] => None
      | [ancestor_id, ...rest] =>
        let* ci = lookup(ancestor_id, statics);
        switch (ci) {
        | InfoExp({term: {term: Let(pat, def, _), _}, _}) =>
          let binds = Pat.bindings(pat);
          List.exists((b: Binding.t) => b.id == binding_id, binds)
            ? Some(IdTagged.rep_id(def)) : climb(rest);
        | InfoExp(_) => None
        | _ => climb(rest)
        };
      };
    climb(Info.ancestors_of(ci_binder));
  };

  /* Find all use sites of a binding. Reads the binding pattern's co_ctx,
   * which statics populates with the body scope's co-context. */
  let uses_of_binding = (_m: t, binding_id: Id.t): list(Id.t) => {
    switch (lookup(binding_id, _m)) {
    | Some(InfoPat({term: {term: Var(name), _}, co_ctx, _})) =>
      switch (Util.VarMap.lookup(co_ctx, name)) {
      | Some(entries) => List.map((e: CoCtx.entry) => e.id, entries)
      | None => []
      }
    | _ => []
    };
  };

  /* Find all use sites of a constructor binding. Climbs ancestors to
   * the enclosing TyAlias InfoExp and reads the constructor name from
   * its co_ctx, which contains constructor uses from the body scope. */
  let uses_of_ctr_binding =
      (m: t, binding_id: Id.t, name: string): list(Id.t) => {
    switch (lookup(binding_id, m)) {
    | Some(info) =>
      let rec find_tyalias = (ancs: list(Id.t)): list(Id.t) =>
        switch (ancs) {
        | [] => []
        | [anc_id, ...rest] =>
          switch (lookup(anc_id, m)) {
          | Some(InfoExp({term: {term: TyAlias(_, _, _), _}, co_ctx, _})) =>
            switch (Util.VarMap.lookup(co_ctx, name)) {
            | Some(entries) => List.map((e: CoCtx.entry) => e.id, entries)
            | None => []
            }
          | _ => find_tyalias(rest)
          }
        };
      find_tyalias(Info.ancestors_of(info));
    | _ => []
    };
  };

  /* Find all use sites of a type variable binding. Reads the binding
   * tpat's tvar_co_ctx, which is populated by populate_tvar_co_ctxs. */
  let uses_of_tvar_binding = (m: t, binding_id: Id.t): list(Id.t) => {
    switch (lookup(binding_id, m)) {
    | Some(InfoTPat({term: {term: Var(name), _}, tvar_co_ctx, _})) =>
      switch (Util.VarMap.lookup(tvar_co_ctx, name)) {
      | Some(ids) => ids
      | None => []
      }
    | _ => []
    };
  };

  /* Post-processing pass: populate each InfoTPat's tvar_co_ctx with
   * the IDs of type variable references that resolve to that binding.
   * Single O(n) scan of the info_map + O(n) map update. */
  let populate_tvar_co_ctxs = (m: t): t => {
    /* Step 1: Scan all InfoTyp Var entries, group use-site IDs by binding ID */
    let uses_by_binding: Id.Map.t(list(Id.t)) =
      Id.Map.fold(
        (id, info: Info.t, acc) =>
          switch (info) {
          | InfoTyp({term: {term: Var(name), _}, ctx, _}) =>
            switch (Ctx.lookup_tvar_id(ctx, name)) {
            | Some(bid) when bid != Id.invalid =>
              let existing =
                switch (Id.Map.find_opt(bid, acc)) {
                | Some(ids) => ids
                | None => []
                };
              Id.Map.add(bid, [id, ...existing], acc);
            | _ => acc
            }
          | _ => acc
          },
        m,
        Id.Map.empty,
      );
    /* Step 2: Update each InfoTPat with its tvar_co_ctx */
    Id.Map.map(
      (info: Info.t) =>
        switch (info) {
        | InfoTPat({term: {term: Var(name), _}, _} as tpat_info) =>
          let tpat_id = TPat.rep_id(tpat_info.term);
          let uses =
            switch (Id.Map.find_opt(tpat_id, uses_by_binding)) {
            | Some(ids) => ids
            | None => []
            };
          Info.InfoTPat({
            ...tpat_info,
            tvar_co_ctx: [(name, uses)],
          });
        | other => other
        },
      m,
    );
  };

  /* Given any Info.t, compute the set of related IDs to highlight:
   * - For a variable reference (Var expr): its binding site + sibling uses
   * - For a variable binding (Var pat): all use sites
   * - For a constructor reference: its binding site
   * - For a type variable reference/binding: binding site + all uses */
  let var_highlight_ids = (m: t, info: Info.t): list(Id.t) => {
    switch (info) {
    | InfoExp({term: {term: Var(name), _}, ctx, _}) =>
      switch (Ctx.lookup_var(ctx, name)) {
      | Some(entry) when entry.id != Id.invalid =>
        let binding_id = entry.id;
        let sibling_uses = uses_of_binding(m, binding_id);
        [binding_id, ...sibling_uses];
      | _ => []
      }
    | InfoPat({term: {term: Var(_), _}, _}) =>
      uses_of_binding(m, Info.id_of(info))
    | InfoExp({term: {term: Constructor(name, _), _}, ctx, _})
    | InfoPat({term: {term: Constructor(name, _), _}, ctx, _}) =>
      switch (Ctx.lookup_ctr(ctx, name)) {
      | Some(entry) when entry.id != Id.invalid =>
        let sibling_uses = uses_of_ctr_binding(m, entry.id, name);
        [entry.id, ...sibling_uses];
      | _ =>
        switch (Ctx.lookup_var(ctx, name)) {
        | Some(entry) when entry.id != Id.invalid => [entry.id]
        | _ => []
        }
      }
    | InfoTyp({
        term: {term: Var(name), _},
        expects: ConstructorExpected(_, _),
        _,
      }) =>
      uses_of_ctr_binding(m, Info.id_of(info), name)
    | InfoTyp({term: {term: Var(name), _}, ctx, _}) =>
      switch (Ctx.lookup_tvar_id(ctx, name)) {
      | Some(id) when id != Id.invalid =>
        let sibling_uses = uses_of_tvar_binding(m, id);
        [id, ...sibling_uses];
      | _ => []
      }
    | InfoTPat({term: {term: Var(_), _}, _}) =>
      uses_of_tvar_binding(m, Info.id_of(info))
    | _ => []
    };
  };
};

let let_definition_path = (~statics: Map.t, ~id: Id.t): list(Pat.t) => {
  let rec contains_id = (target: Id.t, ids: list(Id.t)): bool =>
    switch (ids) {
    | [] => false
    | [head, ...tail] => Id.equal(head, target) || contains_id(target, tail)
    };

  let rec gather =
          (remaining: list(Id.t), seen: list(Id.t), acc: list(Pat.t))
          : list(Pat.t) =>
    switch (remaining) {
    | [] => acc
    | [current_id, ...rest] =>
      let acc' =
        switch (Map.lookup(current_id, statics)) {
        | Some(InfoExp({term: {term: Let(pat, def, _), _}, _})) =>
          contains_id(IdTagged.rep_id(def), seen) ? [pat, ...acc] : acc
        | _ => acc
        };
      gather(rest, [current_id, ...seen], acc');
    };

  switch (Map.lookup(id, statics)) {
  | Some(info) =>
    let ancestors: list(Id.t) = Info.ancestors_of(info);
    let collected: list(Pat.t) = gather(ancestors, [id], []);
    List.rev(collected);
  | _ => []
  };
};

let map_m = (f, xs, m: Map.t) =>
  List.fold_left(
    ((xs, m), x) => f(x, m) |> (((x, m)) => (xs @ [x], m)),
    ([], m),
    xs,
  );

let add_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids |> List.fold_left((m, id) => Id.Map.add(id, info, m), m);

let add_missing_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids
  |> List.fold_left(
       (m, id) =>
         switch (Id.Map.find_opt(id, m)) {
         | Some(_) => m
         | None => Id.Map.add(id, info, m)
         },
       m,
     );

let rec is_arrow_like = (t: Typ.t) => {
  switch (t |> Typ.term_of) {
  | Unknown(_) => true
  | Arrow(_) => true
  | Poly(_, t) => is_arrow_like(t)
  | _ => false
  };
};

let is_recursive = (ctx, p, def, syn: Typ.t) => {
  switch (Pat.get_num_of_vars(p), Exp.get_num_of_functions(def)) {
  | (Some(num_vars), Some(num_fns))
      when num_vars != 0 && num_vars == num_fns =>
    let norm = Typ.normalize(ctx, syn);
    switch (norm |> Typ.term_of) {
    | Prod(syns) when List.length(syns) == num_vars =>
      syns |> List.for_all(is_arrow_like)
    | _ when is_arrow_like(norm) => num_vars == 1
    | _ => false
    };
  | _ => false
  };
};

let syn = Unknown(SynSwitch) |> Typ.temp;

module type ExpressionStatics = {
  let uexp_to_info_map:
    (
      ~ctx: Ctx.t,
      ~ana: Typ.t=?,
      ~is_in_filter: bool=?,
      ~ancestors: Info.ancestors=?,
      ~duplicates: list(string)=?,
      ~expected_labels: list(string)=?,
      ~inferred_label: string=?,
      ~override_self: Self.exp=?,
      ~label_sort: bool=?,
      ~dot_labels: list(string)=?,
      Exp.t,
      Map.t
    ) =>
    (Info.exp, Exp.t, Map.t);
  let add':
    (
      ~elab: Exp.t=?,
      ~label_inference: Info.label_inference(Info.exp)=?,
      ~self: Self.exp,
      ~co_ctx: CoCtx.t,
      Map.t
    ) =>
    (Info.exp, Exp.t, Map.t);
  let label_to_info_map:
    (option(list(string)), Typ.t, Exp.t, Map.t) =>
    (option(string), Info.exp, Exp.t, Map.t);
};

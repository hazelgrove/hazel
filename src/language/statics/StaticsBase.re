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

  let has_errors = (map: t): bool =>
    Id.Map.exists((_: Uuidm.t, info: Info.t) => Info.is_error(info), map);

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
      Exp.t,
      Map.t
    ) =>
    (Info.exp, Map.t);
  let add':
    (
      ~label_inference: Info.label_inference(Info.exp)=?,
      ~self: Self.exp,
      ~co_ctx: CoCtx.t,
      Map.t
    ) =>
    (Info.exp, Map.t);
  let label_to_info_map:
    (option(list(string)), Typ.t, Exp.t, Map.t) =>
    (option(string), Info.exp, Map.t);

  let dynamics: DynamicStatics.Map.t;

  let calculate_dynamic_type: Exp.t => option(Typ.t);
};

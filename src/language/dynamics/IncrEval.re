open Util;

/* Cache used by the incremental evaluator to skip re-evaluating sub-expressions
 * whose elaboration and free-variable dependencies haven't changed since the
 * previous run. Function bodies are a "deferred boundary": ids inside a closure
 * body are never cached (so calling a function twice re-runs the body). */

[@deriving (show({with_path: false}), sexp, yojson)]
type projection =
  | TupleIndex(int, int)
  | ListIndex(int, int)
  | ConsHead
  | ConsTail
  | ConstructorArg(string)
  | TupleLabel(option(string))
  | Ascribed;

[@deriving (show({with_path: false}), sexp, yojson)]
type clean_flag =
  | FlagClean
  | FlagDirty
  | FlagTuple(list(clean_flag))
  | FlagList(list(clean_flag))
  | FlagCons(clean_flag, clean_flag)
  | FlagConstructor(string, clean_flag)
  | FlagTupleLabel(option(string), clean_flag)
  | FlagAscribed(clean_flag);

[@deriving (show({with_path: false}), sexp, yojson)]
type provenance = {
  source: Id.t,
  path: list(projection),
  flag: clean_flag,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type reuse_map = VarMap.t_(provenance);

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  prev_elab: Exp.t,
  /* Provenance for every free variable used by this cached subtree: the
   * expression id that supplied the variable's value, plus a projection path
   * through pattern matches. Reuse is sound only when the current run obtained
   * those variables from the same previous-cache entries. */
  prev_reuse_map: reuse_map,
  /* Snapshot of the cached subtree's probe-targets witness. Compared
   * structurally against the current witness in `reuse_check` to detect
   * any add/remove of a probe target inside this subtree.
   *
   * None when the run was made with `probe_all` on: every InfoExp is then
   * a probe, so any change to the probe set coincides with a change to
   * the elaboration tree — `Exp.fast_equal(prev_elab, info.elab_term)`
   * already catches it, and a separate witness would be redundant. */
  prev_probe_targets: option(SubexpProbeTargets.t),
  value: DHExp.t,
  state: StateSlice.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  entries: Id.Map.t(entry),
  /* Ids evaluated from scratch on this run (cache miss). UI tint. */
  recalculated: list(Id.t),
  /* Ids short-circuited via reuse_check (cache hit). Not the complement of
   * `recalculated`: a recalculated parent can still contain reused children. */
  reused: list(Id.t),
};

let empty: t = {
  entries: Id.Map.empty,
  recalculated: [],
  reused: [],
};

let is_empty = (incr: t): bool =>
  Id.Map.is_empty(incr.entries)
  && incr.recalculated == []
  && incr.reused == [];

let add_entry = (id: Id.t, entry: entry, incr: t): t => {
  ...incr,
  entries: Id.Map.add(id, entry, incr.entries),
};

let mark_recalculated = (id: Id.t, incr: t): t => {
  ...incr,
  recalculated: [id, ...incr.recalculated],
};

let mark_reused = (id: Id.t, incr: t): t => {
  ...incr,
  reused: [id, ...incr.reused],
};

/* The set of ids the UI should paint as "frozen" this run.*/
let frozen_ids = (incr: t): list(Id.t) => {
  let acc = ref([]);
  let collect_subtree = (root: Exp.t): unit => {
    let f_exp = (continue, e: Exp.t) => {
      acc := [Exp.rep_id(e), ...acc^];
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, root);
    ();
  };
  List.iter(
    id =>
      switch (Id.Map.find_opt(id, incr.entries)) {
      | Some(entry) => collect_subtree(entry.prev_elab)
      | None => acc := [id, ...acc^]
      },
    incr.reused,
  );
  acc^;
};

let empty_reuse_map: reuse_map = VarMap.empty;

let equal_provenance = (a: provenance, b: provenance): bool =>
  Id.equal(a.source, b.source) && a.path == b.path && a.flag == b.flag;

let equal_reuse_map = (a: reuse_map, b: reuse_map): bool =>
  List.length(a) == List.length(b)
  && List.for_all(
       ((name, prov)) =>
         switch (VarMap.lookup(b, name)) {
         | Some(prov') => equal_provenance(prov, prov')
         | None => false
         },
       a,
     );

let remove_pat_bindings = (pat: Pat.t, reuse_map: reuse_map): reuse_map => {
  let bound = Pat.bound_vars(pat);
  List.filter(((name, _)) => !List.mem(name, bound), reuse_map);
};

let nth_opt = (xs: list('a), index: int): option('a) =>
  index < 0 || index >= List.length(xs) ? None : Some(List.nth(xs, index));

let normalize_tuple_flag = (flags: list(clean_flag)): clean_flag =>
  flags == [] || List.for_all(flag => flag == FlagClean, flags)
    ? FlagClean
    : List.for_all(flag => flag == FlagDirty, flags)
        ? FlagDirty : FlagTuple(flags);

let normalize_list_flag = (flags: list(clean_flag)): clean_flag =>
  flags == [] || List.for_all(flag => flag == FlagClean, flags)
    ? FlagClean
    : List.for_all(flag => flag == FlagDirty, flags)
        ? FlagDirty : FlagList(flags);

let normalize_cons_flag = (hd: clean_flag, tl: clean_flag): clean_flag =>
  hd == FlagClean && tl == FlagClean
    ? FlagClean
    : hd == FlagDirty && tl == FlagDirty ? FlagDirty : FlagCons(hd, tl);

let normalize_constructor_flag = (name: string, flag: clean_flag): clean_flag =>
  switch (flag) {
  | FlagClean => FlagClean
  | FlagDirty => FlagDirty
  | _ => FlagConstructor(name, flag)
  };

let rec project_label_flag = (name: string, flag: clean_flag): clean_flag =>
  switch (flag) {
  | FlagClean => FlagClean
  | FlagDirty => FlagDirty
  | FlagAscribed(flag) => project_label_flag(name, flag)
  | FlagTupleLabel(Some(label), flag) when label == name => flag
  | FlagTupleLabel(_, _) => FlagDirty
  | FlagTuple(fields) =>
    let matches =
      List.filter_map(
        fun
        | FlagTupleLabel(Some(label), flag) when label == name => Some(flag)
        | _ => None,
        fields,
      );
    switch (matches) {
    | [flag] => flag
    | _ => FlagDirty
    };
  | FlagList(fields) =>
    fields |> List.map(project_label_flag(name)) |> normalize_list_flag
  | FlagCons(hd, tl) =>
    normalize_cons_flag(
      project_label_flag(name, hd),
      project_label_flag(name, tl),
    )
  | FlagConstructor(_, _) => FlagDirty
  };

let rec project_flag = (projection: projection, flag: clean_flag): clean_flag =>
  switch (flag, projection) {
  | (FlagClean, _) => FlagClean
  | (FlagDirty, _) => FlagDirty
  | (FlagAscribed(flag), _) => project_flag(projection, flag)
  | (_, Ascribed) => FlagAscribed(flag)
  | (FlagTuple(fields), TupleIndex(arity, index))
      when List.length(fields) == arity =>
    nth_opt(fields, index) |> Option.value(~default=FlagDirty)
  | (FlagList(fields), ListIndex(arity, index))
      when List.length(fields) == arity =>
    nth_opt(fields, index) |> Option.value(~default=FlagDirty)
  | (FlagCons(hd, _), ConsHead) => hd
  | (FlagCons(_, tl), ConsTail) => tl
  | (FlagConstructor(name, flag), ConstructorArg(expected))
      when name == expected => flag
  | (_, TupleLabel(Some(name))) => project_label_flag(name, flag)
  | (FlagTupleLabel(_, flag), TupleLabel(None)) => flag
  | _ => FlagDirty
  };

let project_provenance =
    (projection: projection, provenance: provenance): provenance => {
  ...provenance,
  path: provenance.path @ [projection],
  flag: project_flag(projection, provenance.flag),
};

let project_label_provenance =
    (label: string, provenance: provenance): provenance =>
  project_provenance(TupleLabel(Some(label)), provenance);

let project_coctx_path =
    (path: CoCtx.path, provenance: provenance): provenance =>
  List.fold_left(
    (prov, label) => project_label_provenance(label, prov),
    provenance,
    path,
  );

let clean_provenance = (provenance: provenance): provenance => {
  ...provenance,
  flag: FlagClean,
};

let clean_reuse_map = (reuse_map: reuse_map): reuse_map =>
  List.map(
    ((name, provenance)) => (name, clean_provenance(provenance)),
    reuse_map,
  );

let restrict_to_co_ctx = (reuse_map: reuse_map, co_ctx: CoCtx.t): reuse_map =>
  List.fold_right(
    ((name, entries), projected) =>
      switch (VarMap.lookup(reuse_map, name)) {
      | Some(prov) => [
          (name, project_coctx_path(CoCtx.path_of_entries(entries), prov)),
          ...projected,
        ]
      | None => projected
      },
    VarMap.to_list(co_ctx),
    [],
  );

let reuse_map_for_co_ctx =
    (reuse_map: reuse_map, co_ctx: CoCtx.t): option(reuse_map) =>
  List.fold_right(
    ((name, entries), acc) =>
      switch (acc) {
      | None => None
      | Some(projected) =>
        switch (VarMap.lookup(reuse_map, name)) {
        | Some(prov) =>
          Some([
            (
              name,
              project_coctx_path(CoCtx.path_of_entries(entries), prov),
            ),
            ...projected,
          ])
        | None => None
        }
      },
    VarMap.to_list(co_ctx),
    Some([]),
  );

let pat_label = (pat: Pat.t): option(string) =>
  switch (pat.term) {
  | Label(name) => Some(name)
  | _ => None
  };

let exp_label = (exp: Exp.t): option(string) =>
  switch (exp.term) {
  | Label(name) => Some(name)
  | _ => None
  };

let apply_path_to_flag =
    (path: list(projection), flag: clean_flag): clean_flag =>
  List.fold_left(
    (flag, projection) => project_flag(projection, flag),
    flag,
    path,
  );

let pat_provenance =
    (~source_id: Id.t, ~source_flag: clean_flag=FlagClean, pat: Pat.t)
    : reuse_map => {
  let rec go = (path: list(projection), pat: Pat.t): reuse_map =>
    switch (pat.term) {
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Invalid(_)
    | Atom(_)
    | Label(_)
    | ExplicitNonlabel
    | Constructor(_) => []
    | Var(name) =>
      let path = List.rev(path);
      [
        (
          name,
          {
            source: source_id,
            path,
            flag: apply_path_to_flag(path, source_flag),
          },
        ),
      ];
    | Parens(p)
    | Projector(_, p) => go(path, p)
    | Asc(p, _) => go([Ascribed, ...path], p)
    | TupLabel(label, p) => go([TupleLabel(pat_label(label)), ...path], p)
    | Ap(ctr, p) =>
      switch (Pat.ctr_name(ctr)) {
      | Some(name) => go([ConstructorArg(name), ...path], p)
      | None => go(path, p)
      }
    | Tuple(ps) =>
      let arity = List.length(ps);
      ps
      |> List.mapi((i, p) => go([TupleIndex(arity, i), ...path], p))
      |> List.flatten;
    | ListLit(ps) =>
      let arity = List.length(ps);
      ps
      |> List.mapi((i, p) => go([ListIndex(arity, i), ...path], p))
      |> List.flatten;
    | Cons(hd, tl) =>
      go([ConsHead, ...path], hd) @ go([ConsTail, ...path], tl)
    };
  go([], pat);
};

let with_pat_provenance =
    (
      ~source_id: Id.t,
      ~source_flag: clean_flag,
      pat: Pat.t,
      reuse_map: reuse_map,
    )
    : reuse_map =>
  pat_provenance(~source_id, ~source_flag, pat)
  @ remove_pat_bindings(pat, reuse_map);

let was_reused = (id: Id.t, incr: t): bool => List.mem(id, incr.reused);

type flag_env = VarMap.t_(clean_flag);

let remove_flag_bindings = (names: list(Var.t), env: flag_env): flag_env =>
  List.filter(((name, _)) => !List.mem(name, names), env);

let pat_flags = (~source_flag: clean_flag, pat: Pat.t): flag_env =>
  pat_provenance(~source_id=Id.invalid, ~source_flag, pat)
  |> List.map(((name, provenance)) => (name, provenance.flag));

let extend_flag_env =
    (pat: Pat.t, ~source_flag: clean_flag, env: flag_env): flag_env =>
  pat_flags(~source_flag, pat)
  @ remove_flag_bindings(Pat.bound_vars(pat), env);

let flag_of_exp = (~incr: t, exp: Exp.t): clean_flag => {
  let rec go = (env: flag_env, exp: Exp.t): clean_flag =>
    if (was_reused(Exp.rep_id(exp), incr)) {
      FlagClean;
    } else {
      switch (exp.term) {
      | Var(name) =>
        VarMap.lookup(env, name) |> Option.value(~default=FlagDirty)
      | Parens(exp)
      | Projector(_, exp)
      | Asc(exp, _) => go(env, exp)
      | Tuple(exps) => exps |> List.map(go(env)) |> normalize_tuple_flag
      | ListLit(exps) => exps |> List.map(go(env)) |> normalize_list_flag
      | Cons(hd, tl) => normalize_cons_flag(go(env, hd), go(env, tl))
      | TupLabel(label, exp) =>
        FlagTupleLabel(exp_label(label), go(env, exp))
      | Ap(_, {term: Constructor(name, _), _}, arg) =>
        normalize_constructor_flag(name, go(env, arg))
      | Dot(exp, {term: Label(name), _}) =>
        project_label_flag(name, go(env, exp))
      | Let(pat, rhs, body) =>
        let rhs_flag = go(env, rhs);
        let env = extend_flag_env(pat, ~source_flag=rhs_flag, env);
        go(env, body);
      | _ => FlagDirty
      };
    };
  go(VarMap.empty, exp);
};

let update_maps_after_binding =
    (
      ~source_id: Id.t,
      ~source_flag: clean_flag,
      pat: Pat.t,
      ~reuse_map: reuse_map,
    )
    : reuse_map =>
  with_pat_provenance(~source_id, ~source_flag, pat, reuse_map);

let reuse_check =
    (
      ~call_stack: Sample.call_stack,
      ~prev: t,
      ~reuse_map: reuse_map,
      ~info_map: EvalInfoMap.t,
      ~id: Id.t,
    )
    : option(entry) => {
  open OptUtil.Syntax;

  let* () = OptUtil.some_if(call_stack == [] && !is_empty(prev), ());
  let* entry = Id.Map.find_opt(id, prev.entries);
  let* info = EvalInfoMap.find_opt(id, info_map);

  let elab_same = Exp.fast_equal(entry.prev_elab, info.elab_term);
  let* () = OptUtil.some_if(elab_same, ());

  let* current_reuse_map = reuse_map_for_co_ctx(reuse_map, info.co_ctx);
  let* () =
    OptUtil.some_if(
      equal_reuse_map(entry.prev_reuse_map, current_reuse_map),
      (),
    );

  let* () =
    OptUtil.some_if(
      Option.equal(
        SubexpProbeTargets.equal,
        entry.prev_probe_targets,
        info.probe_targets,
      ),
      (),
    );

  Some(entry);
};

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
type flag =
  | Clean
  | Dirty
  /* aM: one flag per component of a tuple, so that a value can be clean in
   * some components and dirty in others. Kept normalized — `norm` collapses
   * an all-clean or all-dirty list — which is what lets the re-use check stay
   * a structural comparison of flags. */
  | Parts(list(flag));

/* Build a tuple flag from its components, collapsing the uniform cases so
 * that flags stay normalized. */
let norm = (fs: list(flag)): flag =>
  if (List.for_all(f => f == Clean, fs)) {
    Clean;
  } else if (List.for_all(f => f == Dirty, fs)) {
    Dirty;
  } else {
    Parts(fs);
  };

/* The flag of one component of a value the flag `f` describes. A bare
 * clean/dirty flag applies to every component alike. */
let split = (~arity: int, ~index: int, f: flag): flag =>
  switch (f) {
  | Clean => Clean
  | Dirty => Dirty
  | Parts(fs) when List.length(fs) == arity => List.nth(fs, index)
  /* The flag describes a differently shaped value than the pattern is
   * destructuring. Stay conservative rather than guess a correspondence. */
  | Parts(_) => Dirty
  };

/* Pass a flag through a pattern form that does not project componentwise.
 * A component-shaped flag carries no information about where such a form's
 * sub-values sit, so it degrades to dirty. */
let opaque = (f: flag): flag =>
  switch (f) {
  | Parts(_) => Dirty
  | f => f
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type provenance = {
  source: Id.t,
  path: list(projection),
  flag,
};

/* StringMap rather than VarMap: remove_pat_bindings runs at every binder
 * over a map seeded with the whole builtins env, so lookups/removals must
 * not walk an assoc list. */
[@deriving (show({with_path: false}), sexp, yojson)]
type reuse_map = Maps.StringMap.t(provenance);

let empty_reuse_map: reuse_map = Maps.StringMap.empty;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry('state) = {
  prev_elab: Exp.t,
  prev_reuse_map: reuse_map,
  prev_probe_targets: EvalInfo.probe_targets,
  value: DHExp.t,
  state: 'state,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t('state) = {entries: Id.Map.t(entry('state))};

[@deriving (show({with_path: false}), sexp, yojson)]
type current('state) = {
  id: Id.t,
  state: 'state,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type outbox('state) = {
  completed: t('state),
  current: option(current('state)),
};

let empty: t('state) = {entries: Id.Map.empty};

let empty_outbox: outbox('state) = {
  completed: empty,
  current: None,
};

let outbox_of_completed = (completed: t('state)): outbox('state) => {
  completed,
  current: None,
};

let is_empty = (incr: t('state)): bool => Id.Map.is_empty(incr.entries);

let outbox_is_empty = (outbox: outbox('state)): bool =>
  is_empty(outbox.completed) && Option.is_none(outbox.current);

let add_entry =
    (id: Id.t, entry: entry('state), incr: t('state)): t('state) => {
  entries: Id.Map.add(id, entry, incr.entries),
};

let add_outbox_entry =
    (id: Id.t, entry: entry('state), outbox: outbox('state))
    : outbox('state) => {
  ...outbox,
  completed: add_entry(id, entry, outbox.completed),
};

let set_outbox_current =
    (~id: Id.t, ~state: 'state, outbox: outbox('state)): outbox('state) => {
  ...outbox,
  current:
    Some({
      id,
      state,
    }),
};

let add_stream = (stream: t('state), incr: t('state)): t('state) => {
  entries:
    Id.Map.union(
      (_, _old, new_) => Some(new_),
      incr.entries,
      stream.entries,
    ),
};

let merge_outbox =
    (stream: outbox('state), outbox: outbox('state)): outbox('state) => {
  completed: add_stream(stream.completed, outbox.completed),
  /* A slice that only finished completed entries (or only stepped through
   * non-program ids) may omit current. Keep the prior in-flight publish so
   * mid-stream UI state does not flicker away between slices. */
  current:
    switch (stream.current) {
    | Some(_) as current => current
    | None => outbox.current
    },
};

let copy_descendant_entries =
    (~root_id: Id.t, ~root: Exp.t, ~prev: t('state), incr: t('state))
    : t('state) => {
  let acc = ref(incr);
  let f_exp = (continue, e: Exp.t): Exp.t => {
    let sub_id = Exp.rep_id(e);
    if (!Id.equal(sub_id, root_id)) {
      switch (Id.Map.find_opt(sub_id, prev.entries)) {
      | Some(sub_entry) => acc := add_entry(sub_id, sub_entry, acc^)
      | None => ()
      };
    };
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, root);
  acc^;
};

/* Surface ids covered by cache entries: each entry short-circuits a subtree,
 * so expand via prev_elab rather than using only the map keys. Used by the
 * pending-eval worklist (to drop settled ids) and by the frozen debug tint
 * (to paint a reuse prediction). */
let visible_ids = (incr: t('state)): list(Id.t) => {
  let acc = ref([]);
  let collect_subtree = (root: Exp.t): unit => {
    let f_exp = (continue, e: Exp.t): Exp.t => {
      acc := [Exp.rep_id(e), ...acc^];
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, root);
    ();
  };
  Id.Map.iter((_, entry) => collect_subtree(entry.prev_elab), incr.entries);
  acc^;
};

/* Ids the UI should paint as "frozen" for a reuse plan / prediction. */
let frozen_ids = (~incr: t('state)): list(Id.t) => visible_ids(incr);

let equal_provenance = (a: provenance, b: provenance): bool =>
  Id.equal(a.source, b.source) && a.path == b.path && a.flag == b.flag;

let make_clean = (reuse_map: reuse_map): reuse_map =>
  Maps.StringMap.map(
    (prov: provenance) =>
      {
        ...prov,
        flag: Clean,
      },
    reuse_map,
  );

let equal_reuse_map = (a: reuse_map, b: reuse_map): bool =>
  Maps.StringMap.equal(equal_provenance, a, b);

/* `$hole` is a statics-only sentinel for unused-variable warnings. It is not
 * a runtime dependency, so it should not participate in reuse provenance. */
let is_runtime_dependency = (name: string): bool => name != "$hole";

let restrict_to_co_ctx = (reuse_map: reuse_map, co_ctx: CoCtx.t): reuse_map =>
  List.fold_left(
    (projected, (name, _)) =>
      if (!is_runtime_dependency(name)) {
        projected;
      } else {
        switch (Maps.StringMap.find_opt(name, reuse_map)) {
        | Some(prov) => Maps.StringMap.add(name, prov, projected)
        | None => projected
        };
      },
    empty_reuse_map,
    VarMap.to_list(co_ctx),
  );

let reuse_map_for_co_ctx =
    (reuse_map: reuse_map, co_ctx: CoCtx.t): option(reuse_map) =>
  List.fold_left(
    (acc, (name, _)) =>
      if (!is_runtime_dependency(name)) {
        acc;
      } else {
        switch (acc) {
        | None => None
        | Some(projected) =>
          switch (Maps.StringMap.find_opt(name, reuse_map)) {
          | Some(prov) => Some(Maps.StringMap.add(name, prov, projected))
          | None => None
          }
        };
      },
    Some(empty_reuse_map),
    VarMap.to_list(co_ctx),
  );

// For builtins
let clean_reuse_map_of_env = (env: Environment.t(Exp.t)): reuse_map =>
  env
  |> Environment.to_list
  |> List.fold_left(
       (acc, (name, _)) =>
         Maps.StringMap.add(
           name,
           {
             source: Id.invalid,
             path: [],
             flag: Clean,
           },
           acc,
         ),
       empty_reuse_map,
     );

let remove_pat_bindings = (pat: Pat.t, reuse_map: reuse_map): reuse_map =>
  List.fold_left(
    (acc, name) => Maps.StringMap.remove(name, acc),
    reuse_map,
    Pat.bound_vars(pat),
  );

let pat_label = (pat: Pat.t): option(string) =>
  switch (pat.term) {
  | Label(name) => Some(name)
  | _ => None
  };

/* Descending a tuple pattern extends the path with a step (recording WHERE
 * the binding sits in the cached value) and splits the flag (recording WHICH
 * parts of it are clean). The two are filled in independently: the path comes
 * from aP, the flag from aM. */
let pat_provenance = (~source_id: Id.t, ~flag: flag, pat: Pat.t): reuse_map => {
  let rec go =
          (path: list(projection), flag: flag, pat: Pat.t)
          : list((string, provenance)) =>
    switch (pat.term) {
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Invalid(_)
    | Atom(_)
    | Label(_)
    | ExplicitNonlabel
    | Constructor(_) => []
    | Var(name) => [
        (
          name,
          {
            source: source_id,
            path: List.rev(path),
            flag,
          },
        ),
      ]
    /* Transparent wrappers: same value, so same flag. */
    | Parens(p)
    | Projector(_, p) => go(path, flag, p)
    | Asc(p, _) => go([Ascribed, ...path], flag, p)
    | TupLabel(label, p) =>
      go([TupleLabel(pat_label(label)), ...path], flag, p)
    | Ap(ctr, p) =>
      switch (Pat.ctr_name(ctr)) {
      | Some(name) => go([ConstructorArg(name), ...path], opaque(flag), p)
      | None => go(path, opaque(flag), p)
      }
    | Tuple(ps) =>
      let arity = List.length(ps);
      ps
      |> List.mapi((i, p) =>
           go(
             [TupleIndex(arity, i), ...path],
             split(~arity, ~index=i, flag),
             p,
           )
         )
      |> List.flatten;
    /* Only tuples carry componentwise flags (see `exp_flag`), so list and
     * cons patterns take the conservative reading of a component flag. */
    | ListLit(ps) =>
      let arity = List.length(ps);
      ps
      |> List.mapi((i, p) =>
           go([ListIndex(arity, i), ...path], opaque(flag), p)
         )
      |> List.flatten;
    | Cons(hd, tl) =>
      go([ConsHead, ...path], opaque(flag), hd)
      @ go([ConsTail, ...path], opaque(flag), tl)
    };
  go([], flag, pat) |> List.to_seq |> Maps.StringMap.of_seq;
};

let with_pat_provenance =
    (~source_id: Id.t, ~flag: flag, pat: Pat.t, reuse_map: reuse_map)
    : reuse_map =>
  /* Domains are disjoint: remove_pat_bindings removes exactly the names
   * pat_provenance produces. Prefer the pattern's entry regardless. */
  Maps.StringMap.union(
    (_name, from_pat, _outer) => Some(from_pat),
    pat_provenance(~source_id, ~flag, pat),
    remove_pat_bindings(pat, reuse_map),
  );

/* The components of the tuple cached at `id`, when the previous run really
 * did cache a tuple of this arity there. `None` means no position of this
 * tuple can claim anything: there is nothing cached at this id, or what is
 * cached is not a tuple, or its arity differs -- and position j only names
 * the same slot in the two runs when the arities agree. */
let prev_tuple_components =
    (~prev: t('state), ~id: Id.t, es: list(Exp.t)): option(list(Exp.t)) =>
  switch (Id.Map.find_opt(id, prev.entries)) {
  | Some(entry) =>
    switch (entry.prev_elab.term) {
    | Tuple(prev_es) when List.length(prev_es) == List.length(es) =>
      Some(prev_es)
    | _ => None
    }
  | None => None
  };

/* Which parts of `e`'s value come from the cache.
 *
 * Without tuple flags this is the clean/dirty bit Hazel already used: the
 * whole value is clean exactly when the expression itself was re-used. With
 * them, section 8's Pair rule applies — a tuple is clean componentwise, so
 * editing one component leaves bindings projected from the others re-usable.
 *
 * A variable reports the flag of its binding, which is how partial
 * cleanliness reaches a `let (p, q) = z`: the flag was computed when `z` was
 * bound and is read back here rather than re-derived from the occurrence. */
let rec exp_flag =
        (
          ~tuple_flags: bool,
          ~prev: t('state),
          ~reused: Id.t => bool,
          ~reuse_map: reuse_map,
          e: Exp.t,
        )
        : flag =>
  if (reused(Exp.rep_id(e))) {
    Clean;
  } else if (!tuple_flags) {
    Dirty;
  } else {
    let recur = exp_flag(~tuple_flags, ~prev, ~reused, ~reuse_map);
    switch (e.term) {
    /* A tuple flag is anchored at the tuple's OWN id: each part of it claims
     * that that part of this value matches the corresponding part of the
     * value cached at uid(e). Position j of that cached value is the value
     * the cached component j evaluated to, so a clean component flag —
     * value_new(e_j) = value_prev(uid(e_j)) — is a statement about it
     * exactly when this run's component j still carries that same id. Where
     * the ids differ the position claims nothing, so an id-preserving edit
     * that drops, adds or permutes components never reports a part clean
     * against a cached value of a different shape. Deciding this per
     * component rather than for the tuple as a whole is what keeps the
     * motivating edit — one component of a tuple changed, minting a fresh id
     * for just that component — re-usable in the other components. Arity
     * must still match outright, or "position j" does not name the same slot
     * in the two runs. Component contents may of course differ. */
    | Tuple(es) =>
      switch (prev_tuple_components(~prev, ~id=Exp.rep_id(e), es)) {
      | None => Dirty
      | Some(prev_es) =>
        norm(
          List.map2(
            (prev_e, e) =>
              Id.equal(Exp.rep_id(prev_e), Exp.rep_id(e))
                ? recur(e) : Dirty,
            prev_es,
            es,
          ),
        )
      }
    | Parens(e) => recur(e)
    | Var(name) =>
      switch (Maps.StringMap.find_opt(name, reuse_map)) {
      | Some(prov) => prov.flag
      | None => Dirty
      }
    | _ => Dirty
    };
  };

let update_maps_after_binding =
    (~flag: flag, ~source_id: Id.t, pat: Pat.t, ~reuse_map: reuse_map)
    : reuse_map =>
  with_pat_provenance(~source_id, ~flag, pat, reuse_map);

let reuse_check =
    (
      ~call_stack: CallStack.t,
      ~prev: t('state),
      ~reuse_map: reuse_map,
      ~eval_info: EvalInfo.t,
      ~id: Id.t,
    )
    : option(entry('state)) => {
  open OptUtil.Syntax;

  let* () = OptUtil.some_if(call_stack == [] && !is_empty(prev), ());
  let* entry = Id.Map.find_opt(id, prev.entries);
  let* info = EvalInfo.find_opt(id, eval_info);

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
      EvalInfo.equal_probe_targets(
        entry.prev_probe_targets,
        info.probe_targets,
      ),
      (),
    );

  Some(entry);
};

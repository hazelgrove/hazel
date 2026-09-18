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

/* a2: the cache is keyed by a callstack and an id, kappa(c, uid(e)), rather
 * than by the id alone.
 *
 * It is a trie on the callstack, which is what section 6 suggests: the root
 * holds everything evaluated at the empty callstack, and each edge is the
 * application id of a call entered from the node above it. Two properties
 * follow from that shape, and both matter:
 *
 *  - `entries` still means exactly what it meant before a2 — the top-level
 *    cache — so the streaming collector, the UI's frozen tint and the
 *    benchmark's entry count all read the same thing they used to;
 *  - copying the region under a re-used call is one splice of a child node
 *    rather than a scan, which is the cost section 6 warns about.
 *
 * Callstacks are projected to frame ids, innermost first (which is what
 * CallStack.equal compares), so the path into the trie is the reverse of a
 * callstack: outermost call first. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t('state) = {
  entries: Id.Map.t(entry('state)),
  children: Id.Map.t(t('state)),
};

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

let empty: t('state) = {
  entries: Id.Map.empty,
  children: Id.Map.empty,
};

let empty_outbox: outbox('state) = {
  completed: empty,
  current: None,
};

let outbox_of_completed = (completed: t('state)): outbox('state) => {
  completed,
  current: None,
};

let is_empty = (incr: t('state)): bool =>
  Id.Map.is_empty(incr.entries) && Id.Map.is_empty(incr.children);

let outbox_is_empty = (outbox: outbox('state)): bool =>
  is_empty(outbox.completed) && Option.is_none(outbox.current);

let add_entry =
    (id: Id.t, entry: entry('state), incr: t('state)): t('state) => {
  ...incr,
  entries: Id.Map.add(id, entry, incr.entries),
};

/* The trie path for a callstack: outermost call first, so that a callstack's
 * prefixes are its enclosing calls. */
let path_of_call_stack_ids = (call_stack_ids: list(Id.t)): list(Id.t) =>
  List.rev(call_stack_ids);

let child = (frame: Id.t, incr: t('state)): t('state) =>
  switch (Id.Map.find_opt(frame, incr.children)) {
  | Some(child) => child
  | None => empty
  };

/* Rebuild the trie with `f` applied to the node at `path`. */
let rec update_node =
        (~path: list(Id.t), f: t('state) => t('state), incr: t('state))
        : t('state) =>
  switch (path) {
  | [] => f(incr)
  | [frame, ...rest] => {
      ...incr,
      children:
        Id.Map.add(
          frame,
          update_node(~path=rest, f, child(frame, incr)),
          incr.children,
        ),
    }
  };

let rec find_node =
        (~path: list(Id.t), incr: t('state)): option(t('state)) =>
  switch (path) {
  | [] => Some(incr)
  | [frame, ...rest] =>
    switch (Id.Map.find_opt(frame, incr.children)) {
    | Some(child) => find_node(~path=rest, child)
    | None => None
    }
  };

/* Record under the full key. The empty callstack lands at the root, so the
 * top-level cache is what it was before a2. */
let add_entry_at =
    (
      ~call_stack_ids: list(Id.t),
      ~id: Id.t,
      entry: entry('state),
      incr: t('state),
    )
    : t('state) =>
  update_node(
    ~path=path_of_call_stack_ids(call_stack_ids),
    add_entry(id, entry),
    incr,
  );

let find_at =
    (~call_stack_ids: list(Id.t), ~id: Id.t, incr: t('state))
    : option(entry('state)) =>
  switch (find_node(~path=path_of_call_stack_ids(call_stack_ids), incr)) {
  | Some(node) => Id.Map.find_opt(id, node.entries)
  | None => None
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

let rec add_stream = (stream: t('state), incr: t('state)): t('state) => {
  entries:
    Id.Map.union(
      (_, _old, new_) => Some(new_),
      incr.entries,
      stream.entries,
    ),
  children:
    Id.Map.union(
      (_, old, new_) => Some(add_stream(new_, old)),
      incr.children,
      stream.children,
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

/* On a re-use hit, carry the entries that belong to the skipped subtree into
 * the cache this run is building — they describe evaluations that did not
 * happen this time round but are still faithful.
 *
 * Getting this region wrong is a soundness bug rather than a slowdown: an
 * entry kept alive under a call that does not happen this run would be
 * re-used next run. Two parts belong to the subtree, and nothing else does:
 *
 *  - at the re-used expression's own callstack, the entries for ids inside
 *    the subtree;
 *  - under a child edge whose application id is inside the subtree, the whole
 *    child node — every one of those entries came from a call the subtree
 *    itself made, so it is spliced across in one operation.
 *
 * A child edge whose application id sits outside the subtree is a sibling
 * call, and is deliberately dropped. */
let copy_descendant_entries =
    (
      ~call_stack_ids: list(Id.t),
      ~root_id: Id.t,
      ~root: Exp.t,
      ~prev: t('state),
      incr: t('state),
    )
    : t('state) => {
  let path = path_of_call_stack_ids(call_stack_ids);
  switch (find_node(~path, prev)) {
  | None => incr
  | Some(prev_node) =>
    /* The root's own id counts as part of the subtree: if the re-used
     * expression is itself an application, the calls it makes are recorded
     * under a child edge carrying the root's id. */
    let sub_ids = ref(Id.Map.empty);
    let f_exp = (continue, e: Exp.t): Exp.t => {
      sub_ids := Id.Map.add(Exp.rep_id(e), (), sub_ids^);
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, root);
    let sub_ids = sub_ids^;

    let copy_into = (node: t('state)): t('state) => {
      let entries =
        Id.Map.fold(
          (sub_id, (), entries) =>
            if (Id.equal(sub_id, root_id)) {
              entries;
            } else {
              switch (Id.Map.find_opt(sub_id, prev_node.entries)) {
              | Some(sub_entry) => Id.Map.add(sub_id, sub_entry, entries)
              | None => entries
              };
            },
          sub_ids,
          node.entries,
        );
      let children =
        Id.Map.fold(
          (frame, prev_child, children) =>
            if (Id.Map.mem(frame, sub_ids)) {
              Id.Map.add(
                frame,
                switch (Id.Map.find_opt(frame, children)) {
                | Some(existing) => add_stream(existing, prev_child)
                | None => prev_child
                },
                children,
              );
            } else {
              children;
            },
          prev_node.children,
          node.children,
        );
      {
        entries,
        children,
      };
    };
    update_node(~path, copy_into, incr);
  };
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

/* a2's guard G(e, c): which expressions count as "top-level", and so are
 * worth a cache entry. Section 6 leaves G open, and this reading of it —
 * callstack depth alone, ignoring the expression — has no proof attached, so
 * it is one constant (Calculus.callstack_depth_limit) threaded in as a plain
 * int rather than a decision spread over the evaluator. A limit of 0 admits
 * only the empty callstack, which is section 6's theorem that an
 * empty-callstack guard recovers id-reuse; max_int is no guard at all. */
let cacheable = (~depth_limit: int, call_stack: CallStack.t): bool =>
  CallStack.depth_within(~limit=depth_limit, call_stack);

/* Crossing into a function body swaps the environment for the closure's, so
 * the caller's re-use map stops describing what the names in scope are bound
 * to. a2's App rule takes the body's rho from the closure (lambda is
 * rho-annotated in figure eval2); Hazel's closures carry no re-use map, so we
 * keep the part of the caller's that is still justified: a binding survives
 * the crossing when the body's environment binds that name to the same value
 * the caller's map was describing, which is what makes the caller's
 * provenance a true statement about the body's environment too.
 *
 * Sameness is physical equality first and Exp.fast_equal second — the same
 * value often is the same object (the closure's environment shares cells with
 * the caller's), but not when one side came out of the cache and the other
 * was rebuilt. fast_equal compares closures by environment id, so this stays
 * off the deep-closure-traversal path that clean/dirty flags exist to avoid.
 *
 * Dropping is the conservative side: a dropped binding is simply absent from
 * the map, and reuse_map_for_co_ctx then refuses re-use for any expression
 * that mentions it. */
let transport_across_env =
    (
      ~from_env: Environment.t(Exp.t),
      ~to_env: Environment.t(Exp.t),
      reuse_map: reuse_map,
    )
    : reuse_map =>
  Maps.StringMap.filter(
    (name, _) =>
      switch (
        Environment.lookup(from_env, name),
        Environment.lookup(to_env, name),
      ) {
      | (Some(before), Some(after)) =>
        before === after || Exp.fast_equal(before, after)
      | (Some(_) | None, _) => false
      },
    reuse_map,
  );

let reuse_check =
    (
      ~call_stack_ids: option(list(Id.t)),
      ~prev: t('state),
      ~reuse_map: reuse_map,
      ~eval_info: EvalInfo.t,
      ~id: Id.t,
    )
    : option(entry('state)) => {
  open OptUtil.Syntax;

  let* call_stack_ids = call_stack_ids;
  let* () = OptUtil.some_if(!is_empty(prev), ());
  let* entry = find_at(~call_stack_ids, ~id, prev);
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

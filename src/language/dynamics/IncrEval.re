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
  | Parts(list(flag))
  /* The formalism's injection flags, tup(f, dirty) for inl and tup(dirty, f)
   * for inr: the value is constructor `name` applied to a payload, in this
   * run AND in the cached one, and the inner flag says which parts of the
   * payload are clean. Hazel's constructors are named rather than left/right,
   * so the tag is the name rather than a side.
   *
   * Ctr(name, Dirty) is deliberately not Dirty: it still claims the tag,
   * which is what lets a `match` know which arm both runs took (the
   * formalism keeps tup(dirty, dirty) apart from dirty for the same reason).
   * Build it with `ctr`, never directly. */
  | Ctr(string, flag);

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

/* Build an injection flag, keeping flags normalized: a clean payload under a
 * tag that also agrees is a clean value, and has to be spelled Clean or the
 * re-use check's structural comparison against an all-clean map refuses it. */
let ctr = (name: string, payload: flag): flag =>
  switch (payload) {
  | Clean => Clean
  | payload => Ctr(name, payload)
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
  | Parts(_)
  | Ctr(_) => Dirty
  };

/* The flag of the payload of a value the flag `f` describes, as seen through
 * a pattern `name(p)` -- the formalism's fs.split.1 under caseL. An injection
 * flag for a DIFFERENT constructor describes a value this pattern cannot
 * have matched, so it says nothing about what the pattern bound. */
let split_ctr = (~name: string, f: flag): flag =>
  switch (f) {
  | Clean => Clean
  | Dirty => Dirty
  | Ctr(name', payload) when name' == name => payload
  | Ctr(_)
  | Parts(_) => Dirty
  };

/* Pass a flag through a pattern form that does not project componentwise.
 * A component-shaped flag carries no information about where such a form's
 * sub-values sit, so it degrades to dirty. */
let opaque = (f: flag): flag =>
  switch (f) {
  | Parts(_)
  | Ctr(_) => Dirty
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
 * callstack: outermost call first.
 *
 * `closure_maps` is the re-use map each closure in the cached values
 * captured, keyed by the id of the environment node it closed over (see
 * `closure_maps` below). Only the root's is ever non-empty. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t('state) = {
  entries: Id.Map.t(entry('state)),
  children: Id.Map.t(t('state)),
  closure_maps: Id.Map.t(reuse_map),
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
  closure_maps: Id.Map.empty,
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
  /* Empty but at a stream the client merges -- the evaluator's own merges
   * never carry closure maps -- so keep that case free. A node's map never
   * changes once recorded, so either side's is the same. */
  closure_maps:
    Id.Map.is_empty(stream.closure_maps)
      ? incr.closure_maps
      : Id.Map.union(
          (_, map, _) => Some(map),
          stream.closure_maps,
          incr.closure_maps,
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
        ...node,
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
    /* The formalism's case rules bind x to fs.split.1 with path [L]: the
     * payload's part of an injection flag, with a step recording that the
     * binding sits under the constructor. */
    | Ap(ctr, p) =>
      switch (Pat.ctr_name(ctr)) {
      | Some(name) =>
        go([ConstructorArg(name), ...path], split_ctr(~name, flag), p)
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
    /* Only tuples and constructors carry structured flags (see `exp_flag`),
     * so list and cons patterns take the conservative reading of one. */
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

/* These read the node the previous run cached at (callstack, id) -- the
 * formalism's kappa(c, i) -- so each guard below holds at the callstack the
 * node is running at, not only at the top level.
 *
 * The components of the tuple cached at `id`, when the previous run really
 * did cache a tuple of this arity there. `None` means no position of this
 * tuple can claim anything: there is nothing cached at this id, or what is
 * cached is not a tuple, or its arity differs -- and position j only names
 * the same slot in the two runs when the arities agree. */
let prev_tuple_components =
    (
      ~prev: t('state),
      ~call_stack_ids: list(Id.t),
      ~id: Id.t,
      es: list(Exp.t),
    )
    : option(list(Exp.t)) =>
  switch (find_at(~call_stack_ids, ~id, prev)) {
  | Some(entry) =>
    switch (entry.prev_elab.term) {
    | Tuple(prev_es) when List.length(prev_es) == List.length(es) =>
      Some(prev_es)
    | _ => None
    }
  | None => None
  };

/* The parenthesized sub-expression cached at `id`, when the previous run
 * really did cache a `Parens` there. `None` means this node can claim
 * nothing: nothing is cached at this id, or what is cached is some other
 * expression. */
let prev_parens_child =
    (~prev: t('state), ~call_stack_ids: list(Id.t), ~id: Id.t)
    : option(Exp.t) =>
  switch (find_at(~call_stack_ids, ~id, prev)) {
  | Some(entry) =>
    switch (entry.prev_elab.term) {
    | Parens(prev_e) => Some(prev_e)
    | _ => None
    }
  | None => None
  };

/* The cache entry at `id`, when the previous run really did cache the SAME
 * variable there. `None` means this occurrence can claim nothing about the
 * value cached at its id: nothing is cached there, or what is cached is some
 * other expression -- in particular a DIFFERENT NAME, which is what retyping
 * one character of an occurrence produces while the token, and so the id,
 * survives. */
let prev_var_entry =
    (~prev: t('state), ~call_stack_ids: list(Id.t), ~id: Id.t, name: string)
    : option(entry('state)) =>
  switch (find_at(~call_stack_ids, ~id, prev)) {
  | Some(entry) =>
    switch (entry.prev_elab.term) {
    | Var(prev_name) when prev_name == name => Some(entry)
    | _ => None
    }
  | None => None
  };

/* CleanAgree {name}: the map this run is evaluating under and the map the
 * cache entry was recorded under name the SAME BINDING for `name` -- same
 * binding site, same path into its value. Flags are deliberately not compared
 * (an entry's stored map is all-clean, see `make_clean`): the question is
 * "was this entry recorded for the binding the name denotes now?", not "has
 * nothing changed since?". */
let clean_agree_on = (name: string, a: reuse_map, b: reuse_map): bool =>
  switch (
    Maps.StringMap.find_opt(name, a),
    Maps.StringMap.find_opt(name, b),
  ) {
  | (Some(a), Some(b)) => Id.equal(a.source, b.source) && a.path == b.path
  | (None, None) => true
  | (Some(_), None)
  | (None, Some(_)) => false
  };

/* `$`-named constructors stand for indeterminate values (Unboxing never
 * matches them against a pattern), so no tag claim may be made for one. */
let is_tag_constructor = (name: string): bool =>
  !String.starts_with(~prefix="$", name);

/* The payload of the constructor application cached at `id`, when the
 * previous run really did cache an application of the SAME constructor
 * there -- the formalism's kappa(c, i) = <inl i ec, ...>, where the node's
 * kind is the tag. `None` means no tag claim can be made: nothing is cached
 * there, or what is cached is some other expression, in particular another
 * constructor, which is what retyping the constructor's name produces while
 * the application's id survives. */
let prev_ctr_payload =
    (~prev: t('state), ~call_stack_ids: list(Id.t), ~id: Id.t, name: string)
    : option(Exp.t) =>
  switch (find_at(~call_stack_ids, ~id, prev)) {
  | Some(entry) =>
    switch (entry.prev_elab.term) {
    | Ap(_, {term: Constructor(prev_name, _), _}, prev_arg)
        when prev_name == name =>
      Some(prev_arg)
    | _ => None
    }
  | None => None
  };

/* The scrutinee and rules of the `match` cached at `id`, when the previous
 * run really did cache a `match` there. */
let prev_match =
    (~prev: t('state), ~call_stack_ids: list(Id.t), ~id: Id.t)
    : option((Exp.t, list((Pat.t, Exp.t)))) =>
  switch (find_at(~call_stack_ids, ~id, prev)) {
  | Some(entry) =>
    switch (entry.prev_elab.term) {
    | Match(prev_scrut, prev_rules) => Some((prev_scrut, prev_rules))
    | _ => None
    }
  | None => None
  };

/* The condition and branches of the `if` cached at `id`, when the previous
 * run really did cache an `if` there. */
let prev_if =
    (~prev: t('state), ~call_stack_ids: list(Id.t), ~id: Id.t)
    : option((Exp.t, Exp.t, Exp.t)) =>
  switch (find_at(~call_stack_ids, ~id, prev)) {
  | Some(entry) =>
    switch (entry.prev_elab.term) {
    | If(prev_cond, prev_then, prev_else) =>
      Some((prev_cond, prev_then, prev_else))
    | _ => None
    }
  | None => None
  };

/* A pattern that matches every value, indeterminate ones included. */
let rec matches_anything = (p: Pat.t): bool =>
  switch (p.term) {
  | Parens(p)
  | Projector(_, p) => matches_anything(p)
  | Var(_)
  | Wild => true
  | _ => false
  };

type arm_verdict =
  | Accepts
  | Rejects;

/* What an arm's pattern does with a value whose tag is `name`, read off the
 * pattern alone, the way PatternMatch.match_pattern would decide it. Only
 * forms whose verdict does not depend on the payload get one: a pattern that
 * looks inside the payload (`A(0)`, `A((a, b))`) can go either way -- or be
 * indeterminate -- for a payload the tag flag says nothing about. */
let rec arm_verdict_on_tag = (~name: string, p: Pat.t): option(arm_verdict) =>
  switch (p.term) {
  | Parens(p)
  | Projector(_, p) => arm_verdict_on_tag(~name, p)
  | _ when matches_anything(p) => Some(Accepts)
  | Constructor(name', _) when name' != name => Some(Rejects)
  | Ap(ctr, payload) =>
    switch (Pat.ctr_name(ctr)) {
    | Some(name') when name' != name => Some(Rejects)
    | Some(_) when matches_anything(payload) => Some(Accepts)
    | Some(_)
    | None => None
    }
  | _ => None
  };

/* Whether arm `k`, the arm this run's `match` took, is also the arm the
 * cached run took. Binary inl/inr give the formalism this for free -- a
 * non-dirty scrutinee flag fixes the side -- but Hazel picks the first
 * matching arm over arbitrary patterns, so it is read off the scrutinee's
 * flag:
 *
 *  - Clean: the two scrutinees are the same value and matching is
 *    deterministic, so the same arm is chosen if the patterns up to and
 *    including k are unchanged.
 *  - Ctr(name, _): the two carry tag `name` but their payloads may differ,
 *    so in addition every arm before k must reject `name` outright and arm k
 *    must accept any payload under it -- or a payload this run did not see
 *    could have chosen differently.
 *  - Anything else fixes nothing. */
let same_arm_in_both_runs =
    (
      ~scrut_flag: flag,
      ~prev_rules: list((Pat.t, Exp.t)),
      ~rules: list((Pat.t, Exp.t)),
      k: int,
    )
    : bool => {
  let rec go = (i, rules, prev_rules) =>
    switch (rules, prev_rules) {
    | ([(p, _), ...rules], [(prev_p, _), ...prev_rules])
        when Pat.fast_equal(p, prev_p) =>
      let decided =
        switch (scrut_flag) {
        | Clean => true
        | Ctr(name, _) =>
          arm_verdict_on_tag(~name, p) == Some(i == k ? Accepts : Rejects)
        | Dirty
        | Parts(_) => false
        };
      decided && (i == k || go(i + 1, rules, prev_rules));
    | _ => false
    };
  go(0, rules, prev_rules);
};

/* Section 8's caseFlagL/caseFlagR, decided as the arm is chosen. A `match`'s
 * value is its taken arm's, so its flag is the arm body's: this returns the
 * body's id, and the flag is read from it once it has finished (see
 * `node_flag`). The claim is anchored at the node cached at THIS callstack:
 * a `match` over the same scrutinee node, which took the same arm
 * (`same_arm_in_both_runs`), whose body in that position has the same id
 * (atNode). The scrutinee's flag must not be dirty. `None` makes the match
 * Dirty. */
let case_continuation =
    (
      ~prev: t('state),
      ~call_stack_ids: list(Id.t),
      ~scrut_flag: flag,
      ~arm: int,
      e: Exp.t,
    )
    : option(Id.t) =>
  switch (e.term) {
  | Match(scrut, rules) when scrut_flag != Dirty =>
    switch (prev_match(~prev, ~call_stack_ids, ~id=Exp.rep_id(e))) {
    | Some((prev_scrut, prev_rules))
        when
          Id.equal(Exp.rep_id(prev_scrut), Exp.rep_id(scrut))
          && same_arm_in_both_runs(~scrut_flag, ~prev_rules, ~rules, arm) =>
      let (_, body) = List.nth(rules, arm);
      let (_, prev_body) = List.nth(prev_rules, arm);
      Id.equal(Exp.rep_id(prev_body), Exp.rep_id(body))
        ? Some(Exp.rep_id(body)) : None;
    | Some(_)
    | None => None
    }
  | _ => None
  };

/* Off by default: the formalism has no `if`. With it on, an `if` is the case
 * rule's Clean reading -- a clean condition is the same boolean in both
 * runs, so the same branch was taken -- anchored like a `match`: the cached
 * node at this callstack is an `if` over the same condition node whose
 * taken branch has the same id. Nothing in the Lean development proves it. */
let if_flags = false;

/* `case_continuation` for an `if` that took its `then` branch when
 * `took_then`. `None` makes the `if` Dirty. */
let if_continuation =
    (
      ~prev: t('state),
      ~call_stack_ids: list(Id.t),
      ~cond_flag: flag,
      ~took_then: bool,
      e: Exp.t,
    )
    : option(Id.t) =>
  switch (e.term) {
  | If(cond, then_, else_) when if_flags && cond_flag == Clean =>
    switch (prev_if(~prev, ~call_stack_ids, ~id=Exp.rep_id(e))) {
    | Some((prev_cond, prev_then, prev_else))
        when Id.equal(Exp.rep_id(prev_cond), Exp.rep_id(cond)) =>
      let (prev_branch, branch) =
        took_then ? (prev_then, then_) : (prev_else, else_);
      Id.equal(Exp.rep_id(prev_branch), Exp.rep_id(branch))
        ? Some(Exp.rep_id(branch)) : None;
    | Some(_)
    | None => None
    }
  | _ => None
  };

/* SEAM: how a node's flag travels from where it is computed to where it is
 * read. The formalism's Eval1 RETURNS the flag beside the value, computed at
 * the callstack the node runs at. Here the evaluator records it, as each
 * node finishes at a cacheable callstack, in a table for the run keyed like
 * the cache, by (callstack, id), and readers look it up. That is the same
 * flow as long as every read comes after the evaluation it is about and
 * nothing re-evaluates that key in between, and the only readers are:
 *
 *  - a node as it finishes, reading the children it evaluated inside its
 *    own step, or the arm it continued into (`node_flag`);
 *  - a binding, reading the right-hand side, argument or scrutinee the
 *    step that makes the binding has just evaluated (see
 *    Evaluator.binding_flag).
 *
 * One run CAN evaluate a key twice: a delegating administrative step
 * re-evaluates its redex under its own id, nested inside the first
 * evaluation, and a fixpoint reached through a variable is unwrapped afresh
 * at each use, re-evaluating its body's ids. In both the later write is the
 * evaluation the reader just waited on, and each write is a sound claim
 * about the value it was computed with. Dirty is the absence of an entry and
 * recording Dirty REMOVES one, so a stale clean flag from an earlier
 * evaluation of the key cannot outlive a dirty re-evaluation of it. */
type flags = Hashtbl.t((list(Id.t), Id.t), flag);

let new_flags = (): flags => Hashtbl.create(256);

let record_flag =
    (flags: flags, ~call_stack_ids: list(Id.t), ~id: Id.t, flag: flag): unit =>
  switch (flag) {
  | Dirty => Hashtbl.remove(flags, (call_stack_ids, id))
  | flag => Hashtbl.replace(flags, (call_stack_ids, id), flag)
  };

let flag_at = (flags: flags, ~call_stack_ids: list(Id.t), ~id: Id.t): flag =>
  switch (Hashtbl.find_opt(flags, (call_stack_ids, id))) {
  | Some(flag) => flag
  | None => Dirty
  };

/* Which parts of `e`'s value come from the cache, once `e` has finished at
 * `call_stack_ids` WITHOUT a cache hit there (a hit is Clean, the
 * formalism's reuse rule). Every guard reads the node the previous run
 * cached at this same callstack, kappa(c, uid(e)), and every child's flag is
 * the one that child actually finished with at this callstack.
 *
 * With tuple flags off this is never called and a miss is Dirty: the
 * clean/dirty bit Hazel already used. With them, section 8's Pair rule
 * applies -- a tuple is clean componentwise, so editing one component leaves
 * bindings projected from the others re-usable -- and so do its injection
 * and case rules: a constructor application keeps its payload's flag under
 * its tag, and a `match` that took the same arm as the cached run reports
 * that arm's flag.
 *
 * A variable reports the flag of its binding, which is how partial
 * cleanliness reaches a `let (p, q) = z`: the flag was recorded when `z` was
 * bound and is read back here -- but only once the occurrence's own cache
 * entry says the flag is about the same occurrence of the same binding (see
 * the `Var` case). */
let node_flag =
    (
      ~prev: t('state),
      ~flags: flags,
      ~call_stack_ids: list(Id.t),
      ~reuse_map: reuse_map,
      ~continued_into: option(Id.t),
      e: Exp.t,
    )
    : flag => {
  let child = (c: Exp.t) =>
    flag_at(flags, ~call_stack_ids, ~id=Exp.rep_id(c));
  let id = Exp.rep_id(e);
  switch (e.term) {
  /* A tuple flag is anchored at the tuple's OWN id: each part of it claims
   * that that part of this value matches the corresponding part of the
   * value cached at uid(e). Position j of that cached value is the value
   * the cached component j evaluated to, so a clean component flag —
   * value_new(e_j) = value_prev(c, uid(e_j)) — is a statement about it
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
    switch (prev_tuple_components(~prev, ~call_stack_ids, ~id, es)) {
    | None => Dirty
    | Some(prev_es) =>
      norm(
        List.map2(
          (prev_e, e) =>
            Id.equal(Exp.rep_id(prev_e), Exp.rep_id(e)) ? child(e) : Dirty,
          prev_es,
          es,
        ),
      )
    }
  /* `Parens` is `Tuple` with one component: the inner flag is a claim about
   * uid(e_inner), and passing it up unchanged restates it about uid(e).
   * That transports only while the cache's node at uid(e) is a `Parens`
   * over THAT SAME child -- its recorded value is then the recorded value
   * of the child. An id-preserving edit that swaps the child (deleting the
   * `-` of `(-k)` leaves the parens' id, and k's, in place while the node
   * between them goes away) otherwise reports the old child's value clean
   * against the new one. */
  | Parens(inner) =>
    switch (prev_parens_child(~prev, ~call_stack_ids, ~id)) {
    | Some(prev_inner)
        when Id.equal(Exp.rep_id(prev_inner), Exp.rep_id(inner)) =>
      child(inner)
    | Some(_)
    | None => Dirty
    }
  /* A variable's flag is its BINDING's flag, but that is a claim about the
   * binding's node; reporting it here turns it into a claim about this
   * OCCURRENCE's node. Section 5's varFlag rule therefore asks for two
   * further things before the flag may be carried, and both are load
   * bearing:
   *
   *  - kappa(c, uid(e)) = <var uid(e) x, rho0, w>: the cache holds an entry at
   *    THIS occurrence's id, and the expression recorded there was the same
   *    variable. This is the `Tuple` guard again. Retyping one character of
   *    a name repoints the occurrence at a different binding while the
   *    token, and so the id, survives; without this the newly denoted
   *    binding's (clean) flag licenses serving the OLD variable's cached
   *    value.
   *  - CleanAgree [x] rho rho0: the entry was recorded for the same
   *    binding. The name and the id together still do not pin one down --
   *    inserting a `let x = ...` between binder and occurrence reshadows
   *    the name without touching either.
   *
   * With neither premise available the plain `var` rule applies and the
   * occurrence is dirty. */
  | Var(name) =>
    switch (
      Maps.StringMap.find_opt(name, reuse_map),
      prev_var_entry(~prev, ~call_stack_ids, ~id, name),
    ) {
    | (Some(prov), Some(entry))
        when clean_agree_on(name, reuse_map, entry.prev_reuse_map) =>
      prov.flag
    | (Some(_) | None, Some(_) | None) => Dirty
    }
  /* Section 8's inlFlag/inrFlag, for named constructors: the payload's
   * flag survives, under the tag. Both guards are the `Tuple` ones again.
   * The cached node at uid(e) must apply the SAME constructor, or its
   * value carries another tag -- retyping the name keeps the id -- and the
   * claim that both runs share a tag, which `match` arms are chosen by, is
   * false. And the payload's flag is a claim about uid(arg), so it is
   * carried only while the cached node's payload has that id (atNode);
   * otherwise just the tag is. */
  | Ap(_, {term: Constructor(name, _), _}, arg)
      when is_tag_constructor(name) =>
    switch (prev_ctr_payload(~prev, ~call_stack_ids, ~id, name)) {
    | Some(prev_arg) =>
      ctr(
        name,
        Id.equal(Exp.rep_id(prev_arg), Exp.rep_id(arg))
          ? child(arg) : Dirty,
      )
    | None => Dirty
    }
  /* The arm (or branch) this node continued into, chosen and anchored as
   * its step was taken (`case_continuation`, `if_continuation`). */
  | Match(_)
  | If(_) =>
    switch (continued_into) {
    | Some(id) => flag_at(flags, ~call_stack_ids, ~id)
    | None => Dirty
    }
  | _ => Dirty
  };
};

/* The pre-pass's guess at a binding's flag, made before anything has run --
 * clean exactly when the expression is predicted to be re-used. Only the
 * UI's re-use plan still runs that pass; the evaluator reads the flag each
 * node actually finished with instead (`node_flag`). */
let exp_flag = (~reused: Id.t => bool, e: Exp.t): flag =>
  reused(Exp.rep_id(e)) ? Clean : Dirty;

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

/* The re-use map each closure captured.
 *
 * The formalism's closures carry it: `closure (env : ...) (reuse : ReuseMap)
 * x body`, and the App rule evaluates the body under that captured map,
 * never the caller's. Hazel's runtime closures carry the environment only,
 * so the map is kept beside them, keyed by the id of the environment node a
 * closure closes over: an environment and its map are extended in lockstep
 * -- the formalism's `sigma.set x v` beside `rho.set x (...)` -- so a node
 * names one map. A lookup that misses is a bug, never a fallback: there is
 * no sound map to guess (see ClosureMapMissing).
 *
 * A node's map comes from one of three places, tried in this order:
 *
 *  - recorded this run, by the step that minted the node. Inside the
 *    guard the map is being maintained anyway and is stored whole
 *    (`record_captured`). Beyond it nothing is maintained and every flag is
 *    Dirty, so the step stores only what it bound, over which node, in O(1)
 *    (`record_bindings`); the map is built from that only if a closure over
 *    the node is called back inside the guard, and kept once built.
 *  - the initial environment, each node seeded with the clean map of the
 *    bindings at and below it, as the top-level map is seeded.
 *  - carried from the previous run. A closure that comes out of the cache
 *    closed over a node minted then, and the formalism hands it back with
 *    the map it captured then. `carry` keeps exactly the maps of closures
 *    reachable from the new cache's values, so what is carried is bounded
 *    by what is cached.
 *
 * All of it only under callstack keys: below the top level nothing else is
 * cached, so without them no closure's map is ever read. */
exception
  ClosureMapMissing({
    env_node: Id.t,
    closure: option(string),
  });

type binding = {
  pat: Pat.t,
  source_id: Id.t,
  flag,
};

type closure_node =
  | Captured(reuse_map)
  /* The map of `base` (the empty map for the empty environment) extended
   * by `bindings`, in order -- what update_maps_after_binding would have
   * built had the map been maintained. */
  | Binds({
      base: option(Id.t),
      bindings: list(binding),
    });

type closure_maps = {
  recorded: Hashtbl.t(Id.t, closure_node),
  /* Recorded beyond the guard, newest first, and not yet in `recorded`.
   * A step there only conses onto this; it is moved into `recorded` the
   * first time a lookup finds nothing anywhere else. */
  beyond: ref(list((Id.t, closure_node))),
  seeded: Id.Map.t(reuse_map),
  carried: Id.Map.t(reuse_map),
  /* Environment nodes already walked for the streaming outbox this run. */
  streamed: Hashtbl.t(Id.t, unit),
  /* markClean of a node's map, for `refreshed_closure_map`. */
  refreshed: Hashtbl.t(Id.t, reuse_map),
};

/* The top node of `env`, or None for the empty environment. */
let env_node = (env: Environment.t(Exp.t)): option(Id.t) =>
  switch (env) {
  | Empty => None
  | E(e) => Some(e.id)
  };

/* The environment `n` nodes down the spine of `env`. */
let rec drop_env = (n: int, env: Environment.t(Exp.t)): Environment.t(Exp.t) =>
  switch (n, env) {
  | (0, _)
  | (_, Empty) => env
  | (_, E(e)) => drop_env(n - 1, e.prev_env)
  };

/* Every node of an initial environment, mapped to the clean map of the
 * bindings at and below it. The same environment value is handed to every
 * run (Builtins.env_init), so this is built once per environment. */
let seed_memo: ref(option((Environment.t(Exp.t), Id.Map.t(reuse_map)))) =
  ref(None);

let seed_maps = (env: Environment.t(Exp.t)): Id.Map.t(reuse_map) =>
  switch (seed_memo^) {
  | Some((seeded_env, seeds)) when seeded_env === env => seeds
  | _ =>
    let rec go = (env: Environment.t(Exp.t)) =>
      switch (env) {
      | Empty => (empty_reuse_map, Id.Map.empty)
      | E({id, binding: (name, _), prev_env, _}) =>
        let (below, seeds) = go(prev_env);
        /* As clean_reuse_map_of_env, so the top node's map is the
         * top-level map exactly. */
        let map =
          Maps.StringMap.add(
            name,
            {
              source: Id.invalid,
              path: [],
              flag: Clean,
            },
            below,
          );
        (map, Id.Map.add(id, map, seeds));
      };
    let (_, seeds) = go(env);
    seed_memo := Some((env, seeds));
    seeds;
  };

/* `top_map` is the map the evaluation starts under. When it is the one
 * derived from `env` every node of `env` is seeded; a map supplied from
 * outside describes the whole environment only, so it seeds the top node
 * alone. */
let new_closure_maps =
    (
      ~prev: t('state),
      ~env: Environment.t(Exp.t),
      ~top_map: option(reuse_map),
    )
    : closure_maps => {
  recorded: Hashtbl.create(1024),
  beyond: ref([]),
  seeded:
    switch (top_map, env_node(env)) {
    | (None, _) => seed_maps(env)
    | (Some(map), Some(id)) => Id.Map.singleton(id, map)
    | (Some(_), None) => Id.Map.empty
    },
  carried: prev.closure_maps,
  streamed: Hashtbl.create(64),
  refreshed: Hashtbl.create(64),
};

/* First write wins: a step that mints no node presents an environment some
 * earlier step already described. */
let record = (maps: closure_maps, env: Environment.t(Exp.t), node) =>
  switch (env) {
  | Empty => ()
  | E(e) =>
    if (!Hashtbl.mem(maps.recorded, e.id)) {
      Hashtbl.replace(maps.recorded, e.id, node);
    }
  };

/* Inside the guard: the map is maintained, so store it whole. */
let record_captured =
    (maps: closure_maps, ~env: Environment.t(Exp.t), map: reuse_map) =>
  record(maps, env, Captured(map));

/* Beyond the guard: the step minted the top `added` nodes of `env` by
 * binding `bindings`, over the environment below them. No lookup and no
 * hashing: a node is minted once, so there is nothing to check. */
let record_bindings =
    (
      maps: closure_maps,
      ~env: Environment.t(Exp.t),
      ~added: int,
      bindings: list(binding),
    ) =>
  switch (env) {
  | Empty => ()
  | E(e) =>
    maps.beyond :=
      [
        (
          e.id,
          Binds({
            base: env_node(drop_env(added, env)),
            bindings,
          }),
        ),
        ...maps.beyond^,
      ]
  };

/* Move what was recorded beyond the guard into `recorded`. Oldest first,
 * so that, as in `record`, a node's first record is the one kept. */
let index_beyond = (maps: closure_maps): bool =>
  switch (maps.beyond^) {
  | [] => false
  | beyond =>
    maps.beyond := [];
    List.iter(
      ((id, node)) =>
        if (!Hashtbl.mem(maps.recorded, id)) {
          Hashtbl.replace(maps.recorded, id, node);
        },
      List.rev(beyond),
    );
    true;
  };

/* The map of the node `id`, building it from recorded bindings if need be.
 * Iterative, since a chain of Binds is as long as the recursion that made
 * it, and memoizing every node built on the way. */
let map_of_node =
    (maps: closure_maps, ~closure: option(string), id: Id.t): reuse_map => {
  let rec known = id =>
    switch (Hashtbl.find_opt(maps.recorded, id)) {
    | Some(Captured(map)) => Some(`Map(map))
    | Some(Binds({base, bindings})) => Some(`Binds((base, bindings)))
    | None =>
      switch (Id.Map.find_opt(id, maps.seeded)) {
      | Some(map) => Some(`Map(map))
      | None =>
        switch (Id.Map.find_opt(id, maps.carried)) {
        | Some(map) => Some(`Map(map))
        | None => index_beyond(maps) ? known(id) : None
        }
      }
    };
  /* Walk down to a node whose map is known, stacking the bindings above. */
  let rec down = (id, pending) =>
    switch (known(id)) {
    | Some(`Map(map)) => (map, pending)
    | Some(`Binds(None, bindings)) => (
        empty_reuse_map,
        [(id, bindings), ...pending],
      )
    | Some(`Binds(Some(base), bindings)) =>
      down(base, [(id, bindings), ...pending])
    | None =>
      raise(
        ClosureMapMissing({
          env_node: id,
          closure,
        }),
      )
    };
  let (map, pending) = down(id, []);
  List.fold_left(
    (map, (id, bindings)) => {
      let map =
        List.fold_left(
          (map, {pat, source_id, flag}) =>
            with_pat_provenance(~source_id, ~flag, pat, map),
          map,
          bindings,
        );
      Hashtbl.replace(maps.recorded, id, Captured(map));
      map;
    },
    map,
    pending,
  );
};

/* The map a closure over `env` captured. */
let closure_map =
    (
      maps: closure_maps,
      ~closure: option(string),
      env: Environment.t(Exp.t),
    )
    : reuse_map =>
  switch (env) {
  /* Nothing bound, so nothing for a map to say. */
  | Empty => empty_reuse_map
  | E(e) => map_of_node(maps, ~closure, e.id)
  };

/* make_clean, rebuilding only the entries that are not already clean --
 * in practice few: the builtins, and most bindings once a program has been
 * run once, are clean -- and the map itself when there are none. */
let mark_clean = (map: reuse_map): reuse_map =>
  Maps.StringMap.fold(
    (name, prov: provenance, cleaned) =>
      prov.flag == Clean
        ? cleaned
        : Maps.StringMap.add(
            name,
            {
              ...prov,
              flag: Clean,
            },
            cleaned,
          ),
    map,
    map,
  );

/* The map a call evaluates the body of a closure over `env` under, when
 * the function expression's flag is Clean: markClean of the captured map,
 * the formalism's App rule (and appFix, before the function's own binding
 * is added).
 *
 * A clean flag on the function expression says its value is the value
 * cached for it at this callstack last run: the same body, the same values
 * for its free variables, bound at the same sources and paths, whatever
 * their flags (Veq1.clo). That is the closure applied here last run, so
 * the entries its body left at this callstack were recorded under
 * markClean of this very map and computed from these very values. Its
 * captured flags, by contrast, say only whether those values had changed
 * when the closure was CREATED -- possibly runs ago, for a closure that
 * has come out of the cache since -- which is not what re-using inside the
 * body needs to know.
 *
 * Memoized per node: a closure called repeatedly at clean call sites asks
 * for the same refreshed map each time. */
let refreshed_closure_map =
    (
      maps: closure_maps,
      ~closure: option(string),
      env: Environment.t(Exp.t),
    )
    : reuse_map =>
  switch (env) {
  | Empty => empty_reuse_map
  | E(e) =>
    switch (Hashtbl.find_opt(maps.refreshed, e.id)) {
    | Some(map) => map
    | None =>
      let map = mark_clean(map_of_node(maps, ~closure, e.id));
      Hashtbl.replace(maps.refreshed, e.id, map);
      map;
    }
  };

/* Call `f` on the environment of every closure in a value, through
 * whatever contains it -- data structures, and the environments of other
 * closures, each visited once. A function body is code and holds no
 * runtime closure, so it is not entered. */
let iter_closure_envs =
    (~visited: Hashtbl.t(Id.t, unit), f: Environment.t(Exp.t) => unit)
    : (Exp.t => unit) => {
  let rec walk_env = (env: Environment.t(Exp.t)) =>
    switch (env) {
    | Empty => ()
    | E(e) =>
      if (!Hashtbl.mem(visited, e.id)) {
        Hashtbl.replace(visited, e.id, ());
        value(snd(e.binding));
        walk_env(e.prev_env);
      }
    }
  and value = (v: Exp.t) =>
    switch (v.term) {
    | Atom(_)
    | Var(_)
    | Label(_)
    | Constructor(_)
    | BuiltinFun(_)
    | EmptyHole
    | Fun(_)
    | TypFun(_) => ()
    | Closure(env, body) =>
      f(env);
      walk_env(env);
      switch (body.term) {
      | Fun(_)
      | TypFun(_) => ()
      | _ => value(body)
      };
    | Tuple(vs)
    | ListLit(vs) => List.iter(value, vs)
    | Cons(a, b)
    | Ap(_, a, b)
    | TupLabel(a, b) =>
      value(a);
      value(b);
    | Parens(a)
    | Asc(a, _) => value(a)
    /* Unwrapping a fixpoint extends the environment it is unwrapped in,
     * not this one, so nothing is evaluated under it; walked anyway, so
     * that nothing stored in a value is missed. */
    | FixF(_, _, Some(fix_env)) => walk_env(fix_env)
    | FixF(_, _, None) => ()
    | _ =>
      let _ =
        TermBase.Exp.map_term(
          ~f_exp=
            (continue, e: Exp.t) =>
              switch (e.term) {
              | Closure(_) =>
                value(e);
                e;
              | _ => continue(e)
              },
          v,
        );
      ();
    };
  value;
};

/* `carried` extended with the map of every closure reachable from `v`. */
let carry_value =
    (
      maps: closure_maps,
      ~visited: Hashtbl.t(Id.t, unit),
      carried: Id.Map.t(reuse_map),
      v: Exp.t,
    )
    : Id.Map.t(reuse_map) => {
  let carried = ref(carried);
  iter_closure_envs(
    ~visited,
    env =>
      switch (env) {
      | Empty => ()
      | E(e) =>
        if (!Id.Map.mem(e.id, carried^)) {
          carried :=
            Id.Map.add(
              e.id,
              map_of_node(maps, ~closure=None, e.id),
              carried^,
            );
        }
      },
    v,
  );
  carried^;
};

/* The closure maps the next run needs: those of every closure reachable
 * from a value in `incr`, the cache this run built. A closure the next run
 * can obtain without evaluating comes out of that cache, and nothing else
 * can carry over, so the carried table is pruned to the live closures every
 * run and does not grow with the number of runs. */
let carry = (maps: closure_maps, incr: t('state)): t('state) => {
  let visited = Hashtbl.create(256);
  let rec walk = (carried, incr: t('state)) =>
    Id.Map.fold(
      (_, child, carried) => walk(carried, child),
      incr.children,
      Id.Map.fold(
        (_, entry: entry('state), carried) =>
          carry_value(maps, ~visited, carried, entry.value),
        incr.entries,
        carried,
      ),
    );
  {
    ...incr,
    closure_maps: walk(Id.Map.empty, incr),
  };
};

/* A client still waiting on a result re-runs from the entries streamed so
 * far, so each streamed entry brings the maps of the closures in it. */
let carry_streamed =
    (maps: closure_maps, v: Exp.t, outbox: outbox('state)): outbox('state) => {
  ...outbox,
  completed: {
    ...outbox.completed,
    closure_maps:
      carry_value(
        maps,
        ~visited=maps.streamed,
        outbox.completed.closure_maps,
        v,
      ),
  },
};

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

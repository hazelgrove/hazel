open Util;

/* Cache used by the incremental evaluator to skip re-evaluating sub-expressions
 * whose elaboration and free-variable dependencies haven't changed since the
 * previous run. Function bodies are a "deferred boundary": ids inside a closure
 * body are never cached (so calling a function twice re-runs the body). */

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  prev_elab: Exp.t,
  value: DHExp.t,
  slice: StateSlice.t,
  /* Probe targets active when `slice` was captured. If a new probe target
   * lands inside this subtree on a later run, reuse must be invalidated so
   * the new target records samples. */
  targets_snapshot: Sample.targets,
};

/* Minimal per-id projection of the statics info_map for the incremental
 * evaluator. Ships across the web-worker boundary; the full StaticsBase.Map.t
 * embeds LivelitCtx closures that postMessage's structured-clone rejects. */
module InfoSlice = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry = {
    elab_term: Exp.t,
    co_ctx: CoCtx.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(entry);

  let empty: t = Id.Map.empty;

  let find_opt = Id.Map.find_opt;

  let of_info_map = (info_map: StaticsBase.Map.t): t =>
    Id.Map.filter_map(
      (_id, info) =>
        switch (info) {
        | Info.InfoExp({elab_term, co_ctx, _}) =>
          Some({
            elab_term,
            co_ctx,
          })
        | _ => None
        },
      info_map,
    );
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

/* The set of ids the UI should paint as "frozen" this run.
 *
 * `reused` only contains ids the evaluator explicitly short-circuited
 * via `reuse_check`. When that fires at id X the evaluator returns the
 * cached value without descending into X's subtree, so descendant ids
 * end up in neither `reused` nor `recalculated` — leaving them un-tinted
 * even though they're effectively frozen.
 *
 * This is especially visible across module boundaries: `ExpandModule.expand`
 * desugars `{ let bb=12; let x=fib(bb); ... }` into a chain
 * `Let(bb,12, Let(x,..., Let(...,Tuple(...))))`. Surface-sibling ModLets
 * become elab-ancestors of one another, so reuse at the outermost wrapper
 * short-circuits all the inner ones; without this closure they'd appear
 * untinted in the editor.
 *
 * The walk is over the elab tree (not the surface tree), so the parent-
 * reversal introduced by module expansion is handled naturally —
 * surface-siblings of the cached id appear as elab-descendants and
 * therefore land in the frozen set. Fresh ids minted during elaboration
 * (e.g. `build_labeled_tuple`'s inner Tuple children) are also collected
 * but have no surface tile, so the renderer silently produces no
 * decoration for them. */
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

/* Names that a Let/FixF binder's rhs has dirtied on the current run: if the
 * rhs produced a value different from its cached one, the pattern's bound
 * vars become dirty inside the body.
 *
 * We compare values at ALL descendant ids of `rhs`, not just rhs's outer id:
 * the elaborator's tuple-alignment pass (LabeledTupleHelpers.align_exp) mints
 * fresh outer ids for re-assembled tuples, so those never land in info_slice
 * or the cache. Stable child ids (e.g. Atom leaves) still carry the signal. */
let newly_dirty_vars =
    (~prev: t, ~curr: t, pat: Pat.t, rhs: DHExp.t): list(Var.t) => {
  let id_value_changed = (id: Id.t): bool =>
    switch (
      Id.Map.find_opt(id, prev.entries),
      Id.Map.find_opt(id, curr.entries),
    ) {
    | (Some(p), Some(n)) => !Exp.fast_equal(p.value, n.value)
    /* Cached in prev but not in curr: structural change; treat as dirty. */
    | (Some(_), None) => true
    | (None, _) => false
    };
  let rhs_ids = {
    let ids = ref([]);
    let f_exp = (continue, e: Exp.t) => {
      ids := [DHExp.rep_id(e), ...ids^];
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, rhs);
    ids^;
  };
  List.exists(id_value_changed, rhs_ids) ? Pat.bound_vars(pat) : [];
};

/* A co_ctx is clean iff none of its free-variable uses refer to a dirty name.
 * A `$hole` in the co_ctx widens this: any dirty name anywhere kills reuse,
 * since the hole could, once filled, capture any name in scope. */
let co_ctx_clean = (~dirty_names: list(Var.t), co_ctx: CoCtx.t): bool =>
  if (CoCtx.contains_hole(co_ctx) && dirty_names != []) {
    false;
  } else {
    !List.exists(((name, _)) => List.mem(name, dirty_names), co_ctx);
  };

/* Decide whether `id`'s cached entry in `prev` can be reused. `dirty_names`
 * is the set of names marked dirty on the path down to this evaluation
 * (from binders whose rhs differed from their cached value); it's threaded
 * as an explicit call parameter rather than accumulated globally, so a
 * shadowed rebinding of `x` doesn't invalidate outer-`x` consumers. */
let reuse_check =
    (
      ~prev: t,
      ~dirty_names: list(Var.t),
      ~info_slice: InfoSlice.t,
      ~current_targets: Sample.targets,
      ~id: Id.t,
      ~curr_elab: Exp.t,
    )
    : option(entry) =>
  switch (Id.Map.find_opt(id, prev.entries)) {
  | None => None
  | Some(entry) =>
    let info = InfoSlice.find_opt(id, info_slice);
    let elab_same =
      switch (info) {
      | Some({elab_term, _}) => Exp.fast_equal(entry.prev_elab, elab_term)
      | None => Exp.fast_equal(entry.prev_elab, curr_elab)
      };
    let co_ctx =
      switch (info) {
      | Some({co_ctx, _}) => co_ctx
      | None => CoCtx.empty
      };
    /* Targets stability: if any id covered by the cached slice has a
     * different capture_spec now (including a newly-added target), we must
     * re-evaluate so the new target actually records samples. */
    let subtree_ids = [
      id,
      ...Id.Map.bindings(entry.slice.probes) |> List.map(fst),
    ];
    let targets_stable =
      List.for_all(
        sid =>
          switch (
            Id.Map.find_opt(sid, entry.targets_snapshot),
            Id.Map.find_opt(sid, current_targets),
          ) {
          | (None, None) => true
          | (Some(a), Some(b)) => Sample.equal_capture_spec(a, b)
          | _ => false
          },
        subtree_ids,
      );
    if (elab_same && co_ctx_clean(~dirty_names, co_ctx) && targets_stable) {
      Some(entry);
    } else {
      None;
    };
  };

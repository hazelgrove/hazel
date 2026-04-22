open Util;

/* Cache used by the incremental evaluator to skip re-evaluating sub-expressions
 * whose elaboration and free-variable dependencies haven't changed since the
 * previous run. Function bodies are a "deferred boundary": ids inside a closure
 * body are never cached (so calling a function twice re-runs the body). */

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  prev_elab: Exp.t,
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

module EvalInfoMap = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry = {
    elab_term: Exp.t,
    co_ctx: CoCtx.t,
    /* See `prev_probe_targets` above — None under `probe_all`. */
    probe_targets: option(SubexpProbeTargets.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(entry);

  let empty: t = Id.Map.empty;

  let find_opt = Id.Map.find_opt;

  let of_info_map = (~probe_all: bool, info_map: StaticsBase.Map.t): t =>
    Id.Map.filter_map(
      (_id, info) =>
        switch (info) {
        | Info.InfoExp({elab_term, co_ctx, probe_targets, _}) =>
          Some({
            elab_term,
            co_ctx,
            probe_targets: probe_all ? None : Some(probe_targets),
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

let reuse_check =
    (
      ~call_stack: Sample.call_stack,
      ~prev: t,
      ~dirty_names: list(Var.t),
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

  let co_ctx = info.co_ctx;
  let* () =
    OptUtil.some_if(
      !List.exists(((name, _)) => List.mem(name, dirty_names), co_ctx),
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

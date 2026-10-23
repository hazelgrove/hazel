open Util;

/* Witness for incremental-eval cache invalidation when probing changes.
 * `ProbeAll` means every probeable subexpression is targeted (settings.probe_all). */
[@deriving (show({with_path: false}), sexp, yojson)]
type probe_targets =
  | ProbeAll
  | ProbeTargets(SubexpProbeTargets.t);

let equal_probe_targets = (a: probe_targets, b: probe_targets): bool =>
  switch (a, b) {
  | (ProbeAll, ProbeAll) => true
  | (ProbeTargets(a), ProbeTargets(b)) => SubexpProbeTargets.equal(a, b)
  | (ProbeAll, ProbeTargets(_))
  | (ProbeTargets(_), ProbeAll) => false
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  elab_term: Exp.t,
  co_ctx: CoCtx.t,
  probe_targets,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: Id.Map.t(entry),
  targets: Sample.targets /* IDs of expressions/patterns to sample */
};

let empty: t = {
  statics: Id.Map.empty,
  targets: Sample.no_targets,
};

let find_opt = (id: Id.t, map: t): option(entry) =>
  Id.Map.find_opt(id, map.statics);

/* Observation and stream publication key only on PROPER program
 * structure — ids of the elaborated program the user wrote. Stepped
 * intermediates (administrative structure: Id.invalid temps, fresh ids
 * minted mid-evaluation) are never program nodes. */
let is_program_node = (id: Id.t, map: t): bool =>
  Id.Map.mem(id, map.statics);

/* Statics presence (not targets) is what enables incr-entry snapshots
 * and reuse_check hits. */
let has_statics = (map: t): bool => !Id.Map.is_empty(map.statics);

/* For callers (CLI, tests) that evaluate without statics. */
let of_targets = (targets: Sample.targets): t => {
  ...empty,
  targets,
};

/* `$hole` is a statics-only sentinel for unused-variable warnings. It is not
 * a runtime dependency, so it should not participate in reuse provenance. */
let is_runtime_dependency = (name: string): bool => name != "$hole";

/* what evaluation reads of a co_ctx (its names, IncrEval) but "$hole":
   DefStatics' per-item body holes mint a "$hole" use each analysis,
   which would change every enclosing root's entry */
let runtime_co_ctx = (co_ctx: CoCtx.t): CoCtx.t =>
  List.for_all(((name, _)) => is_runtime_dependency(name), co_ctx)
    ? co_ctx : CoCtx.filter_names(is_runtime_dependency, co_ctx);

/* what an entry's witness reads besides its own probe targets */
[@deriving (show({with_path: false}), sexp, yojson)]
type witnessing = {
  probe_all: bool,
  full: list(Id.t) /* full-capture probes */
};

let witnessing = (~probe_all: bool, ~targets: Sample.targets): witnessing => {
  probe_all,
  full:
    Id.Map.fold(
      (id, spec: Sample.capture_spec, acc) => spec.full ? [id, ...acc] : acc,
      targets,
      [],
    ),
};

/* an expression's entry; None for other infos */
let entry_of_info = (w: witnessing, info: Info.t): option(entry) =>
  switch (info) {
  | InfoExp({elab_term, co_ctx, probe_targets, _}) =>
    Some({
      elab_term,
      co_ctx: runtime_co_ctx(co_ctx),
      /* a term holding a full-capture probe gets a witness of its own,
         so its samples are retaken rather than replayed with elided
         values */
      probe_targets:
        if (List.exists(
              id => MerkleSet.mem(id, probe_targets.probe_ids),
              w.full,
            )) {
          ProbeTargets(SubexpProbeTargets.mark_full(probe_targets));
        } else {
          w.probe_all ? ProbeAll : ProbeTargets(probe_targets);
        },
    })
  | _ => None
  };

let of_info_map =
    (~probe_all: bool, ~targets: Sample.targets, info_map: StaticsBase.Map.t)
    : t => {
  let w = witnessing(~probe_all, ~targets);
  {
    statics:
      Id.Map.filter_map((_id, info) => entry_of_info(w, info), info_map),
    targets,
  };
};

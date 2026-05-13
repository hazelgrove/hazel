open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  elab_term: Exp.t,
  co_ctx: CoCtx.t,
  /* Snapshot of which binder each free-var name in `co_ctx` resolves to at
   * this id, taken at statics time. Used by `IncrEval.reuse_check` to detect
   * shadowing changes: if an outer Let around a cached subtree is added or
   * removed, the subtree's free-var names resolve to different binder ids,
   * and the cached value (computed against the old resolution) is invalid. */
  refs: Binding.s,
  /* See `prev_probe_targets` in IncrEval — None under `probe_all`. */
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
      | Info.InfoExp({elab_term, co_ctx, ctx, probe_targets, _}) =>
        let refs =
          co_ctx
          |> VarMap.to_list
          |> List.map(((n, _)) => Ctx.binding_of(ctx, n));
        Some({
          elab_term,
          co_ctx,
          refs,
          probe_targets: probe_all ? None : Some(probe_targets),
        });
      | _ => None
      },
    info_map,
  );

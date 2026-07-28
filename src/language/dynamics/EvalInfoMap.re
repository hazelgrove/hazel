open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  elab_term: Exp.t,
  co_ctx: CoCtx.t,
  /* See `prev_probe_targets` in IncrEval — None under `probe_all`. */
  probe_targets: option(SubexpProbeTargets.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(entry);

let empty: t = Id.Map.empty;

let is_empty = Id.Map.is_empty;

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

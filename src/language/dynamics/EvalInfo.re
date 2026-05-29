open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  elab_term: Exp.t,
  co_ctx: CoCtx.t,
  /* See `prev_probe_targets` in IncrEval — None under `probe_all`. */
  probe_targets: option(SubexpProbeTargets.t),
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

let of_info_map =
    (~probe_all: bool, ~targets: Sample.targets, info_map: StaticsBase.Map.t)
    : t => {
  statics:
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
    ),
  targets,
};

open Util;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: Exp.t,
  elaborated: Exp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
  targets: Sample.targets /* Maps expr/pat IDs to capture specs for sampling */
};

let empty: t = {
  term: {
    annotation: {
      ids: [Id.invalid],
      secondary: IdTagged.IdTag.empty_secondary,
    },
    term: Tuple([]),
  },
  elaborated: {
    annotation: {
      ids: [Id.invalid],
      secondary: IdTagged.IdTag.empty_secondary,
    },
    term: Tuple([]),
  },
  info_map: Id.Map.empty,
  error_ids: [],
  targets: Sample.no_targets,
};

let elaborate =
  Core.Memo.general(~cache_size_bound=1000, Elaborator.uexp_elab);

let dh_err = (error: string): DHExp.t => Var(error) |> DHExp.fresh;

/* Predicate for whether a term should be probed when ProbeAll is on.
 * Currently const true - probes all expressions / patterns */
let should_probe = (info: Info.t): bool =>
  switch (info) {
  | InfoExp(_)
  | InfoPat(_) => true
  | _ => false
  };

/* Collect all expression and pattern IDs from info_map that pass the should_probe predicate. */
let all_probeable_ids = (info_map: Statics.Map.t): Id.Map.t(unit) =>
  Id.Map.fold(
    (id, info, acc) => should_probe(info) ? Id.Map.add(id, (), acc) : acc,
    info_map,
    Id.Map.empty,
  );

/* Compute targets from probe_ids. For each ID, determine whether it's
 * an expression or pattern target, then look up the appropriate refs to capture.
 * When probe_all is enabled, we target everything in info_map that passes
 * should_probe, ignoring the passed probe_ids (which are a subset anyway). */
let compute_targets =
    (
      ~settings: CoreSettings.t,
      ~info_map: Statics.Map.t,
      ~probe_ids: Id.Map.t(unit),
    )
    : Sample.targets => {
  let effective_probe_ids =
    settings.probe_all ? all_probeable_ids(info_map) : probe_ids;
  Id.Map.fold(
    (id, (), acc) => {
      let refs =
        switch (Statics.Map.lookup(id, info_map)) {
        | Some(InfoExp(_)) => Statics.Map.refs_in(info_map, id) /* Expression target */
        | Some(InfoPat(_)) => Statics.Map.bound_in(info_map, id) /* Pattern target */
        | _ => [] /* Unknown - no refs */
        };
      let spec: Sample.capture_spec = {refs: refs};
      Id.Map.add(id, spec, acc);
    },
    effective_probe_ids,
    Id.Map.empty,
  );
};

let init_from_term =
    (
      ~settings,
      ~is_dynamic_term,
      ~ctx=?,
      ~ana=?,
      ~probe_ids=Id.Map.empty,
      term,
    )
    : t => {
  let ctx_init =
    Option.value(
      ~default=Builtins.ctx_init(is_dynamic_term ? None : Some(Int)),
      ctx,
    );
  let info_map = Statics.mk(~ana?, settings, ctx_init, term);
  let error_ids = Statics.Map.error_ids(info_map);
  let elaborated =
    switch () {
    | _ when !settings.statics => dh_err("Statics disabled")
    | _ when !settings.dynamics && !settings.elaborate =>
      dh_err("Dynamics & Elaboration disabled")
    | _ =>
      switch (elaborate(info_map, term)) {
      | DoesNotElaborate => dh_err("Elaboration returns None")
      | Elaborates(d, _) => d
      }
    };
  let targets = compute_targets(~settings, ~info_map, ~probe_ids);
  {
    term,
    elaborated,
    info_map,
    error_ids,
    targets,
  };
};

let init =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~ctx=?,
      ~ana=?,
      z: Zipper.t,
    )
    : t => {
  let make_term_result = MakeTerm.from_zip_for_sem(z);
  let term = make_term_result.term |> stitch;
  /* Extract probe IDs directly from zipper's refractors (manuals + ephemerals).
   * Map values to unit since we only need the IDs as keys. */
  let probe_ids =
    Id.Map.union(
      (_, _, _) => Some(),
      Id.Map.map(_ => (), z.refractors.manuals),
      Id.Map.map(_ => (), z.refractors.autos.ephemerals),
    );

  init_from_term(~settings, ~ctx?, ~is_dynamic_term, ~ana?, ~probe_ids, term);
};

let init =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~ctx=?,
      ~ana=?,
      z: Zipper.t,
    ) =>
  settings.statics
    ? init(~settings, ~stitch, ~ctx?, ~is_dynamic_term, ~ana?, z) : empty;

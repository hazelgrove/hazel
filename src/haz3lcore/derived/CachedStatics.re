open Util;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: Exp.t,
  elaborated: Exp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
  probe_map: Id.Map.t(Probe.t) /* Maps expr/pat IDs to probe metadata (refs to capture) */
};

let empty: t = {
  term: {
    annotation: {
      ids: [Id.invalid],
    },
    term: Tuple([]),
  },
  elaborated: {
    annotation: {
      ids: [Id.invalid],
    },
    term: Tuple([]),
  },
  info_map: Id.Map.empty,
  error_ids: [],
  probe_map: Id.Map.empty,
};

let elaborate =
  Core.Memo.general(~cache_size_bound=1000, Elaborator.uexp_elab);

let dh_err = (error: string): DHExp.t => Var(error) |> DHExp.fresh;

/* Predicate for whether an expression/pattern should be probed.
 * Currently const true - probes everything (InfoExp and InfoPat).
 * Future: could filter out holes, function-typed exprs, etc. */
let should_probe = (info: Info.t): bool =>
  switch (info) {
  | InfoExp(_)
  | InfoPat(_) => true
  | _ => false /* Skip InfoTyp, InfoTPat, Secondary */
  };

/* Collect all expression and pattern IDs from info_map that pass the should_probe predicate. */
let all_probeable_ids = (info_map: Statics.Map.t): Id.Map.t(unit) =>
  Id.Map.fold(
    (id, info, acc) => should_probe(info) ? Id.Map.add(id, (), acc) : acc,
    info_map,
    Id.Map.empty,
  );

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

  /* Compute probe_map from probe_ids. For each ID, determine whether it's
   * an expression or pattern probe, then look up the appropriate refs to capture.
   *
   * When probe_all is enabled, we probe everything in info_map that passes
   * should_probe, ignoring the passed probe_ids (which are a subset anyway).
   *
   * KNOWN ISSUE: Probes on parenthesized expressions don't work. The paren tile ID
   * is added to refractors, but elaboration strips Parens wrappers, so the ID
   * doesn't exist in the elaborated term and the probe won't capture anything.
   * See plans/progressive-sample-accumulation.md "Probe on Parens Bug". */
  let effective_probe_ids =
    settings.probe_all ? all_probeable_ids(info_map) : probe_ids;
  let probe_map =
    Id.Map.fold(
      (id, (), acc) => {
        let refs =
          switch (Statics.Map.lookup(id, info_map)) {
          | Some(InfoExp(_)) => Statics.Map.refs_in(info_map, id) /* Expression probe */
          | Some(InfoPat(_)) => Statics.Map.bound_in(info_map, id) /* Pattern probe */
          | _ => [] /* Unknown - no refs */
          };
        let probe = {Probe.refs: refs};
        Id.Map.add(id, probe, acc);
      },
      effective_probe_ids,
      Id.Map.empty,
    );

  {
    term,
    elaborated,
    info_map,
    error_ids,
    probe_map,
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
      Id.Map.map(_ => (), z.refractors.ephemerals),
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

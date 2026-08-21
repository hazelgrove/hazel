open Util;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: Exp.t,
  elaborated: Exp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
  warning_ids: list(Id.t),
  targets: Sample.targets, /* Maps expr/pat IDs to capture specs for sampling */
  /* Type-shape obligations derived from the PRE-reification pass:
     the view must show what is owed even though (with reification
     on) the final info_map no longer exhibits the deficit. These
     are the TYPE FACTS only, at statics cadence (debounce-stale
     during typing) — the assist stream assembles from them
     FRAME-FRESH in Editor.calculate (TypeObligations.assist_stream)
     so anchors and counts never lag the syntax. */
  obligations: list(TypeObligations.t),
};

let empty: t = {
  term: {
    term: Tuple([]),
    annotation: IdTagged.IdTag.temp(),
  },
  elaborated: {
    term: Tuple([]),
    annotation: IdTagged.IdTag.temp(),
  },
  info_map: Id.Map.empty,
  error_ids: [],
  warning_ids: [],
  targets: Sample.no_targets,
  obligations: [],
};

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
        switch (Statics.Map.lookup_exp(id, info_map)) {
        | Some(_) => Statics.Map.refs_in(info_map, id)
        | None =>
          switch (Statics.Map.lookup_pat(id, info_map)) {
          | Some(_) => Statics.Map.bound_in(info_map, id)
          | None => []
          }
        };
      let spec: Sample.capture_spec = {refs: refs};
      Id.Map.add(id, spec, acc);
    },
    effective_probe_ids,
    Id.Map.empty,
  );
};

/* Extract probe IDs directly from zipper's refractors (manuals + ephemerals).
 * Map values to unit since we only need the IDs as keys. */
let probe_ids_of_zipper = (z: Zipper.t): Id.Map.t(unit) =>
  Id.Map.union(
    (_, _, _) => Some(),
    Id.Map.map(_ => (), Id.Map.of_list(z.refractors.manuals)),
    Id.Map.map(_ => (), z.refractors.multis.ephemerals),
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
  let (info_map, elaborated) =
    Statics.mk(~ana?, ~probe_ids, settings, ctx_init, term);
  let error_ids = Statics.Map.error_ids(info_map);
  let warning_ids = Statics.Map.warning_ids(info_map);
  let elaborated =
    switch () {
    | _ when !settings.statics => dh_err("Statics disabled")
    | _ when !settings.dynamics && !settings.elaborate =>
      dh_err("Dynamics & Elaboration disabled")
    | _ => elaborated
    };
  let targets = compute_targets(~settings, ~info_map, ~probe_ids);
  {
    term,
    elaborated,
    info_map,
    error_ids,
    warning_ids,
    targets,
    obligations: [],
  };
};

/* Recompute only `targets` from the zipper's current refractors, reusing the
 * existing info_map. Cheap: O(|probe_ids|) fold. Used at the end of
 * Editor.Update.calculate to pick up refractor changes made by probe
 * effects (collision cleanup, auto-probe regen), without redoing statics. */
let with_targets = (~settings: CoreSettings.t, z: Zipper.t, s: t): t => {
  let probe_ids = probe_ids_of_zipper(z);
  let targets = compute_targets(~settings, ~info_map=s.info_map, ~probe_ids);
  {
    ...s,
    targets,
  };
};

let init =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~ctx=?,
      ~root,
      ~ana=?,
      z: Zipper.t,
    )
    : t => {
  let make_term_result = MakeTerm.from_zip_for_sem(z, ~root);
  let term = make_term_result.term |> stitch;
  let probe_ids = probe_ids_of_zipper(z);

  let statics =
    init_from_term(
      ~settings,
      ~ctx?,
      ~is_dynamic_term,
      ~ana?,
      ~probe_ids,
      term,
    );
  /* Pass 2: reify type-shape obligations. The deficit is judged on
     pass-1 statics; when owed and enabled, semantics reruns on the
     spliced term (per-element ana, no arity error). Obligations are
     kept from pass 1 for display either way. One step reaches the
     fixpoint: the spliced tuples are complete. */
  let with_obligations = (statics: t, obs: list(TypeObligations.t)): t => {
    ...statics,
    obligations: obs,
  };
  /* satisfied records (deficit 0, no junctions) are TYPE FACTS for
     the frame assembly, not reification work — don't pay the second
     statics pass for them */
  let needs_reify = (obs: list(TypeObligations.t)) =>
    obs
    |> List.exists((ob: TypeObligations.t) =>
         TypeObligations.deficit(ob) > 0 || ob.junctions
       );
  switch (TypeObligations.derive(statics.info_map)) {
  | [] => with_obligations(statics, [])
  | obs when !settings.reify_obligations || !needs_reify(obs) =>
    with_obligations(statics, obs)
  | obs =>
    let make_term_result =
      MakeTerm.from_zip_for_sem_spliced(
        z,
        ~root,
        ~splice=TypeObligations.reify(obs),
      );
    let term = make_term_result.term |> stitch;
    let statics =
      init_from_term(
        ~settings,
        ~ctx?,
        ~is_dynamic_term,
        ~ana?,
        ~probe_ids,
        term,
      );
    with_obligations(statics, obs);
  };
};

let init =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~ctx=?,
      ~root,
      ~ana=?,
      z: Zipper.t,
    ) =>
  settings.statics
    ? init(~settings, ~stitch, ~ctx?, ~is_dynamic_term, ~root, ~ana?, z)
    : empty;

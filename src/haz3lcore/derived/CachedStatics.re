open Util;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: Exp.t,
  elaborated: Exp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
  warning_ids: list(Id.t),
  targets: Sample.targets /* Maps expr/pat IDs to capture specs for sampling */
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
  /* Reify scaffold buffer: virtually insert scaffold commas so statics
   * sees the tuple structure (e.g., Ap(f, Tuple([1, ⬚])) not Ap(f, 1)) */
  let z_for_sem = TyDiScaffold.reify(~root, z);
  let make_term_result = MakeTerm.from_zip_for_sem(z_for_sem, ~root);
  let term = make_term_result.term |> stitch;
  let probe_ids = probe_ids_of_zipper(z);

  init_from_term(~settings, ~ctx?, ~is_dynamic_term, ~ana?, ~probe_ids, term);
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

/* Compute the correct assist buffer and statics together, resolving
 * the circular dependency between them:
 *
 * 1. Run statics on the bare zipper (no buffer) → info_map
 * 2. Compute the assist buffer using that info_map
 *    (completion + scaffold + suppression logic)
 * 3. If the buffer has structural scaffold content, re-run statics
 *    so the elaborated term includes the tuple structure via reify
 *
 * Returns (updated_zipper, statics). The zipper has the correct
 * buffer set; statics reflects the reified scaffold if present. */
let init_with_assist =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~ctx=?,
      ~root,
      ~ana=?,
      z: Zipper.t,
    )
    : (Zipper.t, t) => {
  /* Step 1: statics on bare zipper */
  let clean_z = Zipper.clear_unparsed_buffer(z);
  let statics =
    init(~settings, ~is_dynamic_term, ~stitch, ~ctx?, ~root, ~ana?, clean_z);

  if (!settings.assist || !settings.statics) {
    (clean_z, statics);
  } else {
    /* Step 2: compute buffer with fresh info_map */
    let z_with_buffer =
      Buffer.set_assist_buffer(~info_map=statics.info_map, clean_z);

    /* Step 3: if scaffold was generated, re-run statics so
     * reify can virtually insert the commas/holes */
    if (TyDiScaffold.is_scaffold(z_with_buffer)) {
      let statics =
        init(
          ~settings,
          ~is_dynamic_term,
          ~stitch,
          ~ctx?,
          ~root,
          ~ana?,
          z_with_buffer,
        );
      (z_with_buffer, statics);
    } else {
      (z_with_buffer, statics);
    };
  };
};

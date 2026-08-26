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
  /* the probe ids the info_map was ANALYZED with (per-node probe_targets
     witnesses depend on them). with_targets deliberately does NOT update
     this: it refreshes only `targets`, so a mismatch against the zipper's
     current probes means the map itself is stale for probing. */
  probe_ids: Id.Map.t(unit),
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
  probe_ids: Id.Map.empty,
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
    probe_ids,
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

/* Typ-rooted cells (type-alias bodies in the editor stack): wrap the
   type in a TyAlias under the frozen ctx so the info map carries real
   InfoTyp entries — cursor inspector, sort refinement, type errors.
   Wrapper node ids are fresh and never rendered in the cell, so their
   marks stay invisible there (and the Problems panel filters to ids
   present in each editor's own term). */
let init_typ = (~settings: CoreSettings.t, ~ctx=?, z: Zipper.t): t =>
  if (!settings.statics) {
    empty;
  } else {
    let ctx =
      Option.value(
        ~default=Builtins.ctx_init(Some(Operators.default_mode)),
        ctx,
      );
    let ty = MakeTerm.from_zip_for_typ(z);
    let term: Exp.t =
      Exp.fresh(TyAlias(TPat.fresh(EmptyHole), ty, Exp.fresh(Tuple([]))));
    let (info_map, _) = Statics.mk(settings, ctx, term);
    {
      term,
      elaborated: dh_err("Type cell: no dynamics"),
      info_map,
      error_ids: Statics.Map.error_ids(info_map),
      warning_ids: [],
      targets: Sample.no_targets,
      probe_ids: Id.Map.empty,
    };
  };

/* Pat-rooted cells (`name : T` header editors): wrap the pattern as a
   function parameter so it types under the frozen ctx — InfoPat
   entries for the inspector + sort styling. The hole body keeps the
   binders from reading as unused. */
let init_pat = (~settings: CoreSettings.t, ~ctx=?, z: Zipper.t): t =>
  if (!settings.statics) {
    empty;
  } else {
    let ctx =
      Option.value(
        ~default=Builtins.ctx_init(Some(Operators.default_mode)),
        ctx,
      );
    let p = MakeTerm.from_zip_for_pat(z);
    let term: Exp.t = Exp.fresh(Fun(p, Exp.fresh(EmptyHole), None, None));
    let (info_map, _) = Statics.mk(settings, ctx, term);
    {
      term,
      elaborated: dh_err("Header cell: no dynamics"),
      info_map,
      error_ids: Statics.Map.error_ids(info_map),
      warning_ids: [],
      targets: Sample.no_targets,
      probe_ids: Id.Map.empty,
    };
  };

/* TPat-rooted cells (type-alias header editors): wrap as the alias
   binder of an unknown type. */
let init_tpat = (~settings: CoreSettings.t, ~ctx=?, z: Zipper.t): t =>
  if (!settings.statics) {
    empty;
  } else {
    let ctx =
      Option.value(
        ~default=Builtins.ctx_init(Some(Operators.default_mode)),
        ctx,
      );
    let tp = MakeTerm.from_zip_for_tpat(z);
    let term: Exp.t =
      Exp.fresh(
        TyAlias(
          tp,
          Typ.fresh(Unknown(Hole(EmptyHole))),
          Exp.fresh(Tuple([])),
        ),
      );
    let (info_map, _) = Statics.mk(settings, ctx, term);
    {
      term,
      elaborated: dh_err("Header cell: no dynamics"),
      info_map,
      error_ids: Statics.Map.error_ids(info_map),
      warning_ids: [],
      targets: Sample.no_targets,
      probe_ids: Id.Map.empty,
    };
  };

/* COMPOSITIONAL init for whole-program (Exp-rooted, top-level) editors:
   statics via DefStatics — per top-level item with chained ctxs — so
   an edit re-analyzes only the dirty set, and no monolithic
   whole-program statics/elaboration recursion runs (which STACK
   OVERFLOWS in the browser on some large programs, e.g. mega-2k).
   The whole-program elaboration is grafted from the per-item elabs;
   if a graft boundary has an unexpected shape we degrade to a
   no-eval error term instead of crashing. Falls back to the
   monolithic path for non-Exp roots or custom ctx/ana. */
let init_compositional =
    (~settings: CoreSettings.t, ~stitch, ~root, z: Zipper.t): t =>
  if (!settings.statics) {
    empty;
  } else if (root != Sort.Exp) {
    init(~settings, ~is_dynamic_term=false, ~stitch, ~root, z);
  } else {
    let make_term_result = MakeTerm.from_zip_for_sem(z, ~root);
    let term = make_term_result.term |> stitch;
    let probe_ids = probe_ids_of_zipper(z);
    let ds = DefStatics.calc_auto(~settings, ~probe_ids, term);
    let info_map = ds.merged;
    let elaborated =
      switch () {
      | _ when !settings.dynamics && !settings.elaborate =>
        dh_err("Dynamics & Elaboration disabled")
      | _ =>
        switch (DefStatics.whole_elab(ds)) {
        | Some(elab) => elab
        | None => dh_err("Compositional elaboration gap")
        }
      };
    {
      term,
      elaborated,
      info_map,
      error_ids: DefStatics.all_error_ids(ds),
      warning_ids: DefStatics.all_warning_ids(ds),
      targets: compute_targets(~settings, ~info_map, ~probe_ids),
      probe_ids,
    };
  };

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
    annotation: IdTagged.IdTag.temp,
  },
  elaborated: {
    term: Tuple([]),
    annotation: IdTagged.IdTag.temp,
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
  /* The model argument of a projected livelit use, whose value the commit
     path reads when writing a ^name.update(model, action) transition */
  let rec livelit_model = (e: Exp.t): option(Id.t) =>
    switch (e.term) {
    | Parens(e) => livelit_model(e)
    | Ap(_, {term: LivelitName(_), _}, model) => Some(Exp.rep_id(model))
    | _ => None
    };
  Id.Map.fold(
    (id, (), acc) => {
      let entries =
        switch (Statics.Map.lookup_exp(id, info_map)) {
        /* A livelit projector's samples are read only for their value;
           its refs would capture the whole livelit record per sample.
           Also target the model argument, for the commit path. */
        | Some({
            user_term: {term: Projector({kind: Livelit, _}, inner), _},
            _,
          }) =>
          let model =
            switch (livelit_model(inner)) {
            | Some(model_id) => [(model_id, {Sample.refs: []})]
            | None => []
            };
          [(id, {Sample.refs: []}), ...model];
        | Some(_) => [(id, {refs: Statics.Map.refs_in(info_map, id)})]
        | None =>
          switch (Statics.Map.lookup_pat(id, info_map)) {
          | Some(_) => [(id, {refs: Statics.Map.bound_in(info_map, id)})]
          | None => [(id, {refs: []})]
          }
        };
      List.fold_left(
        (acc, (id, spec)) => Id.Map.add(id, spec, acc),
        acc,
        entries,
      );
    },
    effective_probe_ids,
    Id.Map.empty,
  );
};

/* Ids of projectors which opt into dynamic information (Projector.dynamics).
 * Probing a projector's term id is what populates its `info.dynamics`, so
 * such a projector can see the live value of the syntax it replaces. */
let projector_probe_ids =
    (projectors: Id.Map.t(Base.projector)): Id.Map.t(unit) =>
  Id.Map.fold(
    (id, p: Base.projector, acc) => {
      let (module P) = ProjectorInit.to_module(p.kind);
      P.dynamics ? Id.Map.add(id, (), acc) : acc;
    },
    projectors,
    Id.Map.empty,
  );

/* Extract probe IDs directly from zipper's refractors (manuals + ephemerals),
 * plus the ids of any dynamics-requesting projectors.
 * Map values to unit since we only need the IDs as keys. */
let probe_ids_of_zipper =
    (~projectors=Id.Map.empty, z: Zipper.t): Id.Map.t(unit) =>
  Id.Map.union(
    (_, _, _) => Some(),
    Id.Map.union(
      (_, _, _) => Some(),
      Id.Map.map(_ => (), Id.Map.of_list(z.refractors.manuals)),
      Id.Map.map(_ => (), z.refractors.multis.ephemerals),
    ),
    projector_probe_ids(projectors),
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
let with_targets =
    (~settings: CoreSettings.t, ~projectors=Id.Map.empty, z: Zipper.t, s: t)
    : t => {
  let probe_ids = probe_ids_of_zipper(~projectors, z);
  let targets = compute_targets(~settings, ~info_map=s.info_map, ~probe_ids);
  /* identity-preserving: this runs on EVERY calculate cycle (each
     eval-result chunk included), and downstream consumers key caches
     and change-detection on the statics RECORD — rebuilding it each
     time made every eval tick look like a program change */
  Id.Map.equal(Sample.equal_capture_spec, targets, s.targets)
    ? s
    : {
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
  let probe_ids =
    probe_ids_of_zipper(~projectors=make_term_result.projectors, z);

  init_from_term(~settings, ~ctx?, ~is_dynamic_term, ~ana?, ~probe_ids, term);
};

/* The zipper the editor's statics were last computed for, and that
   result: a structural (agent) action on the very same zipper can start
   from this map instead of a fresh full pass (CompositionGo). Physical
   identity is the freshness test — an edited zipper is a new value. */
/* statics computed for a zipper by someone who is not the editor (the
   agent tool path checks the program it just produced): offered here so the
   editor's own recompute for that very program can take them instead.
   Keyed by a fingerprint of the program's piece ids (secondaries left out:
   normalization and re-indentation only move whitespace, and the editor
   rebuilds its zipper record on every calculate, so object identity does
   not survive the trip) */
let rec fingerprint_seg = (seg: Segment.t, acc: list(Id.t)): list(Id.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (p) {
      | Secondary(_) => acc
      | Grout(g) => [g.id, ...acc]
      | Projector(pr) => [pr.id, ...acc]
      | Tile(t) =>
        List.fold_left(
          (acc, ch) => fingerprint_seg(ch, acc),
          [t.id, ...acc],
          t.children,
        )
      },
    acc,
    seg,
  );
let fingerprint = (z: Zipper.t): list(Id.t) =>
  fingerprint_seg(Zipper.unselect_and_zip(~erase_buffer=true, z), []);
/* the last few inits (other editors and the canvas snapshot also init),
   keyed by program fingerprint */
let last_inits: ref(list((list(Id.t), t))) = ref([]);
let offered: ref(list((list(Id.t), t))) = ref([]);
let offer = (z: Zipper.t, st: t): unit => {
  let fp = fingerprint(z);
  offered := [(fp, st), ...List.filteri((i, _) => i < 3, offered^)];
  /* an editor that takes the offer holds statics that never went through
     init: enter them in the ring too, so the next tool's initial-statics
     reuse (for_zipper) recognizes them */
  last_inits := [(fp, st), ...List.filteri((i, _) => i < 5, last_inits^)];
};
let offered_for = (z: Zipper.t): option(t) =>
  switch (offered^) {
  | [] =>
    PerfTimer.record("offer/miss-none", 0.);
    None;
  | offers =>
    let fp = fingerprint(z);
    switch (List.find_opt(((fp0, _)) => fp0 == fp, offers)) {
    | Some((_, st)) => Some(st)
    | None =>
      PerfTimer.record("offer/miss-fp", 0.);
      None;
    };
  };
/* the editor's own statics, when they were computed from the program the
   zipper holds now (the editor rebuilds its zipper record on every
   calculate, so this is a fingerprint match, not identity) */
let for_zipper = (z: Zipper.t, st: t): option(t) =>
  switch (List.find_opt(((_, st0)) => st0 === st, last_inits^)) {
  | Some((fp0, _)) when fp0 == fingerprint(z) => Some(st)
  | _ => None
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
  if (settings.statics) {
    PerfTimer.record("cs/init", 0.);
    let st =
      init(~settings, ~stitch, ~ctx?, ~is_dynamic_term, ~root, ~ana?, z);
    last_inits :=
      [
        (fingerprint(z), st),
        ...List.filteri((i, _) => i < 5, last_inits^),
      ];
    st;
  } else {
    empty;
  };

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
/* compositional statics from an already-made TERM: callers that hold
   a plain segment (restructure ops) skip the zipper round-trip —
   from_zip_for_sem's Dump.to_segment walk alone was ~300ms on mega-2k */
let init_compositional_term =
    (~settings: CoreSettings.t, ~probe_ids, term: Exp.t): t => {
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

let init_compositional =
    (~settings: CoreSettings.t, ~stitch, ~root, ~probe_ids=?, z: Zipper.t): t =>
  if (!settings.statics) {
    empty;
  } else if (root != Sort.Exp && root != Sort.Mod) {
    init(~settings, ~is_dynamic_term=false, ~stitch, ~root, z);
  } else {
    /* from_zip_for_sem exists to EMPTY THE BACKPACK for semantics —
       with an empty backpack its Dump.to_segment walk is pure
       overhead (~660ms at 4k lines), and the per-item incremental
       parse replaces the monolithic one. Mod roots would be MISPARSED
       by the Exp-rooted [go], so their backpack fallback goes through
       go_mod_root on the emptied segment instead. */
    /* (this branch has no backpack: a segment with MISSING shards takes
       the canonical-completion parse instead of the per-item one) */
    let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
    let term =
      Segment.global_missing_shards(seg) == []
        ? MakeTerm.Incr.term_of_root(~root, seg) |> stitch
        : (
            root == Sort.Mod
              ? MakeTerm.go_mod_root(seg).term
              : MakeTerm.from_zip_for_sem(z, ~root).term
          )
          |> stitch;
    /* callers with probes living in OTHER zippers (stacked cells)
       pass the union; default = this zipper's own */
    let probe_ids =
      switch (probe_ids) {
      | Some(p) => p
      | None => probe_ids_of_zipper(z)
      };
    /* NOT entered in the reuse ring: the agent tool path reuses ring
       records as its initial map and builds its node map from them
       (HighLevelNodeMap.build walks Info.ancestors to the program top; the
       per-item map records ancestors per item → build = None). The tools
       need a monolithic map until they are converged onto items. */
    PerfTimer.record("cs/init-compositional", 0.);
    init_compositional_term(~settings, ~probe_ids, term);
  };

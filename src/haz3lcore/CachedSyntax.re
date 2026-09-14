type t = {
  old: bool,
  segment: Segment.t,
  measured: Measured.t,
  selection_ids: list(Id.t),
  /* The term-derived data structured below, may differ
   * from the term used for semantics. These terms are identical when
   * the backpack is empty. If the backpack is non-empty, then when we
   * make the term for semantics, we attempt to empty the backpack
   * according to some simple heuristics (~ try to empty it greedily
   * while moving rightwards from the current caret position).
   * this is currently necessary to have the cursorinfo/completion
   * workwhen the backpack is nonempty.
   *
   * This is a brittle part of the current implementation. there are
   * some other comments at some of the weakest joints; the biggest
   * issue is that dropping the backpack can add/remove grout, causing
   * certain ids to be present/non-present unexpectedly. */
  term_data: TermData.t,
  terms: TermMap.t,
  /* A list of projector IDs in the order they appear in the segment
   * (allows actions to refer to projectors by index) */
  projector_list: list(Id.t),
  /* Since the introduction of shape_map below, caching projectors
   * here is almost vesigial (currently used only for error deco) */
  projectors: Id.Map.t(Base.projector),
  /* The shape_map is used to leave space for projectors in the
   * underlying editor. In principle calculating this can involve
   * both static and dynamic information, so we cache this for perf */
  shape_map: ProjectorCore.Shape.Map.t,
  /* Rows reserved below a refractor's tile, e.g. an open probe drawer.
   * Nonzero entries only, so consumers may treat the map as "open
   * drawers" and iterate it wholesale (Move, RefractorShift). Measured
   * and Code.view must agree on these rows or decorations drift from
   * caret/text; both defer them to the linebreak after the tile.
   * Rebuilds with unchanged contents reuse the old map, so physical
   * identity doubles as a did-anything-change signal downstream. */
  refractor_rows: Id.Map.t(int),
  /* Refractor inputs last used to compute refractor_rows/shape_map;
   * compared by physical eq in `calculate` to skip the rebuild. */
  cached_manuals: Refractors.RefractorList.t,
  cached_ephemerals: Refractors.Map.t,
  /* Errors reported by projectors (e.g. "can't render as table") */
  projector_errors: Id.Map.t(ProjectorBase.error),
  cached_backpack: list(Tile.t),
  /* Inputs last used to compute shape_map/projector_errors/measured.
   * Kept so `calculate` can detect when statics changed and refresh
   * shapes automatically — callers don't need to plumb that signal. */
  shape_info_map: Language.Statics.Map.t,
  shape_dyn_map: Language.Dynamics.Map.t,
  shape_elaborated: option(Language.Exp.t),
  /* per-editor chunk memo for incremental re-measurement; rides the
     generation chain via {...old} so each editor keeps its own */
  m_cache: Measured.Incr.cache,
  /* per-editor item memo for incremental parsing (terms/term_data/
     projectors composed per item instead of a whole-program walk) */
  t_cache: MakeTerm.Incr.cache,
};

// should not be serializing
let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

/* fallback Secondary covers ids with no resolvable segment yet
 * (early frames before MakeTerm has populated term_data). */
let refractor_syntax_piece = (id: Id.t, term_data: TermData.t): Base.piece =>
  Option.value(
    TermData.segment(id, term_data)
    |> Option.map(Segment.unparenthesize)
    |> Option.map(Segment.trim_secondary(Left))
    |> Option.map(Segment.trim_secondary(Right))
    |> Option.map(Segment.parenthesize),
    ~default=
      Base.Secondary({
        id: Id.invalid,
        content: Whitespace(""),
      }),
  );

let mk_refractor_rows =
    (
      z: Zipper.t,
      term_data: TermData.t,
      info_map,
      dyn_map,
      ~elaborated: option(Language.Exp.t),
    )
    : Id.Map.t(int) => {
  let entries =
    Id.Map.union(
      (_, _, b) => Some(b),
      z.refractors.manuals |> Id.Map.of_list,
      z.refractors.multis.ephemerals,
    );
  Id.Map.filter_map(
    (id, entry: Refractors.entry) => {
      let syntax_piece = refractor_syntax_piece(id, term_data);
      let p = Refractors.to_projector(syntax_piece, id, entry);
      let info =
        ProjectorInfo.mk_info(
          p,
          ~sample_focus=z.refractors.sample_focus,
          ~statics=info_map,
          ~dynamics=dyn_map,
          ~elaborated,
        );
      let (module P) = ProjectorInit.to_module(entry.kind);
      let shape = P.placeholder(entry.model, info);
      switch (shape.vertical) {
      | Inline
      | Block(0)
      | Tab(0) => None
      | Tab(n)
      | Block(n) => Some(n)
      };
    },
    entries,
  );
};

let mk =
    (
      ~root=Sort.Exp,
      ~m_cache=?,
      ~t_cache=?,
      ~info_map,
      ~dyn_map,
      ~elaborated=None,
      z,
    )
    : t => {
  let m_cache =
    switch (m_cache) {
    | Some(c) => c
    | None => Measured.Incr.mk_cache()
    };
  let t_cache =
    switch (t_cache) {
    | Some(c) => c
    | None => MakeTerm.Incr.mk_cache()
    };
  let segment = Zipper.unselect_and_zip(z);
  /* Exp and Mod roots take the per-item incremental parse; other
     roots (Pat/Typ/TPat/Drv/... cells, all small) parse ONCE at their
     own sort — the Exp-rooted [go] misparses them (every token
     sort-inconsistent), and running it just for projectors paid a
     full wrong parse */
  let (terms, term_data, projectors, projector_list) =
    if (root == Sort.Exp || root == Sort.Mod) {
      let MakeTerm.{term: _, terms, projectors, projector_list, term_data} =
        MakeTerm.Incr.go_incr(~root, ~cache=t_cache, segment);
      (terms, term_data, projectors, projector_list);
    } else {
      MakeTerm.sorted_syntax_data(~root, segment);
    };
  let (projector_shapes, projector_errors) =
    ProjectorInfo.ShapeMapSemantics.mk(
      projectors,
      z.refractors,
      info_map,
      dyn_map,
      ~elaborated,
    );
  let refractor_rows =
    mk_refractor_rows(z, term_data, info_map, dyn_map, ~elaborated);
  let measured =
    Measured.Incr.of_segment(
      ~cache=m_cache,
      segment,
      projector_shapes,
      refractor_rows,
    );
  {
    old: false,
    segment,
    term_data,
    measured,
    selection_ids: Selection.selection_ids(z.selection),
    terms,
    projectors,
    projector_list,
    shape_map: projector_shapes,
    refractor_rows,
    cached_manuals: z.refractors.manuals,
    cached_ephemerals: z.refractors.multis.ephemerals,
    projector_errors,
    cached_backpack: Segment.global_missing_shards_incr(segment),
    shape_info_map: info_map,
    shape_dyn_map: dyn_map,
    shape_elaborated: elaborated,
    m_cache,
    t_cache,
  };
};

let init = (~root=Sort.Exp, z: Zipper.t) =>
  mk(~root, z, ~info_map=Id.Map.empty, ~dyn_map=Id.Map.empty);

let mark_old: t => t =
  old => {
    ...old,
    old: true,
  };

/* statics or refractor model changed but the segment didn't: reuse
 * segment/term_data, recompute only the shape-derived fields. */
let refresh_shapes =
    (z: Zipper.t, info_map, dyn_map, ~elaborated=None, old: t) => {
  let (shape_map, projector_errors) =
    ProjectorInfo.ShapeMapSemantics.mk(
      old.projectors,
      z.refractors,
      info_map,
      dyn_map,
      ~elaborated,
    );
  let refractor_rows =
    mk_refractor_rows(z, old.term_data, info_map, dyn_map, ~elaborated);
  let refractor_rows =
    Id.Map.equal((==), refractor_rows, old.refractor_rows)
      ? old.refractor_rows : refractor_rows;
  /* Measured only exists to place projector boxes: when the recomputed
     shapes come out identical (the common case — statics/dynamics
     change every streamed chunk, projector shapes almost never do),
     keep the old layout. Re-measuring the whole program here was an
     O(program) cost on EVERY dynamics change. */
  let measured =
    compare(shape_map, old.shape_map) == 0
    && refractor_rows === old.refractor_rows
      ? old.measured
      : Measured.Incr.of_segment(
          ~cache=old.m_cache,
          old.segment,
          shape_map,
          refractor_rows,
        );
  {
    ...old,
    shape_map,
    refractor_rows,
    projector_errors,
    measured,
    cached_manuals: z.refractors.manuals,
    cached_ephemerals: z.refractors.multis.ephemerals,
    shape_info_map: info_map,
    shape_dyn_map: dyn_map,
    shape_elaborated: elaborated,
  };
};

/* phys-eq on option(Exp.t): None===None holds but Some(x)===Some(y) is
 * always false (new box), so compare the inner Exp ref. */
let elaborated_phys_eq =
    (a: option(Language.Exp.t), b: option(Language.Exp.t)): bool =>
  switch (a, b) {
  | (None, None) => true
  | (Some(x), Some(y)) => x === y
  | _ => false
  };

/* Decide how much work to do based on what changed:
 *   - `old.old` flag (segment changed from an edit/buffer clear) → full `mk`
 *   - statics-input refs changed (info_map / dyn_map / elaborated) → refresh shapes
 *   - otherwise just update selection_ids (cheap cursor-only path) */
let calculate =
    (~root=Sort.Exp, z: Zipper.t, info_map, dyn_map, ~elaborated=None, old: t) => {
  let refractor_inputs_changed =
    z.refractors.manuals !== old.cached_manuals
    || z.refractors.multis.ephemerals !== old.cached_ephemerals;
  if (old.old) {
    /* [old] is marked on every zipper change, but CARET/SELECTION
       moves don't change the content: measured/terms/term_data are
       segment functions and can be reused wholesale (a full mk paid
       ~350ms per caret move at 4k lines) */
    let segment = Zipper.unselect_and_zip(z);
    if (Segment.ptr_eq(segment, old.segment)) {
      {
        ...refresh_shapes(z, info_map, dyn_map, ~elaborated, old),
        old: false,
        selection_ids: Selection.selection_ids(z.selection),
      };
    } else {
      mk(
        ~root,
        ~m_cache=old.m_cache,
        ~t_cache=old.t_cache,
        z,
        ~info_map,
        ~dyn_map,
        ~elaborated,
      );
    };
  } else if (info_map !== old.shape_info_map
             || dyn_map !== old.shape_dyn_map
             || !elaborated_phys_eq(elaborated, old.shape_elaborated)
             || refractor_inputs_changed) {
    refresh_shapes(z, info_map, dyn_map, ~elaborated, old);
  } else {
    {
      ...old,
      selection_ids: Selection.selection_ids(z.selection),
    };
  };
};

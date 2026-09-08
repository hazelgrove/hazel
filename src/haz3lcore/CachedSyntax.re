type t = {
  old: bool,
  segment: Segment.t,
  measured: Measured.t,
  selection_ids: list(Id.t),
  /* May differ from the term used for semantics: with shards missing,
   * that term is built from the canonically COMPLETED segment
   * (CanonicalCompletion.for_make_term), so ids may be present/absent
   * between the two views. */
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
  missing_shards: list(Tile.t),
  /* Inputs last used to compute shape_map/projector_errors/measured.
   * Kept so `calculate` can detect when statics changed and refresh
   * shapes automatically — callers don't need to plumb that signal. */
  shape_info_map: Language.Statics.Map.t,
  shape_dyn_map: Language.Dynamics.Map.t,
  shape_elaborated: option(Language.Exp.t),
  /* Inline ghost completion: (id, shard) marks of pieces spliced into
   * `segment` at their insertion's anchor for display only — the
   * zipper never contains them. Shard-precise so a ghost closer
   * doesn't gray its tile's real opener. Empty = no ghost. */
  ghost_marks: list((Id.t, option(int))),
  /* Witness sub-token styling: (tile id, shard idx) -> typed_len. A
   * witness delimiter (in / => / -> / then) is a REAL completed shard
   * whose first typed_len chars the user typed; the view renders that
   * prefix normal and the remainder ghost. */
  typed_lens: list(((Id.t, int), int)),
  /* Witness caret anchors (partial token id -> reified tile/shard +
   * typed_len): a replaced-witness caret maps to the reified shard.
   * DisplayCaret.point reads this. */
  caret_witnesses: list((Id.t, (Id.t, int, int))),
  /* Edit-armed ghost state: set by an edit, cleared by any other
   * action (Editor.Update). While armed, a statics refresh re-forks
   * the display — statics are DEBOUNCED during typing, so the frame
   * that has fresh assist data is the deferred refresh, not the edit
   * frame itself. Movement never arms: activation stays edit-only. */
  ghost_armed: bool,
  /* persist-ratchet state carried across frames (Persist mode) */
  persist_known: list(string),
  persist_held: list(string),
  /* the inline_persist flag this cache was built under — a flip
     forces a re-fork so the toggle takes effect immediately */
  persist_on: Language.CoreSettings.persist_mode,
  /* THE assist stream (A1 single source), assembled frame-fresh by
   * PromiseRender.mk from this frame's syntax + statics' type facts.
   * Cached here because it depends only on (erased segment,
   * obligations) — caret-free — so movement frames reuse it.
   * Chips, the inline ghost, and Tab all read this one list. */
  assist: list(CanonicalCompletion.insertion),
  /* insertions actually ghosted this frame (physical members of
   * assist) — chip suppression matches exactly these */
  ghosted: list(CanonicalCompletion.insertion),
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

/* `obligations = None` means the assist machinery is off (settings
 * gate, init paths): no assist stream, no ghost. Some(obs) threads
 * the frame's type obligations to THE display projection
 * (PromiseRender.mk), the single zipper→displayed-segment pipeline
 * shared with the test harness. */
let mk =
    (
      ~info_map,
      ~dyn_map,
      ~elaborated=None,
      ~obligations=None,
      ~armed=false,
      ~inline_persist=Language.CoreSettings.Off,
      ~persist_state=([], []),
      ~persist_edit=false,
      z,
    )
    : t => {
  /* THE live display path is the PROMISE RENDER: the display projected
   * from the single reified artifact (PromiseRender.mk), replacing the
   * reconstruction fork. DisplayFork remains the shared home for the
   * assist stream / view-policy layer both call. */
  let fork =
    switch (obligations) {
    | Some(obligations) =>
      PromiseRender.mk(
        ~info_map,
        ~obligations,
        ~armed,
        ~inline_persist,
        ~persist_state,
        ~persist_edit,
        z,
      )
    | None => DisplayFork.plain(z)
    };
  let DisplayFork.{
    segment,
    ghost_marks,
    typed_lens,
    caret_witnesses,
    assist,
    ghosted,
    persist_known: fork_known,
    persist_held: fork_held,
    parsed,
  } = fork;
  let MakeTerm.{term: _, terms, projectors, projector_list, term_data} = parsed;
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
    Measured.of_segment(segment, projector_shapes, refractor_rows);
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
    missing_shards: Segment.global_missing_shards(segment),
    shape_info_map: info_map,
    shape_dyn_map: dyn_map,
    shape_elaborated: elaborated,
    ghost_marks,
    typed_lens,
    caret_witnesses,
    ghost_armed: false,
    persist_known: fork_known,
    persist_held: fork_held,
    persist_on: inline_persist,
    assist,
    ghosted,
  };
};

let init = (z: Zipper.t) =>
  mk(z, ~info_map=Id.Map.empty, ~dyn_map=Id.Map.empty);

let mark_old: t => t =
  old => {
    ...old,
    old: true,
  };

/* statics or refractor model changed but the segment didn't: reuse
 * segment/term_data, recompute only the shape-derived fields. */
let refresh_shapes =
    (
      z: Zipper.t,
      info_map,
      dyn_map,
      ~elaborated=None,
      ~obligations=None,
      old: t,
    ) => {
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
  let measured = Measured.of_segment(old.segment, shape_map, refractor_rows);
  /* the assist stream depends on obligations, which derive from
   * statics — a statics refresh refreshes it too (a dyn/elaborated
   * -only refresh recomputes the same value; obligations gone means
   * assist off) */
  let assist =
    switch (obligations) {
    | Some(obligations) when info_map !== old.shape_info_map =>
      TypeObligations.assist_stream(z, ~info_map, obligations)
    | Some(_) => old.assist
    | None => []
    };
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
    assist,
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
    (
      z: Zipper.t,
      info_map,
      dyn_map,
      ~elaborated=None,
      ~obligations=None,
      ~armed=false,
      ~inline_persist=Language.CoreSettings.Off,
      ~persist_edit=false,
      old: t,
    ) => {
  let refractor_inputs_changed =
    z.refractors.manuals !== old.cached_manuals
    || z.refractors.multis.ephemerals !== old.cached_ephemerals;
  if (old.old) {
    mk(
      z,
      ~info_map,
      ~dyn_map,
      ~elaborated,
      ~obligations,
      ~armed,
      ~inline_persist,
      ~persist_state=(old.persist_known, old.persist_held),
      ~persist_edit,
    );
  } else if (info_map !== old.shape_info_map
             || dyn_map !== old.shape_dyn_map
             || !elaborated_phys_eq(elaborated, old.shape_elaborated)
             || refractor_inputs_changed) {
    refresh_shapes(z, info_map, dyn_map, ~elaborated, ~obligations, old);
  } else {
    {
      ...old,
      selection_ids: Selection.selection_ids(z.selection),
    };
  };
};

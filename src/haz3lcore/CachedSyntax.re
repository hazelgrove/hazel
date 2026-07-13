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
  /* Edit-armed ghost state: set by an edit, cleared by any other
   * action (Editor.Update). While armed, a statics refresh re-forks
   * the display — statics are DEBOUNCED during typing, so the frame
   * that has fresh assist data is the deferred refresh, not the edit
   * frame itself. Movement never arms: activation stays edit-only. */
  ghost_armed: bool,
  /* THE assist stream (A1 single source), assembled frame-fresh by
   * DisplayFork.mk from this frame's syntax + statics' type facts.
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

/* `obligations = None` means the assist machinery is off (settings
 * gate, init paths): no assist stream, no ghost. Some(obs) threads
 * the frame's type obligations to THE display fork (DisplayFork.mk),
 * the single zipper→displayed-segment pipeline shared with the test
 * harness. */
let mk =
    (
      ~info_map,
      ~dyn_map,
      ~elaborated=None,
      ~obligations=None,
      ~armed=false,
      z,
    )
    : t => {
  let fork =
    switch (obligations) {
    | Some(obligations) => DisplayFork.mk(~info_map, ~obligations, ~armed, z)
    | None => DisplayFork.plain(z)
    };
  let DisplayFork.{segment, ghost_marks, assist, ghosted, parsed} = fork;
  let MakeTerm.{term: _, terms, projectors, projector_list, term_data} = parsed;
  let (projector_shapes, projector_errors) =
    ProjectorInfo.ShapeMapSemantics.mk(
      projectors,
      z.refractors,
      info_map,
      dyn_map,
      ~elaborated,
    );
  let refractor_shape_map = Id.Map.empty; // z.refractors.map |> Id.Map.map(_p => 2);
  let measured =
    Measured.of_segment(segment, projector_shapes, refractor_shape_map);
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
    projector_errors,
    missing_shards: Segment.global_missing_shards(segment),
    shape_info_map: info_map,
    shape_dyn_map: dyn_map,
    shape_elaborated: elaborated,
    ghost_marks,
    ghost_armed: false,
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

/* Recompute only the statics-derived fields (shape_map, projector_errors,
 * measured) while reusing the segment/term_data from a prior `mk` pass.
 * Used on refresh-only frames: statics changed but the segment did not,
 * so a full `mk` would be wasteful but shapes/measured need the new
 * elaborated expression (e.g. TableProj placeholder size). */
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
  let refractor_shape_map = Id.Map.empty;
  let measured =
    Measured.of_segment(old.segment, shape_map, refractor_shape_map);
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
    projector_errors,
    measured,
    shape_info_map: info_map,
    shape_dyn_map: dyn_map,
    shape_elaborated: elaborated,
    assist,
  };
};

/* Physical equality on option(Exp.t): `None === None` holds (shared
 * immediate), but `Some(x) === Some(y)` is always false (new box). Hit the
 * cache when the underlying Exp.t ref matches — same stability guarantee
 * as info_map/dyn_map, which are persistent Id.Maps compared by ref. */
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
      old: t,
    ) =>
  if (old.old) {
    mk(z, ~info_map, ~dyn_map, ~elaborated, ~obligations, ~armed);
  } else if (info_map !== old.shape_info_map
             || dyn_map !== old.shape_dyn_map
             || !elaborated_phys_eq(elaborated, old.shape_elaborated)) {
    refresh_shapes(z, info_map, dyn_map, ~elaborated, ~obligations, old);
  } else {
    {
      ...old,
      selection_ids: Selection.selection_ids(z.selection),
    };
  };

type splice = {
  segment: Segment.t,
  measured: Measured.t,
  /* A list of projector IDs in the order they appear in the segment
   * (allows actions to refer to projectors by index) */
  projector_list: list(Id.t),
};

type t = {
  old: bool,
  main_splice: splice,
  /* Cached sub-editor data for every splice in the segment (including
   * splices nested inside projector syntax), keyed by splice id. Each
   * entry's measured is in the splice's own coordinate frame, so a
   * sub-editor can be handed a `splice` and operate self-sufficiently
   * (the `main_splice` switcheroo in CodeEditable.View.view). */
  splices: Id.Map.t(splice),
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
  cached_backpack: list(Tile.t),
  /* Inputs last used to compute shape_map/projector_errors/measured.
   * Kept so `calculate` can detect when statics changed and refresh
   * shapes automatically — callers don't need to plumb that signal. */
  shape_info_map: Language.Statics.Map.t,
  shape_dyn_map: Language.Dynamics.Map.t,
  shape_elaborated: option(Language.Exp.t),
};

// should not be serializing
let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

/* Projectors visible in a splice's own frame: those in its content,
 * including inside tile children, but not across nested splice or
 * projector boundaries (those belong to other sub-editors). */
let splice_projector_list = Segment.frame_projector_ids;

let mk_splice_map =
    (segment: Segment.t, shape_map: ProjectorCore.Shape.Map.t)
    : Id.Map.t(splice) =>
  Segment.splices(segment)
  |> List.fold_left(
       (acc, s: Base.splice) =>
         Id.Map.add(
           s.id,
           {
             segment: s.content,
             measured:
               Measured.of_segment(s.content, shape_map, Id.Map.empty),
             projector_list: splice_projector_list(s.content),
           },
           acc,
         ),
       Id.Map.empty,
     );

let mk = (~info_map, ~dyn_map, ~elaborated=None, z): t => {
  let segment = Zipper.unselect_and_zip(z);
  let MakeTerm.{term: _, terms, projectors, projector_list, term_data} =
    MakeTerm.go(segment);
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
    main_splice: {
      segment,
      measured,
      /* Frame-local render list: top-level projectors only. Nested
       * projectors render in their host splice's sub-editor, and are
       * listed in that splice's cache entry. The global
       * [projector_list] below keeps every projector for action-index
       * resolution. */
      projector_list: splice_projector_list(segment),
    },
    splices: mk_splice_map(segment, projector_shapes),
    term_data,
    selection_ids: Selection.selection_ids(z.selection),
    terms,
    projectors,
    projector_list,
    shape_map: projector_shapes,
    projector_errors,
    cached_backpack: Segment.global_missing_shards(segment),
    shape_info_map: info_map,
    shape_dyn_map: dyn_map,
    shape_elaborated: elaborated,
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
    (z: Zipper.t, info_map, dyn_map, ~elaborated=None, old: t) => {
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
    Measured.of_segment(
      old.main_splice.segment,
      shape_map,
      refractor_shape_map,
    );
  {
    ...old,
    main_splice: {
      ...old.main_splice,
      measured,
    },
    splices: mk_splice_map(old.main_splice.segment, shape_map),
    shape_map,
    projector_errors,
    shape_info_map: info_map,
    shape_dyn_map: dyn_map,
    shape_elaborated: elaborated,
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
let calculate = (z: Zipper.t, info_map, dyn_map, ~elaborated=None, old: t) =>
  if (old.old) {
    mk(z, ~info_map, ~dyn_map, ~elaborated);
  } else if (info_map !== old.shape_info_map
             || dyn_map !== old.shape_dyn_map
             || !elaborated_phys_eq(elaborated, old.shape_elaborated)) {
    refresh_shapes(z, info_map, dyn_map, ~elaborated, old);
  } else {
    {
      ...old,
      selection_ids: Selection.selection_ids(z.selection),
    };
  };

let measured = (syntax: t) => syntax.main_splice.measured;
let segment = (syntax: t) => syntax.main_splice.segment;
let splice_opt = (id: Id.t, syntax: t): option(splice) =>
  Id.Map.find_opt(id, syntax.splices);

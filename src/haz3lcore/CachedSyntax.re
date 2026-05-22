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
  /* Per-refractor extra-row reservations. Refractors overlay the
   * underlying syntax rather than replacing it, so only the vertical
   * count is meaningful here. Built in mk by running each refractor's
   * placeholder and reducing the resulting Shape.vertical to an int.
   * Both Measured.of_segment and Code.view consume this; if either
   * side disagrees with the other, decorations drift out of sync with
   * caret/text positions. */
  refractor_shape_map: Id.Map.t(int),
  /* References to the refractor stores this cache was built from.
   * Compared by physical equality in `calculate` to detect refractor
   * model mutations (e.g. probe drawer-mode toggle). The Zipper is
   * functional, so any SetModel touching a refractor produces a fresh
   * map reference; if these match the new zipper's, the shape map is
   * still valid and we skip the rebuild. */
  cached_manuals: Refractors.RefractorList.t,
  cached_ephemerals: Refractors.Map.t,
  /* The dynamics map the placeholder pass last consumed. Reference-
   * compared in `calculate` so the shape map rebuilds when new samples
   * arrive from the worker (probe drawer heights depend on dynamics). */
  cached_dyn_map: Language.Dynamics.Map.t,
  cached_backpack: list(Tile.t),
};

// should not be serializing
let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

let mk = (~info_map, ~dyn_map, z): t => {
  let segment = Zipper.unselect_and_zip(z);
  let MakeTerm.{term: _, terms, projectors, projector_list, term_data} =
    MakeTerm.go(segment);
  let projector_shapes =
    ProjectorInfo.ShapeMapSemantics.mk(
      projectors,
      z.refractors,
      info_map,
      dyn_map,
    );
  /* Build refractor shape map by running each refractor's placeholder
   * and extracting the deferred-linebreak count. Refractors overlay
   * existing syntax, so only the vertical row count is meaningful;
   * the int stored is "extra rows to reserve after the refractor's
   * underlying tile". Probe drawer-mode is the current consumer. */
  let refractor_shape_map: Id.Map.t(int) = {
    let entries =
      Id.Map.union(
        (_, _, b) => Some(b),
        z.refractors.manuals |> Id.Map.of_list,
        z.refractors.multis.ephemerals,
      );
    Id.Map.mapi(
      (id, entry: Refractors.entry) => {
        let p = Refractors.to_projector(id, entry);
        let info =
          ProjectorInfo.mk_info(
            p,
            ~sample_focus=z.refractors.sample_focus,
            ~statics=info_map,
            ~dynamics=dyn_map,
          );
        let (module P) = ProjectorInit.to_module(entry.kind);
        let shape = P.placeholder(entry.model, info);
        switch (shape.vertical) {
        | Inline
        | Block(0)
        | Tab(0) => 0
        | Tab(n)
        | Block(n) => n
        };
      },
      entries,
    );
  };
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
    refractor_shape_map,
    cached_manuals: z.refractors.manuals,
    cached_ephemerals: z.refractors.multis.ephemerals,
    cached_dyn_map: dyn_map,
    cached_backpack: Segment.global_missing_shards(segment),
  };
};

let init = (z: Zipper.t) =>
  mk(z, ~info_map=Id.Map.empty, ~dyn_map=Id.Map.empty);

let mark_old: t => t =
  old => {
    ...old,
    old: true,
  };

/* Row delta to scroll `#main` by to keep the caret stable across a
 * refractor_shape_map rebuild. Sums the change in row count for each
 * refractor whose underlying tile sits strictly above the caret in
 * the new layout — those are the ones whose growth/shrinkage pushed
 * the caret vertically. Refractors at or below the caret don't affect
 * caret Y. Uses new_.measured throughout (the new zipper's caret
 * pieces are guaranteed to be present there; old.measured can lag on
 * edits). O(refractors). */
let caret_row_shift = (~old: t, ~new_: t, z: Zipper.t): int => {
  let caret_row = Zipper.Caret.point(new_.measured, z).row;
  let above_caret = (id: Id.t): bool =>
    switch (Measured.find_by_id(id, new_.measured)) {
    | Some(m) => m.origin.row < caret_row
    | None => false
    };
  Id.Map.fold(
    (id, new_n, acc) =>
      if (above_caret(id)) {
        let old_n =
          Id.Map.find_opt(id, old.refractor_shape_map)
          |> Option.value(~default=0);
        acc + (new_n - old_n);
      } else {
        acc;
      },
    new_.refractor_shape_map,
    0,
  );
};

let calculate = (z: Zipper.t, info_map, dyn_map, old: t) => {
  /* Detect refractor model mutations (e.g. probe drawer-mode toggle)
   * cheaply by reference-comparing the maps we built from previously.
   * If either reference differs, the shape map may have changed and we
   * rebuild. Caret moves leave these references intact. */
  let refractor_inputs_changed =
    z.refractors.manuals !== old.cached_manuals
    || z.refractors.multis.ephemerals !== old.cached_ephemerals;
  /* Detect dynamics arriving from the worker. Probe drawer heights
   * depend on dynamics (DrawerHeight.compute reads samples), so a new
   * dyn_map can change `Tab(n)` values even with z untouched. */
  let dynamics_changed = dyn_map !== old.cached_dyn_map;
  if (old.old || refractor_inputs_changed || dynamics_changed) {
    let new_ = mk(z, ~info_map, ~dyn_map);
    let shift = caret_row_shift(~old, ~new_, z);
    if (shift != 0) {
      ScrollCompensation.add(shift);
    };
    new_;
  } else {
    {
      ...old,
      selection_ids: Selection.selection_ids(z.selection),
    };
  };
};

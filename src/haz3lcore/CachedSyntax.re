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
  /* Since the introduction of shape_map below, caching projectors
   * here is almost vesigial (currently used only for error deco) */
  projectors: Id.Map.t(Base.projector),
  /* The shape_map is used to leave space for projectors in the
   * underlying editor. In principle calculating this can involve
   * both static and dynamic information, so we cache this for perf */
  shape_map: ProjectorCore.Shape.Map.t,
  cached_backpack: list(Tile.t),
};

// should not be serializing
let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

let mk = (~info_map, ~inference_map, ~dyn_map, z): t => {
  let segment = Zipper.unselect_and_zip(z);
  let MakeTerm.{term: _, terms, projectors, term_data} =
    MakeTerm.go(segment);
  let projector_shapes =
    ProjectorInfo.ShapeMapSemantics.mk(
      projectors,
      info_map,
      inference_map,
      dyn_map,
    );
  {
    old: false,
    segment,
    term_data,
    measured: Measured.of_segment(segment, projector_shapes),
    selection_ids: Selection.selection_ids(z.selection),
    terms,
    projectors,
    shape_map: projector_shapes,
    cached_backpack: Segment.global_missing_shards(segment),
  };
};

let init = (z: Zipper.t) =>
  mk(
    z,
    ~info_map=Id.Map.empty,
    ~inference_map=Language.SolutionMap.empty,
    ~dyn_map=Id.Map.empty,
  );

let mark_old: t => t =
  old => {
    ...old,
    old: true,
  };

let calculate = (z: Zipper.t, info_map, inference_map, dyn_map, old: t) =>
  old.old
    ? mk(z, ~info_map, ~inference_map, ~dyn_map)
    : {
      ...old,
      selection_ids: Selection.selection_ids(z.selection),
    };

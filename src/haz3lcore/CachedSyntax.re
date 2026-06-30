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
  /* extra rows reserved per refractor; Measured.of_segment and Code.view
   * must agree on these or decorations drift from caret/text. */
  refractor_shape_map: Id.Map.t(int),
  /* compared by physical eq in `calculate`: unchanged refractor refs
   * mean the shape map is still valid and we skip the rebuild. */
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

let mk_refractor_shape_map =
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
  Id.Map.mapi(
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
      | Tab(0) => 0
      | Tab(n)
      | Block(n) => n
      };
    },
    entries,
  );
};

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
  let refractor_shape_map =
    mk_refractor_shape_map(z, term_data, info_map, dyn_map, ~elaborated);
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
  let refractor_shape_map =
    mk_refractor_shape_map(z, old.term_data, info_map, dyn_map, ~elaborated);
  let measured =
    Measured.of_segment(old.segment, shape_map, refractor_shape_map);
  {
    ...old,
    shape_map,
    refractor_shape_map,
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

let calculate = (z: Zipper.t, info_map, dyn_map, ~elaborated=None, old: t) => {
  let refractor_inputs_changed =
    z.refractors.manuals !== old.cached_manuals
    || z.refractors.multis.ephemerals !== old.cached_ephemerals;
  if (old.old) {
    mk(z, ~info_map, ~dyn_map, ~elaborated);
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

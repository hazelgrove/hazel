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
  /* splice id → (host projector id, row offset of the splice within
   * the host's placeholder block). Computed alongside shape_map from
   * each projector's splice_rows; lets main-frame decorations find the
   * document row a splice's contents are laid out on. */
  splice_layout: Id.Map.t((Id.t, int)),
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
      /* Refractors carry no splices of their own -- to_projector wraps
         the term's syntax -- and the probe kinds routed here ignore
         the argument, so an all-zero size is exact, not a stand-in. */
      let splice_size = _ => Util.Point.zero;
      let shape = P.placeholder(entry.model, info, splice_size);
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

let mk = (~info_map, ~dyn_map, ~elaborated=None, z): t => {
  let segment = Zipper.unselect_and_zip(z);
  let MakeTerm.{term: _, terms, projectors, projector_list, term_data} =
    MakeTerm.go(segment);
  let (projector_shapes, splice_layout, projector_errors) =
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
    refractor_rows,
    cached_manuals: z.refractors.manuals,
    cached_ephemerals: z.refractors.multis.ephemerals,
    projector_errors,
    splice_layout,
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
  let (shape_map, splice_layout, projector_errors) =
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
  let measured =
    Measured.of_segment(old.main_splice.segment, shape_map, refractor_rows);
  {
    ...old,
    main_splice: {
      ...old.main_splice,
      measured,
    },
    splices: mk_splice_map(old.main_splice.segment, shape_map),
    shape_map,
    refractor_rows,
    projector_errors,
    splice_layout,
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

let measured = (syntax: t) => syntax.main_splice.measured;
let segment = (syntax: t) => syntax.main_splice.segment;
let splice_opt = (id: Id.t, syntax: t): option(splice) =>
  Id.Map.find_opt(id, syntax.splices);

/* The splice whose content contains the piece/term [id], if any.
 * Splice interiors are merged into the main measured map in their own
 * (splice-local) coordinate frame, so main-frame decorations must not
 * position themselves directly from such ids — see doc_row_of_splice
 * for the vertical remap. */
let splice_containing_id = (id: Id.t, syntax: t): option((Id.t, splice)) => {
  let candidates =
    Id.Map.bindings(syntax.splices)
    |> List.filter(((_, s: splice)) =>
         Measured.find_by_id_quiet(id, s.measured) != None
       );
  /* Splice contents are measured recursively, so an id inside a nested
   * splice is found in every enclosing splice's map too; the id's own
   * frame is the innermost candidate — the one containing no other. */
  List.find_opt(
    ((_, s: splice)) =>
      !
        List.exists(
          ((sid, _)) => Measured.has_splice_info(sid, s.measured),
          candidates,
        ),
    candidates,
  );
};

/* Whether the frame identified by [frame] (None = the root editor,
 * Some(sid) = splice sid's sub-editor) owns the on-screen position of
 * [id]: an id inside a splice is positioned only in that splice's own
 * frame, so other frames must not draw decorations anchored on it. */
let frame_owns_id = (frame: option(Id.t), id: Id.t, syntax: t): bool =>
  switch (frame, splice_containing_id(id, syntax)) {
  | (None, None) => true
  | (Some(frame_sid), Some((sid, _))) => Id.equal(frame_sid, sid)
  | (None, Some(_))
  | (Some(_), None) => false
  };

/* The document row on which the splice's contents begin: the host
 * projector's origin row, plus the splice's row offset within the
 * host's block (splice_layout), climbing recursively when the host
 * itself sits inside another splice. This is a grid-level estimate of
 * the projector's DOM layout — exact horizontal positions inside the
 * projector remain unknowable at this level. */
let rec doc_row_of_splice = (sid: Id.t, syntax: t): option(int) => {
  open Util.OptUtil.Syntax;
  let* (host, offset) = Id.Map.find_opt(sid, syntax.splice_layout);
  let+ host_row =
    switch (splice_containing_id(host, syntax)) {
    | Some((outer_sid, outer)) =>
      let* m = Measured.find_by_id_quiet(host, outer.measured);
      let+ outer_row = doc_row_of_splice(outer_sid, syntax);
      outer_row + m.origin.row;
    | None =>
      Option.map(
        (m: Measured.measurement) => m.origin.row,
        Measured.find_by_id_quiet(host, syntax.main_splice.measured),
      )
    };
  host_row + offset;
};

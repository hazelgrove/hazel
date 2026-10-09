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
  cached_stepping: option(ProjectorBase.stepping),
  /* ProbeProj.Settings.layout when the rows were computed */
  cached_layout: int,
  cached_proofs: Refractors.Map.t,
  /* Errors reported by projectors (e.g. "can't render as table") */
  projector_errors: Id.Map.t(ProjectorBase.error),
  missing_shards: list(Tile.t),
  /* Inputs last used to compute shape_map/projector_errors/measured.
   * Kept so `calculate` can detect when statics changed and refresh
   * shapes automatically — callers don't need to plumb that signal. */
  shape_info_map: Language.Statics.Map.t,
  shape_dyn_map: Language.Dynamics.Map.t,
  shape_elaborated: option(Language.Exp.t),
  /* incremental measure/parse memos; carried via {...old}, so each
     editor keeps its own */
  m_cache: Measured.Incr.cache,
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

/* a probe's drawer rows go under its term's last line. Measured and the
   code view add a tile's rows at the linebreak after it, so they're keyed
   by the term's last top-level tile: the anchor (an infix operator, say)
   can sit lines above the term's end, where the drawer is drawn */
let rows_key = (id: Id.t, term_data: TermData.t): Id.t =>
  switch (TermData.segment(id, term_data)) {
  | Some(seg) =>
    List.fold_left(
      (key, p: Piece.t) =>
        switch (p) {
        | Tile(t) => t.id
        | _ => key
        },
      id,
      seg,
    )
  | None => id
  };

/* the ⇓ probe's drawer reserves no rows: it's drawn after the program's
   last line (RefractorView), past any trailing blank lines */
let is_program_value = (z: Zipper.t, id: Id.t): bool =>
  z.refractors.tail_target == Some(id)
  && Id.Map.mem(id, z.refractors.multis.ephemerals);

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
    )
    |> Id.Map.filter((id, _) => !is_program_value(z, id))
    |> Id.Map.union((_, a, _) => Some(a), _, z.refractors.proofs);
  /* proofs keep their theorem tile: their rows go under its own line */
  let rekey = rows =>
    Id.Map.fold(
      (id, n, acc) => {
        let key =
          Id.Map.mem(id, z.refractors.proofs) ? id : rows_key(id, term_data);
        Id.Map.update(
          key,
          fun
          | Some(m) => Some(max(m, n))
          | None => Some(n),
          acc,
        );
      },
      rows,
      Id.Map.empty,
    );
  rekey @@
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
          ~stepping=z.refractors.stepping,
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
  /* only Exp/Mod roots parse incrementally; other (small) roots parse
     once at their own sort, which the Exp-rooted [go] would misparse */
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
    cached_stepping: z.refractors.stepping,
    cached_layout: ProbeProj.Settings.layout^,
    cached_proofs: z.refractors.proofs,
    projector_errors,
    missing_shards: Segment.global_missing_shards_incr(segment),
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
  /* Measured depends only on the segment, shapes, and refractor rows;
   * when new dynamics leave them all unchanged (the common case for a
   * sample refresh), the whole-program re-measure is pure waste. Keep
   * the old shape_map ref too so downstream phys-eq caches stay warm.
   * (Re-measures go through the chunk cache: only changed chunks.) */
  let shapes_equal =
    Id.Map.equal((a, b) => a == b, shape_map, old.shape_map)
    && refractor_rows === old.refractor_rows;
  let (shape_map, measured) =
    shapes_equal
      ? (old.shape_map, old.measured)
      : (
        shape_map,
        Measured.Incr.of_segment(
          ~cache=old.m_cache,
          old.segment,
          shape_map,
          refractor_rows,
        ),
      );
  {
    ...old,
    shape_map,
    refractor_rows,
    projector_errors,
    measured,
    cached_manuals: z.refractors.manuals,
    cached_ephemerals: z.refractors.multis.ephemerals,
    cached_stepping: z.refractors.stepping,
    cached_layout: ProbeProj.Settings.layout^,
    cached_proofs: z.refractors.proofs,
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

/* cost follows the change: new segment → full `mk`; new statics,
 * dynamics or refractor inputs → refresh_shapes; else just selection_ids */
let calculate =
    (~root=Sort.Exp, z: Zipper.t, info_map, dyn_map, ~elaborated=None, old: t) => {
  let refractor_inputs_changed =
    z.refractors.manuals !== old.cached_manuals
    || z.refractors.multis.ephemerals !== old.cached_ephemerals
    || z.refractors.stepping != old.cached_stepping
    || ProbeProj.Settings.layout^ != old.cached_layout
    || z.refractors.proofs !== old.cached_proofs;
  if (old.old) {
    /* [old] marks caret moves too; an unchanged segment keeps its
       measured/terms/term_data */
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
    /* keep the record's identity when nothing changed: view memos key
       on it, and every non-editor update (a streamed chat token) lands
       here */
    let selection_ids = Selection.selection_ids(z.selection);
    selection_ids == old.selection_ids
      ? old
      : {
        ...old,
        selection_ids,
      };
  };
};

open Util;
open OptUtil.Syntax;

/* Projection logic is based on selection and parenthesization.
 * If there is no current selection, we select the currently indicated
 * term. In this case, it is assured that the syntax to be projected
 * is both a term in isolation and a subterm of the containing term.
 * However, if there is already a selection, it could be that the
 * selection is a term in isolation, but NOT a subterm of the containing
 * term. An example of this is `1+2`, where the full syntax is `1+2*3`.
 * In these cases, we project anyway, under the logic that projection is
 * (a kind of) parenthesization. That is, it changes the semantics of the
 * program precisely when parenthesization would. Note that in some cases
 * this can result in not static but syntactic errors; for example, if we
 * project `x:Int` within `x:Int->Bool`. Again, this is done under the
 * logic of doing the same thing parenthesis-wrapping would do, for the
 * purposes of predictability. Similarly, when we unproject something, the
 * surrounding lexical context may have changed, so even if the projected
 * syntax was a subterm of the surrounding term when projected, it will
 * not necessarily be when it is unprojected; for example, if the projected
 * syntax is rooted at an infix expression, and after projection a
 * neighboring infix operation was added which binds tighter. Again,
 * this is the same as would happen if unparenthesizing a subterm. */
/* Get the root term ID from a segment, if it's a well-formed term */
let seg_root_id = (seg: Base.segment): option(Id.t) =>
  try(Some(Segment.root_id(Segment.skel(seg), seg))) {
  | _ => None
  };

/* Initialize a projector on a segment. For projectors with
 * elaborate_syntax=true, if the raw syntax doesn't pass init,
 * validates against the elaborated form but stores the original
 * syntax. The elaborated expression is used at view time instead. */
let init =
    (
      kind: ProjectorCore.Kind.t,
      seg: Base.segment,
      ~elaborated: Language.Exp.t,
    )
    : option(Base.piece) => {
  let (module P) = ProjectorInit.to_module(kind);
  let any = MakeTerm.for_projection(seg);

  /* Try raw syntax first; for elaborate_syntax projectors, fall back to the
     elaborated form keyed by the term's id. */
  switch (Option.bind(any, ProjectorInit.init(kind, seg, _)), any) {
  | (Some(_) as result, _) => result
  | (None, Some(Exp(exp))) when P.elaborate_syntax =>
    let* elab_exp =
      Language.Exp.find_by_id(Language.Exp.rep_id(exp), elaborated);
    let+ (model_str, override) = P.init(Exp(elab_exp), seg);
    let syntax = Option.value(override, ~default=seg);
    Base.Projector(ProjectorCore.mk(kind, syntax, model_str));
  | (None, _) => None
  };
};

/* Migrate a refractor from one ID to another (if present) */
let migrate_refractor = (from_id: Id.t, to_id: Id.t, z: Zipper.t): Zipper.t =>
  ZipperBase.update_manuals(
    List.map(((id, entry)) =>
      if (id == from_id) {
        (to_id, entry);
      } else {
        (id, entry);
      }
    ),
    z,
  );

let replace_selection_and_unselect =
    (piece: Base.piece, focus: Direction.t, z: Zipper.t): Zipper.t =>
  z
  |> Zipper.replace_selection(focus, [piece])
  |> Zipper.directional_unselect(focus);

let remove = (seg: Base.segment, focus: Direction.t, z: Zipper.t): Zipper.t =>
  /* If it's a convex tile, unselect; otherwise, leave selection to guarantee you can toggle */
  switch (seg) {
  | [piece] => replace_selection_and_unselect(piece, Right, z)
  | _ => Zipper.replace_selection(focus, seg, z)
  };

let rec unsplice_segment = (seg: Base.segment): Base.segment =>
  List.concat_map(
    (p: Base.piece) =>
      switch (p) {
      | Splice(s) => unsplice_segment(s.content)
      | Tile(t) => [
          Tile({
            ...t,
            children: List.map(unsplice_segment, t.children),
          }),
        ]
      | Projector(_)
      | Grout(_)
      | Secondary(_) => [p]
      },
    seg,
  );

let term_to_segment =
    (~original_syntax: Base.segment, ~preserve_splices: bool, term) =>
  ExpToSegment.any_to_projector_segment(
    ~settings={
      ...
        ExpToSegment.Settings.of_core(
          ~inline=true,
          Language.CoreSettings.off,
        ),
      show_unknown_as_hole: false,
    },
    ~original_syntax,
    ~preserve_splices,
    term,
  );

let update_piece =
    (f: Base.projector => Base.projector, id: Id.t, piece: Base.piece)
    : Base.segment =>
  switch (piece) {
  | Projector(pr) when pr.id == id => [Base.Projector(f(pr))]
  | x => [x]
  };

let update =
    (f: Base.projector => Base.projector, id: Id.t, z: ZipperBase.t)
    : ZipperBase.t =>
  ZipperBase.MapPiece.fast_local_seg(update_piece(f, id), id, z);

let inside_projector = (id: Id.t, z: Zipper.t): bool =>
  List.exists(
    ((ancestor, _)) =>
      switch (ancestor) {
      | Ancestor.Projector({id: projector_id, _}) => projector_id == id
      | _ => false
      },
    z.relatives.ancestors,
  );

let containing_projector = (z: Zipper.t): option(Id.t) =>
  List.find_map(
    ((ancestor, _)) =>
      switch (ancestor) {
      | Ancestor.Projector({id, _}) => Some(id)
      | _ => None
      },
    z.relatives.ancestors,
  );

let update_from_root =
    (f: Base.projector => Base.projector, id: Id.t, z: Zipper.t): Zipper.t => {
  let segment =
    Zipper.unselect_and_zip(z)
    |> ZipperBase.MapPiece.of_segment(update_piece(f, id));
  {
    ...Zipper.unzip(segment),
    refractors: z.refractors,
  };
};

let rec find_projector =
        (id: Id.t, seg: Base.segment): option(Base.projector) =>
  List.find_map(
    (p: Base.piece) =>
      switch (p) {
      | Projector(pr) when pr.id == id => Some(pr)
      | Tile(t) => List.find_map(find_projector(id), t.children)
      | _ => None
      },
    seg,
  );

let remove_from_root = (id: Id.t, z: Zipper.t): option(Zipper.t) => {
  let segment = Zipper.unselect_and_zip(z);
  let* pr = find_projector(id, segment);
  let z =
    switch (seg_root_id(pr.syntax)) {
    | Some(term_id) => migrate_refractor(pr.id, term_id, z)
    | None => z
    };
  let segment =
    ZipperBase.MapPiece.of_segment(
      fun
      | Projector(pr') when pr'.id == id => unsplice_segment(pr'.syntax)
      | p => [p],
      segment,
    );
  Some({
    ...Zipper.unzip(segment),
    refractors: z.refractors,
  });
};

let go =
    (
      term_data: TermData.t,
      a: Action.project,
      z: Zipper.t,
      projector_list: list(Id.t),
      refractor_list: list(Id.t),
      ~elaborated: Language.Exp.t,
      ~root,
    )
    : result(ZipperBase.t, Action.Failure.t) => {
  let projector_idx_to_id = (idx: int): Id.t =>
    List.nth(projector_list, idx);
  let refractor_idx_to_id = (idx: int): Id.t =>
    List.nth(refractor_list, idx);
  let idx_to_id = (kind: ProjectorCore.Kind.t, idx: int): Id.t =>
    ProjectorCore.Kind.is_refractor(kind)
      ? refractor_idx_to_id(idx) : projector_idx_to_id(idx);

  let select_term =
    Select.current_term(
      term_data,
      ~defs_exclude_bodies=false,
      ~case_rules=false,
    );

  let setup_selection = (z: Zipper.t): option((Direction.t, Zipper.t)) =>
    Selection.is_empty(z.selection)
      ? switch (select_term(z), Indicated.direction(z)) {
        | (Some(z), Some(d)) => Some((Direction.toggle(d), z))
        | _ =>
          switch (Indicated.for_index(z)) {
          | Some({piece: Projector(_), side, _}) =>
            let focus = Direction.toggle(side);
            let+ z = Select.local(focus, z);
            (focus, z);
          | _ => None
          }
        }
      : Some((z.selection.focus, z));

  let set_indicated =
      (z: Zipper.t, kind: ProjectorCore.Kind.t): option(Zipper.t) => {
    /* If not projected, project. If already same kind, remove. If other kind, change.
     * Also migrate any refractor on the term to/from the projector. */
    let* (focus, z) = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] when pr.kind == kind =>
      /* Remove projector: restore original syntax */
      let z =
        switch (seg_root_id(pr.syntax)) {
        | Some(term_id) => migrate_refractor(pr.id, term_id, z)
        | None => z
        };
      Some(remove(unsplice_segment(pr.syntax), focus, z));
    | [Projector(pr)] =>
      /* Switch projector kind: migrate refractor to new projector */
      let+ piece = init(kind, unsplice_segment(pr.syntax), ~elaborated);
      let z =
        switch (piece) {
        | Projector(new_pr) => migrate_refractor(pr.id, new_pr.id, z)
        | _ => z
        };
      replace_selection_and_unselect(piece, focus, z);
    | seg =>
      /* Add projector: migrate refractor from term to projector */
      let+ piece = init(kind, seg, ~elaborated);
      let z =
        switch (seg_root_id(seg), piece) {
        | (Some(term_id), Projector(new_pr)) =>
          migrate_refractor(term_id, new_pr.id, z)
        | _ => z
        };
      replace_selection_and_unselect(piece, focus, z);
    };
  };

  let remove_indicated = (z: Zipper.t): option(Zipper.t) => {
    let* (focus, z) = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] =>
      /* Migrate refractor back to underlying term */
      let z =
        switch (seg_root_id(pr.syntax)) {
        | Some(term_id) => migrate_refractor(pr.id, term_id, z)
        | None => z
        };
      Some(remove(unsplice_segment(pr.syntax), focus, z));
    | _ => None
    };
  };

  switch (a) {
  | SetIndicated(Specific(kind)) =>
    switch (set_indicated(z, kind)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | SetIndicated(ChooseLivelit) =>
    switch (
      List.filter_map(
        set_indicated(z),
        ProjectorCore.Kind.livelit_projectors,
      )
    ) {
    | [hd, ..._] => Ok(hd)
    | [] => Error(Cant_project)
    }
  | RemoveIndicated =>
    let removed_from_root = {
      let indicated_projector =
        switch (Indicated.for_index(z)) {
        | Some({piece: Projector({id, _}), _}) => Some(id)
        | _ => None
        };
      let projector_id =
        switch (containing_projector(z)) {
        | Some(_) as id => id
        | None => indicated_projector
        };
      switch (projector_id) {
      | Some(id) => remove_from_root(id, z)
      | None => None
      };
    };
    switch (removed_from_root) {
    | Some(z) => Ok(z)
    | None =>
      switch (remove_indicated(z)) {
      | Some(z) => Ok(z)
      | None => Error(Cant_project)
      }
    };
  | SetSyntax(idx, kind, seg) =>
    let id = idx_to_id(kind, idx);
    /* Strip trailing whitespace/newlines before parenthesizing,
     * as lift_syntax(~inline=false) may append trailing newlines */
    let trimmed_seg =
      seg
      |> Segment.unparenthesize
      |> Segment.trim_secondary(Right)
      |> Segment.trim_secondary(Left);
    let parenthesized_piece = Segment.parenthesize(trimmed_seg);
    if (ProjectorCore.Kind.is_refractor(kind)) {
      let parenthesized_seg = [parenthesized_piece];
      let manual_model =
        List.assoc_opt(id, z.refractors.manuals)
        |> Option.map((pr: Refractors.entry) => pr.model);
      let is_ephemeral = Id.Map.mem(id, z.refractors.multis.ephemerals);
      let do_replace = () => {
        let* (l, r) = TermData.extremes_shards(id, term_data);
        let+ z = Select.shard_range(l, r, z);
        Zipper.replace_selection(Right, parenthesized_seg, z);
      };
      if (is_ephemeral && Option.is_none(manual_model)) {
        switch (do_replace()) {
        | Some(z) => Ok(z)
        | None => Error(Cant_project)
        };
      } else {
        let new_id =
          MakeTerm.from_zip_for_sem(
            Zipper.unzip(~direction=Right, parenthesized_seg),
            ~root,
          ).
            term
          |> Language.Exp.rep_id;
        switch (do_replace()) {
        | Some(z) =>
          let z =
            Zipper.update_manuals(List.filter(((mid, _)) => mid != id), z);
          Ok(ZipperBase.add_manual(~model=?manual_model, new_id, kind, z));
        | None => Error(Cant_project)
        };
      };
    } else {
      Ok(
        update(
          p =>
            {
              ...p,
              syntax: [parenthesized_piece],
            },
          id,
          z,
        ),
      );
    };
  | SetTerm(idx, term, preserve_splices) =>
    let id = projector_idx_to_id(idx);
    let f = (p: Base.projector) => {
      ...p,
      syntax:
        term_to_segment(~original_syntax=p.syntax, ~preserve_splices, term),
    };
    Ok(
      inside_projector(id, z)
        ? update_from_root(f, id, z) : update(f, id, z),
    );
  | SetModel(idx, kind, new_model) =>
    let id = idx_to_id(kind, idx);
    Ok(
      if (ProjectorCore.Kind.is_refractor(kind)) {
        Zipper.update_refractor(
          id,
          fun
          | Some(entry: Refractors.entry) =>
            Some(
              Refractors.{
                kind: entry.kind,
                model: new_model,
              },
            )
          | None => None,
          z,
        );
      } else {
        update(
          pr =>
            {
              ...pr,
              model: new_model,
            },
          id,
          z,
        );
      },
    );
  | Focus(idx, kind, d) =>
    let id = idx_to_id(kind, idx);
    switch (d) {
    | None =>
      /* Focus by pointer click or probe-to-probe navigation */
      let (module P) = ProjectorInit.to_module(kind);
      switch (P.focusable.pointer) {
      | Some(focus) => focus(id)
      | None => ()
      };
      let z = Option.value(~default=z, Move.jump_to_id_indicated(z, id));
      /* Set pending_probe_cursor so the sample focus adapts to the
         newly focused probe. For pointer clicks on a specific sample,
         the subsequent Capture action will override with more specific
         data; for probe-to-probe navigation, most_aligned_sample picks
         the best match. */
      let z =
        Zipper.update_refractors(z, r =>
          {
            ...r,
            pending_probe_cursor: Some([id]),
          }
        );
      Ok(z);
    | Some(Right) =>
      /* Focus by arrow key hand-off */
      let (module P) = ProjectorInit.to_module(kind);
      switch (P.focusable.keyboard) {
      | Some(focus) => focus(id, Right)
      | None => ()
      };
      Ok(z);
    | Some(Left) =>
      /* Focus by arrow key hand-off */
      let (module P) = ProjectorInit.to_module(kind);
      switch (P.focusable.keyboard) {
      | Some(focus) => focus(id, Left)
      | None => ()
      };
      Ok(z);
    };
  | Escape(idx, d) =>
    switch (Move.jump_to_side_of_id(d, z, projector_idx_to_id(idx))) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | EscapeToLineEnd(idx, kind) =>
    switch (Move.jump_to_side_of_id(Right, z, idx_to_id(kind, idx))) {
    | Some(z) => Ok(Option.value(~default=z, Move.to_linebreak(Right, z)))
    | None => Error(Cant_project)
    }
  | SampleFocus(a) => Ok(SampleFocusPerform.go(z, a))
  };
};

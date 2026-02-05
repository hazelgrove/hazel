open ProjectorBase;
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

let init = (kind: ProjectorCore.Kind.t, seg: Base.segment): option(syntax) =>
  /* Projected syntax always gets parenthesized, but only the contents
   * of those parentheses are passed to the projector implementations  */
  switch (MakeTerm.for_projection(seg)) {
  | None => None
  | Some(any) => ProjectorInit.init(kind, Segment.parenthesize(seg), any)
  };

/* Get the root term ID from a segment, if it's a well-formed term */
let seg_root_id = (seg: Base.segment): option(Id.t) =>
  try(Some(Segment.root_id(Segment.skel(seg), seg))) {
  | _ => None
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

let remove = (piece: Base.piece, focus: Direction.t, z: Zipper.t): Zipper.t => {
  let seg = Piece.unparenthesize(piece);
  /* If it's a convex tile, unselect; otherwise, leave selection to guarantee you can toggle */
  switch (seg) {
  | [piece] => replace_selection_and_unselect(piece, Right, z)
  | _ => Zipper.replace_selection(focus, seg, z)
  };
};

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

let go =
    (
      term_data: TermData.t,
      a: Action.project,
      z: Zipper.t,
      projector_list: list(Id.t),
      refractor_list: list(Id.t),
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
        | _ => None
        }
      : Some((z.selection.focus, z));

  let set_indicated =
      (z: Zipper.t, kind: ProjectorCore.Kind.t): option(Zipper.t) => {
    /* If not projected, project. If already same kind, remove. If other kind, change.
     * Also migrate any refractor on the term to/from the projector. */
    let* (focus, z) = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] when pr.kind == kind =>
      /* Remove projector: migrate refractor back to underlying term */
      let underlying_seg = Piece.unparenthesize(pr.syntax);
      let z =
        switch (seg_root_id(underlying_seg)) {
        | Some(term_id) => migrate_refractor(pr.id, term_id, z)
        | None => z
        };
      Some(remove(pr.syntax, focus, z));
    | [Projector(pr)] =>
      /* Switch projector kind: migrate refractor to new projector */
      let+ piece = init(kind, Piece.unparenthesize(pr.syntax));
      let z =
        switch (piece) {
        | Projector(new_pr) => migrate_refractor(pr.id, new_pr.id, z)
        | _ => z
        };
      replace_selection_and_unselect(piece, focus, z);
    | seg =>
      /* Add projector: migrate refractor from term to projector */
      let+ piece = init(kind, seg);
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
      let underlying_seg = Piece.unparenthesize(pr.syntax);
      let z =
        switch (seg_root_id(underlying_seg)) {
        | Some(term_id) => migrate_refractor(pr.id, term_id, z)
        | None => z
        };
      Some(remove(pr.syntax, focus, z));
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
    switch (remove_indicated(z)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | SetSyntax(idx, kind, seg) =>
    let id = idx_to_id(kind, idx);
    // Ensure seg is parenthesized unless it already is
    let parenthesized_piece =
      Segment.unparenthesize(seg) |> Segment.parenthesize;
    if (ProjectorCore.Kind.is_refractor(kind)) {
      let parenthesized_seg = [parenthesized_piece];
      // Check for existing refractors (automatic ephemeral or manual) that control this projector
      let original_id = id;
      let ephemeral_model =
        Id.Map.find_opt(original_id, z.refractors.autos.ephemerals)
        |> Option.map((pr: Refractors.entry) => pr.model);
      let manual_refractor_model =
        List.assoc_opt(original_id, z.refractors.manuals)
        |> Option.map((pr: Refractors.entry) => pr.model);
      switch (manual_refractor_model, ephemeral_model) {
      | (Some(refractor_model), _) =>
        // Manual refractor exists: update the selection to this projector's term range,
        // replace with new syntax, and create a new refractor probe monitoring the updated term
        let new_id =
          MakeTerm.from_zip_for_sem(
            Zipper.unzip(~direction=Right, parenthesized_seg),
          ).
            term
          |> Language.Exp.rep_id;

        let new_z = {
          open OptUtil.Syntax;
          let* (l, r) = TermData.extremes_shards(id, term_data);
          let+ z = Select.shard_range(l, r, z);
          let z = Zipper.replace_selection(Right, parenthesized_seg, z);
          ZipperBase.add_manual(~model=refractor_model, new_id, kind, z);
        };

        switch (new_z) {
        | Some(z) => Ok(z)
        | None => Error(Cant_project)
        };
      | (_, Some(_)) =>
        // Ephemeral refractor exists: update selection and replace syntax,
        // but don't create new refractor as this is handled automatically

        let new_z = {
          open OptUtil.Syntax;
          let* (l, r) = TermData.extremes_shards(id, term_data);
          let+ z = Select.shard_range(l, r, z);
          let z = Zipper.replace_selection(Right, parenthesized_seg, z);
          z;
        };

        switch (new_z) {
        | Some(z) => Ok(z)
        | None => Error(Cant_project)
        };
      | (None, None) => assert(false)
      };
    } else {
      Ok(
        update(
          pr =>
            {
              ...pr,
              syntax: parenthesized_piece,
            },
          id,
          z,
        ),
      );
    };
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
      /* Focus by mouse click */
      let (module P) = ProjectorInit.to_module(kind);
      switch (P.focusable.pointer) {
      | Some(focus) => focus(id)
      | None => ()
      };
      Ok(Option.value(~default=z, Move.jump_to_id_indicated(z, id)));
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
  | SampleCursor(a) => Ok(SampleCursorPerform.go(z, a))
  };
};

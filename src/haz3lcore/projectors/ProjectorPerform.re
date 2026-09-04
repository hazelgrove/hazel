open ProjectorBase;
open Util_web;
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
    : option(syntax) => {
  let (module P) = ProjectorInit.to_module(kind);
  let orig_piece = Segment.parenthesize(seg);
  let any = MakeTerm.for_projection(seg);

  /* Try raw syntax first; for elaborate_syntax projectors, fall back to the
     elaborated form keyed by the term's id. */
  switch (Option.bind(any, ProjectorInit.init(kind, orig_piece, _)), any) {
  | (Some(_) as result, _) => result
  | (None, Some(Exp(exp))) when P.elaborate_syntax =>
    let* elab_exp =
      Language.Exp.find_by_id(Language.Exp.rep_id(exp), elaborated);
    let+ model_str = P.init(Exp(elab_exp));
    Base.Projector(ProjectorCore.mk(kind, orig_piece, model_str));
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
      /* Remove projector: restore original syntax */
      let restore_syntax = pr.syntax;
      let underlying_seg = Piece.unparenthesize(restore_syntax);
      let z =
        switch (seg_root_id(underlying_seg)) {
        | Some(term_id) => migrate_refractor(pr.id, term_id, z)
        | None => z
        };
      Some(remove(restore_syntax, focus, z));
    | [Projector(pr)] =>
      /* Switch projector kind: migrate refractor to new projector */
      let+ piece = init(kind, Piece.unparenthesize(pr.syntax), ~elaborated);
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
      let restore_syntax = pr.syntax;
      let underlying_seg = Piece.unparenthesize(restore_syntax);
      let z =
        switch (seg_root_id(underlying_seg)) {
        | Some(term_id) => migrate_refractor(pr.id, term_id, z)
        | None => z
        };
      Some(remove(restore_syntax, focus, z));
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
      /* Select the term range and replace with new syntax.
       * Don't unselect/remold here — the normal update cycle handles that. */
      let do_replace = () => {
        let* (l, r) = TermData.extremes_shards(id, term_data);
        let+ z = Select.shard_range(l, r, z);
        Zipper.replace_selection(Right, parenthesized_seg, z);
      };
      if (is_ephemeral && Option.is_none(manual_model)) {
        /* Ephemeral refractor: replace syntax only, auto system re-detects */
        switch (do_replace()) {
        | Some(z) => Ok(z)
        | None => Error(Cant_project)
        };
      } else {
        /* Manual or fallback: replace and re-register */
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

/* --- Agent tools: path-resolved syntax projectors (after Select.term) ---
   Placement/removal calls migrate_refractor: wrapping or stripping a projector
   changes the id at that location, and probe/statics overlays are keyed by id,
   so they must be re-keyed to survive. */

let with_selection_after_term =
    (
      ~term_data: TermData.t,
      id: Id.t,
      z: Zipper.t,
      f: (Direction.t, Zipper.t) => option(Zipper.t),
    )
    : option(Zipper.t) => {
  let* z =
    Select.term(
      term_data,
      ~defs_exclude_bodies=false,
      ~case_rules=false,
      id,
      z,
    );
  let* (focus, z) =
    Selection.is_empty(z.selection) ? None : Some((z.selection.focus, z));
  f(focus, z);
};

/** Place [kind] on the term at [id]. If already that kind, leave unchanged. */
let try_place_syntax_projector =
    (
      ~term_data: TermData.t,
      ~elaborated: Language.Exp.t,
      id: Id.t,
      kind: ProjectorCore.Kind.t,
      z: Zipper.t,
    )
    : option(Zipper.t) => {
  with_selection_after_term(~term_data, id, z, (focus, z) =>
    switch (z.selection.content) {
    | [Projector(pr)] when pr.kind == kind => Some(z)
    | [Projector(pr)] =>
      let* piece = init(kind, Piece.unparenthesize(pr.syntax), ~elaborated);
      let z =
        switch (piece) {
        | Projector(new_pr) => migrate_refractor(pr.id, new_pr.id, z)
        | _ => z
        };
      Some(replace_selection_and_unselect(piece, focus, z));
    | seg =>
      let* piece = init(kind, seg, ~elaborated);
      let z =
        switch (seg_root_id(seg), piece) {
        | (Some(term_id), Projector(new_pr)) =>
          migrate_refractor(term_id, new_pr.id, z)
        | _ => z
        };
      Some(replace_selection_and_unselect(piece, focus, z));
    }
  );
};

/** Toggle [kind] on the term at [id] (same as editor menu: same kind removes). */
let try_toggle_syntax_projector =
    (
      ~term_data: TermData.t,
      ~elaborated: Language.Exp.t,
      id: Id.t,
      kind: ProjectorCore.Kind.t,
      z: Zipper.t,
    )
    : option(Zipper.t) => {
  with_selection_after_term(~term_data, id, z, (focus, z) =>
    switch (z.selection.content) {
    | [Projector(pr)] when pr.kind == kind =>
      let underlying_seg = Piece.unparenthesize(pr.syntax);
      let z =
        switch (seg_root_id(underlying_seg)) {
        | Some(term_id) => migrate_refractor(pr.id, term_id, z)
        | None => z
        };
      Some(remove(pr.syntax, focus, z));
    | [Projector(pr)] =>
      let* piece = init(kind, Piece.unparenthesize(pr.syntax), ~elaborated);
      let z =
        switch (piece) {
        | Projector(new_pr) => migrate_refractor(pr.id, new_pr.id, z)
        | _ => z
        };
      Some(replace_selection_and_unselect(piece, focus, z));
    | seg =>
      let* piece = init(kind, seg, ~elaborated);
      let z =
        switch (seg_root_id(seg), piece) {
        | (Some(term_id), Projector(new_pr)) =>
          migrate_refractor(term_id, new_pr.id, z)
        | _ => z
        };
      Some(replace_selection_and_unselect(piece, focus, z));
    }
  );
};

/** Remove a syntax projector on the term at [id], if the selection is a projector. */
let try_remove_syntax_projector =
    (~term_data: TermData.t, id: Id.t, z: Zipper.t): option(Zipper.t) => {
  with_selection_after_term(~term_data, id, z, (focus, z) =>
    switch (z.selection.content) {
    | [Projector(pr)] =>
      let underlying_seg = Piece.unparenthesize(pr.syntax);
      let z =
        switch (seg_root_id(underlying_seg)) {
        | Some(term_id) => migrate_refractor(pr.id, term_id, z)
        | None => z
        };
      Some(remove(pr.syntax, focus, z));
    | _ => None
    }
  );
};

/** Re-validate projectors after an edit: strip any whose underlying syntax no
    longer parses ([MakeTerm.for_projection]) or whose kind no longer
    initializes ([ProjectorInit.init]), migrating probe/statics overlays to the
    exposed term id. */
let revalidate_projectors_in_segment =
    (z: Zipper.t, seg: Base.segment): (Zipper.t, Base.segment, bool) => {
  let rec go_seg =
          (z: Zipper.t, seg: Base.segment): (Zipper.t, Base.segment, bool) => {
    List.fold_left(
      ((z, acc, any_ch), p) => {
        let (z'', parts, p_ch) = go_piece(z, p);
        (z'', acc @ parts, any_ch || p_ch);
      },
      (z, [], false),
      seg,
    );
  }
  and go_piece =
      (z: Zipper.t, piece: Base.piece): (Zipper.t, Base.segment, bool) =>
    switch (piece) {
    | Tile(t) =>
      let (z', children, ch) =
        List.fold_left(
          ((z, rev_chs, any_ch), c) => {
            let (z'', c', c_ch) = go_seg(z, c);
            (z'', [c', ...rev_chs], any_ch || c_ch);
          },
          (z, [], false),
          t.children,
        );
      let children = List.rev(children);
      (
        z',
        [
          Tile({
            ...t,
            children,
          }),
        ],
        ch,
      );
    | Grout(_)
    | Secondary(_) => (z, [piece], false)
    | Projector(pr) =>
      let inner0 = Piece.unparenthesize(pr.syntax);
      let (z1, inner_seg, inner_ch) = go_seg(z, inner0);
      switch (MakeTerm.for_projection(inner_seg)) {
      | None =>
        let z2 =
          switch (seg_root_id(inner_seg)) {
          | Some(tid) => migrate_refractor(pr.id, tid, z1)
          | None => z1
          };
        (z2, inner_seg, true);
      | Some(any) =>
        switch (
          ProjectorInit.init(pr.kind, Segment.parenthesize(inner_seg), any)
        ) {
        | None =>
          let z2 =
            switch (seg_root_id(inner_seg)) {
            | Some(tid) => migrate_refractor(pr.id, tid, z1)
            | None => z1
            };
          (z2, inner_seg, true);
        | Some(syn) => inner_ch ? (z1, [syn], true) : (z1, [piece], false)
        }
      };
    };

  go_seg(z, seg);
};

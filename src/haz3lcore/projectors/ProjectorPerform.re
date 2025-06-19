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

let init =
    (
      ~projector_init,
      ~seg_to_ed: Base.segment('p) => option('ed),
      kind: 'p_kind,
      seg: Base.segment('p),
    )
    : option(Base.piece('p)) => {
  /* Projected syntax always gets parenthesized, but only the contents
   * of those parentheses are passed to the projector implementations  */
  open OptUtil.Syntax;
  let* any =
    MakeTerm.for_projection(
      ~of_projector=(~sort as _, ~id as _, _) => Any(),
      ~log_projector=_ => (),
      seg,
    );
  let+ model = projector_init(kind, any, () => seg_to_ed(seg));
  let sort = Language.Term.Any.sort(any);
  Base.Projector(Base.mk_projector(~sort, ~model));
};

let replace_selection_and_unselect =
    (piece: Base.piece('p), focus: Direction.t, z: Zipper.t('p))
    : Zipper.t('p) =>
  z
  |> Zipper.replace_selection(focus, [piece])
  |> Zipper.directional_unselect(focus);

let remove =
    (seg: Base.segment('p), focus: Direction.t, z: Zipper.t('p))
    : Zipper.t('p) => {
  /* If it's a convex tile, unselect; otherwise, leave selection to guarantee you can toggle */
  switch (seg) {
  | [piece] => replace_selection_and_unselect(piece, Right, z)
  | _ => Zipper.replace_selection(focus, seg, z)
  };
};

let update_piece =
    (
      f: Base.projector('p) => Base.projector('p),
      id: Id.t,
      piece: Base.piece('p),
    )
    : Base.segment('p) =>
  switch (piece) {
  | Projector(pr) when pr.id == id => [Base.Projector(f(pr))]
  | x => [x]
  };

let update =
    (
      f: Base.projector('p) => Base.projector('p),
      id: Id.t,
      z: ZipperBase.t('p),
    )
    : ZipperBase.t('p) =>
  ZipperBase.MapPiece.fast_local_seg(update_piece(f, id), id, z);

let get_model = (id: Id.t, z: ZipperBase.t('p)): option('p) => {
  switch (ZipperBase.FindPiece.in_zipper(x => Piece.id(x) == id, z)) {
  | Some(Projector(pr)) => Some(pr.model)
  | Some(_)
  | None => None
  };
};

let go =
    (
      type p,
      type p_kind,
      type p_a,
      ~seg_to_ed,
      ~projector_init,
      ~update_projector,
      ~seg_of_pr: p => Base.segment(p),
      ~livelit_projectors,
      jump_to_side_of_id,
      select_term: Zipper.t(p) => option(Zipper.t(p)),
      a: Action.project(p_kind, p, p_a),
      z: Zipper.t(p),
    )
    : result(ZipperBase.t(p), Action.Failure.t) => {
  let setup_selection =
      (z: Zipper.t(p)): option((Direction.t, Zipper.t(p))) =>
    Selection.is_empty(z.selection)
      ? switch (select_term(z), Indicated.direction(z)) {
        | (Some(z), Some(d)) => Some((Direction.toggle(d), z))
        | _ => None
        }
      : Some((z.selection.focus, z));

  let set_indicated = (z: Zipper.t(p), kind: p_kind): option(Zipper.t(p)) => {
    /* If not projected, project. If already same kind, remove. If other kind, change */
    // TODO [Matt]: Make this check the kind again
    let* (focus, z) = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] => Some(remove(seg_of_pr(pr.model), focus, z))
    // | [Projector(pr)] =>
    //   let+ piece =
    //     init(~projector_init, kind, Piece.unparenthesize(pr.syntax));
    //   replace_selection_and_unselect(piece, focus, z);
    | seg =>
      let+ piece = init(~projector_init, ~seg_to_ed, kind, seg);
      replace_selection_and_unselect(piece, focus, z);
    };
  };

  let remove_indicated = (z: Zipper.t(p)): option(Zipper.t(p)) => {
    let* (focus, z) = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] => Some(remove(seg_of_pr(pr.model), focus, z))
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
    switch (List.filter_map(set_indicated(z), livelit_projectors)) {
    | [hd, ..._] => Ok(hd)
    | [] => Error(Cant_project)
    }
  | RemoveIndicated =>
    switch (remove_indicated(z)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | Perform(id, action) =>
    Ok(
      update(
        pr =>
          {
            ...pr,
            model: update_projector(~sort=pr.mold.out, ~id, action, pr.model),
          },
        id,
        z,
      ): ZipperBase.t(p),
    )
  | Escape(id, d) => Ok(jump_to_side_of_id(d, z, id))
  };
};

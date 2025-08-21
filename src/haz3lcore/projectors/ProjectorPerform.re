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
      ~seg_to_ed: Base.segment => option('ed),
      kind: 'p_kind,
      seg: Base.segment,
    )
    : option(Base.piece) => {
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
    (piece: Base.piece, focus: Direction.t, z: Zipper.t): Zipper.t =>
  z
  |> Zipper.replace_selection(focus, [piece])
  |> Zipper.directional_unselect(focus);

let remove = (seg: Base.segment, focus: Direction.t, z: Zipper.t): Zipper.t => {
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

let get_model = (id: Id.t, z: ZipperBase.t): option(_) => {
  switch (ZipperBase.FindPiece.in_zipper(x => Piece.id(x) == id, z)) {
  | Some(Projector(pr)) => Some(pr.model)
  | Some(_)
  | None => None
  };
};

let setup_selection =
    (type p, ~select_term: Zipper.t => option(Zipper.t), z: Zipper.t)
    : option((Direction.t, Zipper.t)) =>
  Selection.is_empty(z.selection)
    ? switch (select_term(z), Indicated.direction(z)) {
      | (Some(z), Some(d)) => Some((Direction.toggle(d), z))
      | _ => None
      }
    : Some((z.selection.focus, z));

let remove_indicated =
    (
      ~select_term: Zipper.t => option(Zipper.t),
      ~seg_of_projector,
      z: Zipper.t,
    )
    : option(Zipper.t) => {
  let* (focus, z) = setup_selection(~select_term, z);
  switch (z.selection.content) {
  | [Projector(pr)] => Some(remove(seg_of_projector(pr.model), focus, z))
  | _ => None
  };
};

let set_indicated =
    (
      ~select_term: Zipper.t => option(Zipper.t),
      ~seg_of_projector,
      ~projector_init,
      ~seg_to_ed,
      z: Zipper.t,
      kind,
    )
    : option(Zipper.t) => {
  /* If not projected, project. If already same kind, remove. If other kind, change */
  // TODO [Matt]: Make this check the kind again
  let* (focus, z) = setup_selection(~select_term, z);
  switch (z.selection.content) {
  | [Projector(pr)] => Some(remove(seg_of_projector(pr.model), focus, z))
  // | [Projector(pr)] =>
  //   let+ piece =
  //     init(~projector_init, kind, Piece.unparenthesize(pr.syntax));
  //   replace_selection_and_unselect(piece, focus, z);
  | seg =>
    let+ piece = init(~projector_init, ~seg_to_ed, kind, seg);
    replace_selection_and_unselect(piece, focus, z);
  };
};

let go =
    (
      type p,
      ~seg_to_ed,
      ~projector_init,
      ~update_projector,
      ~seg_of_projector,
      ~livelit_projectors,
      ~jump_to_side_of_id,
      ~select_term: Zipper.t => option(Zipper.t),
      a: Action.project('p_kind, p, 'p_a),
      z: Zipper.t,
    )
    : result(Zipper.t, Action.Failure.t) => {
  let set_indicated =
    set_indicated(
      ~select_term,
      ~seg_of_projector,
      ~projector_init,
      ~seg_to_ed,
    );
  let remove_indicated = remove_indicated(~select_term, ~seg_of_projector);
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
      ): Zipper.t,
    )
  | Escape(id, d) =>
    switch (jump_to_side_of_id(d, z, id)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | MoveCaretTo(target_id) =>
    switch (Indicated.index(z)) {
    | Some(indicated_id) when indicated_id != target_id =>
      /* May as well not flop sides willy nilly if already there */
      switch (jump_to_side_of_id(Left, z, target_id)) {
      | Some(z) => Ok(z)
      | None => Error(Cant_project)
      }
    | _ => Ok(z)
    }
  // | Focus(id, kind, d) =>
  //   switch (d) {
  //   | None =>
  //     /* Focus by mouse click */
  //     let (module P) = ProjectorInit.to_module(kind);
  //     switch (P.focusable.pointer) {
  //     | Some(focus) => focus(id)
  //     | None => ()
  //     };
  //     Ok(Option.value(~default=z, jump_to_id_indicated(z, id)));
  //   | Some(Right) =>
  //     /* Focus by arrow key hand-off */
  //     let (module P) = ProjectorInit.to_module(kind);
  //     switch (P.focusable.keyboard) {
  //     | Some(focus) => focus(id, Right)
  //     | None => ()
  //     };
  //     Ok(z);
  //   | Some(Left) =>
  //     /* Focus by arrow key hand-off */
  //     let (module P) = ProjectorInit.to_module(kind);
  //     switch (P.focusable.keyboard) {
  //     | Some(focus) => focus(id, Left)
  //     | None => ()
  //     };
  //     Ok(z);
  //   }
  // | Escape(id, d) =>
  //   switch (jump_to_side_of_id(d, z, id)) {
  //   | Some(z) => Ok(z)
  //   | None => Error(Cant_project)
  //     }
  };
};

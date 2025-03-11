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

let init =
    (kind: ProjectorCore.Kind.t, projector_id, seg: Base.segment)
    : option(Base.piece) =>
  /* Projected syntax always gets parenthesized, but only the contents
   * of those parentheses are passed to the projector implementations  */
  //TODO(andrew)
  switch (MakeTerm.for_projection(_ => Any(), seg)) {
  | None => None
  | Some(any) => ProjectorInit.init(kind, projector_id, any)
  };

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

// let update_piece =
//     (f: Base.projector => Base.projector, id: Id.t, piece: Base.piece)
//     : Base.segment =>
//   switch (piece) {
//   | Projector(pr) when pr.id == id => [Base.Projector(f(pr))]
//   | x => [x]
//   };

// let update =
//     (f: Base.projector => Base.projector, id: Id.t, z: ZipperBase.t)
//     : ZipperBase.t =>
//   ZipperBase.MapPiece.fast_local_seg(update_piece(f, id), id, z);

let go =
    (
      jump_to_id_indicated, //TODO:
      jump_to_side_of_id,
      select_term: Zipper.t => option(Zipper.t),
      projectors: Id.Map.t(ProjectorBase.trad),
      a: Action.project,
      z: Zipper.t,
    )
    : result(ZipperBase.t, Action.Failure.t) => {
  let setup_selection = (z: Zipper.t): option((Direction.t, Zipper.t)) =>
    Selection.is_empty(z.selection)
      ? switch (select_term(z), Indicated.direction(z)) {
        | (Some(z), Some(d)) => Some((Direction.toggle(d), z))
        | _ => None
        }
      : Some((z.selection.focus, z));

  let set_indicated =
      (z: Zipper.t, projector_id: Id.t, kind: ProjectorCore.Kind.t)
      : option(Zipper.t) => {
    /* If not projected, project. If already same kind, remove. If other kind, change */
    let* (focus, z) = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] =>
      let pr = Id.Map.find_opt(pr.id, projectors);
      switch (pr) {
      | Some(pr) when pr.kind == kind => Some(remove(pr.syntax, focus, z))
      | Some(pr) =>
        let+ piece =
          init(kind, projector_id, Piece.unparenthesize(pr.syntax));
        replace_selection_and_unselect(piece, focus, z);
      | None =>
        prerr_endline("Projector not found");
        None;
      };
    | seg =>
      let+ piece = init(kind, projector_id, seg);
      replace_selection_and_unselect(piece, focus, z);
    };
  };

  // let remove_indicated = (z: Zipper.t): option(Zipper.t) => {
  //   let* (focus, z) = setup_selection(z);
  //   switch (z.selection.content) {
  //   | [Projector(pr)] =>
  //     let pr = Id.Map.find_opt(pr.id, projectors);
  //     switch (pr) {
  //     | Some(pr) => Some(remove(pr.syntax, focus, z))
  //     | None => None
  //     };
  //   | _ => None
  //   };
  // };

  switch (a) {
  | SetIndicated(Specific(kind), projector_id) =>
    switch (set_indicated(z, projector_id, kind)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | SetIndicated(ChooseLivelit, projector_id) =>
    switch (
      List.filter_map(
        set_indicated(z, projector_id),
        ProjectorCore.Kind.livelit_projectors,
      )
    ) {
    | [hd, ..._] => Ok(hd)
    | [] => Error(Cant_project)
    }
  // | SetSyntax(_id, _seg) =>
  //   failwith("TODO: projectorperform update: wire or remove")

  //Ok(update(p => {...p, syntax: Segment.parenthesize(seg)}, id, z))
  // | SetModel(_id, _model) =>
  //   failwith("TODO: projectorperform setmodel: wire or remove")
  //Ok(update(pr => {...pr, model}, id, z))
  | Focus(id, d) =>
    // failwith("TODO: projectorperform focus: wire or remove")
    switch (d) {
    | None =>
      /* Focus by mouse click */
      /* Currently not calling focus method as projectors get focus here naturally */
      Ok(Option.value(~default=z, jump_to_id_indicated(z, id)))
    | Some(Right) =>
      /* Focus by arrow key hand-off */
      switch (Siblings.left_neighbor(z.relatives.siblings)) {
      | Some(Projector({id})) =>
        let pr = Id.Map.find_opt(id, projectors);
        switch (pr) {
        | Some(pr) =>
          let (module P) = ProjectorInit.to_module(pr.kind);
          P.focus((id, Some(Right)));
        | None => ()
        };
      | _ => ()
      };
      Ok(z);
    | Some(Left) =>
      /* Focus by arrow key hand-off */
      switch (Siblings.right_neighbor(z.relatives.siblings)) {
      | Some(Projector({id})) =>
        let pr = Id.Map.find_opt(id, projectors);
        switch (pr) {
        | Some(pr) =>
          let (module P) = ProjectorInit.to_module(pr.kind);
          P.focus((id, Some(Left)));
        | None => ()
        };
      | _ => ()
      };
      Ok(z);
    }
  | Escape(id, d) => Ok(jump_to_side_of_id(d, z, id))
  };
};

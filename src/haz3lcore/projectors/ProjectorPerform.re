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
    (term_data: TermData.t, a: Action.project, z: Zipper.t)
    : result(ZipperBase.t, Action.Failure.t) => {
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
    /* If not projected, project. If already same kind, remove. If other kind, change */
    let* (focus, z) = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] when pr.kind == kind =>
      Some(remove(pr.syntax, focus, z))
    | [Projector(pr)] =>
      let+ piece = init(kind, Piece.unparenthesize(pr.syntax));
      replace_selection_and_unselect(piece, focus, z);
    | seg =>
      let+ piece = init(kind, seg);
      replace_selection_and_unselect(piece, focus, z);
    };
  };

  let remove_indicated = (z: Zipper.t): option(Zipper.t) => {
    let* (focus, z) = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] => Some(remove(pr.syntax, focus, z))
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
  | SetSyntax(id, seg) =>
    Ok(
      update(
        p =>
          {
            ...p,
            syntax: Segment.parenthesize(seg),
          },
        id,
        z,
      ),
    )
  | SetModel(id, model) =>
    Ok(
      update(
        pr =>
          {
            ...pr,
            model,
          },
        id,
        z,
      ),
    )
  | Focus(id, kind, d) =>
    switch (d) {
    | None =>
      /* Focus by mouse click */
      let (module P) = ProjectorInit.to_module(kind);
      switch (P.focusable.pointer) {
      | Some(focus) => focus(id)
      | None => ()
      };
      //if in refractors, look up id using reverse_mapping
      /* TODO: perf */
      // let rmap =
      //   ZipperBase.Refractor.reverse_mapping(
      //     Id.Map.union(
      //       (_, _, b) => Some(b),
      //       z.refractors.map,
      //       z.refractors.ephemerals,
      //     ),
      //   );
      let id =
        switch (Id.recover_original(id)) {
        | pid =>
          // print_endline("found in rmap refractors. looking up id");
          pid
        // | None => id
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
    }
  | Escape(id, d) =>
    switch (Move.jump_to_side_of_id(d, z, id)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | DynCursor(a) => Ok(DynCursorPerform.perform(z, a))
  };
};

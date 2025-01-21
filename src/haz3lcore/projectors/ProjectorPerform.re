open ProjectorBase;

/* Updates the underlying piece of syntax for a projector */
module Update = {
  let update_piece =
      (f: Base.projector => Base.projector, id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when pr.id == id => Base.Projector(f(pr))
    | x => x
    };

  let init = (kind: ProjectorCore.kind, syntax: syntax): option(syntax) =>
    /* Note that we always unparenthesize the syntax before passing it to maketerm.
     * By convention, the stored syntax for a projector is always parenthesized
     * on projection (and de-parenthesized on unprojection), regardless of its
     * initial form. Thus this change makes the term passed to the projector's
     * can_project method more reflective of the initial syntax. However, the
     * segment passed to it will be the parenthesized one, so if a projector
     * bases its can_project logic on the segment, it must take this into account.
     * This distinction is mostly an artifact of current syntax implementation
     * decisions and will likely be eliminated in the future. */
    switch (syntax |> ProjectorInfo.unparenthesize |> MakeTerm.any) {
    | Nul () => None
    | any => ProjectorInit.init(kind, syntax, any)
    };

  let add_or_replace =
      (kind: ProjectorCore.kind, syntax: syntax): option(syntax) =>
    switch (syntax) {
    | Projector(pr) => init(kind, pr.syntax)
    | syntax => init(kind, syntax)
    };

  let add_or_remove =
      (kind: ProjectorCore.kind, syntax: syntax): option(syntax) =>
    switch (syntax) {
    | Projector(pr) => Some(pr.syntax)
    | syntax => init(kind, syntax)
    };

  let update =
      (f: Base.projector => Base.projector, id: Id.t, z: ZipperBase.t)
      : ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(update_piece(f, id), id, z);

  let remove_projector = (id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when pr.id == id => pr.syntax
    | x => x
    };

  let remove = (id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(remove_projector(id), id, z);
};

let go =
    (
      jump_to_id_indicated,
      jump_to_side_of_id,
      select_term: Zipper.t => option(Zipper.t),
      a: Action.project,
      z: Zipper.t,
    )
    : result(ZipperBase.t, Action.Failure.t) => {
  let get_direction = (z: Zipper.t): option(Util.Direction.t) =>
    Selection.is_empty(z.selection)
      ? Indicated.direction(z)
      : Some(Util.Direction.toggle(z.selection.focus));

  let setup_selection = (z: Zipper.t): option(Zipper.t) =>
    Selection.is_empty(z.selection) ? select_term(z) : Some(z);

  let replace_selection = (z, focus, segment): Zipper.t =>
    {...z, selection: Selection.mk(~focus, segment)}
    |> Zipper.unselect
    |> Zipper.remold_regrout(Util.Direction.Right)
    |> Zipper.remold_regrout(Util.Direction.Left);

  /* TODO: On undo project space-padded type anno from right:
     Skel.push_output: split_kids: index out of bounds */

  //TODO: maybe also reject secondary-padded stuff?

  //TODO: maybe if unprojecting non-operand, leave it selected? need to be careful with remolding..

  let do_indicated = (~remove: bool, kind, z): option(Zipper.t) => {
    open Util.OptUtil.Syntax;
    let* focus = get_direction(z);
    let* z = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] when remove =>
      let seg = ProjectorInfo.unparenthesize(pr.syntax);
      Some(replace_selection(z, focus, seg));
    | [Projector(pr)] when !remove =>
      let+ syntax = Update.init(kind, pr.syntax);
      replace_selection(z, focus, [syntax]);
    | seg =>
      switch (MakeTerm.any(seg)) {
      /* Incomplete or Invalid term */
      | Nul () => None
      | _ =>
        //TODO: specify override sort below in hole case
        let piece = Segment.parenthesize(seg);
        let+ syntax = Update.init(kind, piece);
        replace_selection(z, focus, [syntax]);
      }
    };
  };

  let remove_indicated = (z): option(Zipper.t) => {
    open Util.OptUtil.Syntax;
    let* focus = get_direction(z);
    let* z = setup_selection(z);
    switch (z.selection.content) {
    | [Projector(pr)] =>
      let seg = ProjectorInfo.unparenthesize(pr.syntax);
      Some(replace_selection(z, focus, seg));
    | _ => None
    };
  };

  switch (a) {
  /* Projection logic is based on selection and parenthezation.
   * If there is no current selection, we select the currently indicated
   * term. In this case, it is assured that the syntax to be projected
   * is both a term in isolation and a subterm of the containing term.
   * However, if there is already a selection, it could be that the
   * selection is a term in isolation, but NOT a subterm of the containing
   * term. An example of this is `1+2`, where the full synatx is `1+2*3`.
   * In these cases, we project anyway, under the logic that projection is
   * (a kind of) parenthesization. That is, it changes the semantics of the
   * program precisely when parenthesization would. Note that is some cases
   * this can result in not static but syntactic errors; for example, if we
   * project `x:Int` within `x:Int->Bool`. Again, this is done under the
   * logic of doing the same thing parenthesis-wrapping would do, for the
   * purposes of predictability. Similarly, when we unproject something, the
   * surrounding lexical context may have changed, so even if the projected
   * syntax was a subterm of the surrounding term when projected, it will
   * not necessarily be when it it unprojected; for example, if the projected
   * syntax is rooted at an infix expression, and after projection a
   * neighboring infix operation was added which binds tighter. Again,
   * this is the same as would happen if unparenthesizing a subterm. */
  | SetIndicated(kind) =>
    switch (do_indicated(~remove=false, kind, z)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | ToggleIndicated(Specific(kind)) =>
    switch (do_indicated(~remove=true, kind, z)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | ToggleIndicated(ChooseLivelit) =>
    let guys =
      List.filter_map(
        kind => do_indicated(~remove=true, kind, z),
        ProjectorCore.livelit_projectors,
      );
    switch (guys) {
    | [hd, ..._] => Ok(hd)
    | [] => Error(Cant_project)
    };
  | RemoveIndicated =>
    switch (remove_indicated(z)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | SetSyntax(id, syntax) =>
    /* Note we update piece id to keep in sync with projector id;
     * See intial id setting in Update.init */
    Ok(
      Update.update(
        p => {...p, syntax: Piece.replace_id(id, syntax)},
        id,
        z,
      ),
    )
  | SetModel(id, model) => Ok(Update.update(pr => {...pr, model}, id, z))
  | Focus(id, d) =>
    let z =
      switch (d) {
      | None =>
        /* d == None means focus by mouse click */
        jump_to_id_indicated(z, id) |> Option.value(~default=z)
      | Some(_) => z
      };
    switch (Indicated.projector(z)) {
    | Some((_, p)) =>
      let (module P) = ProjectorInit.to_module(p.kind);
      P.focus((id, d));
      Ok(z);
    | None => Error(Cant_project)
    };
  | Escape(id, d) => Ok(jump_to_side_of_id(d, z, id))
  };
};

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
    switch (MakeTerm.any([syntax])) {
    | Nul () => None
    | any => Some(ProjectorInit.init(kind, syntax, any))
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

/* If the caret is inside the indicated piece, move it out
 * NOTE: Might need to be updated to support pieces with more than 2 delims */
let move_out_of_piece =
    (d: Util.Direction.t, rel: Indicated.relation, z: Zipper.t): Zipper.t =>
  switch (rel) {
  | Sibling => {...z, caret: Outer}
  | Parent =>
    switch (Zipper.move(d, {...z, caret: Outer})) {
    | Some(z) => z
    | None => z
    }
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
    //TODO(andrew): remold/regrout? prob necessary for non-convex-mono case
    {...z, selection: Selection.mk(~focus, segment)} |> Zipper.unselect;

  let set_indicated = (z, p): option(Zipper.t) => {
    open Util.OptUtil.Syntax;
    let* focus = get_direction(z);
    let* z = setup_selection(z);
    switch (z.selection.content) {
    | [piece] =>
      let+ syntax = Update.add_or_replace(p, piece);
      replace_selection(z, focus, [syntax]);
    | _ => None
    };
  };

  let toggle_indicated = (z, p): option(Zipper.t) => {
    open Util.OptUtil.Syntax;
    let* focus = get_direction(z);
    let* z = setup_selection(z);
    switch (z.selection.content) {
    | [piece] =>
      let+ syntax = Update.add_or_remove(p, piece);
      replace_selection(z, focus, [syntax]);
    | _ => None
    };
  };

  switch (a) {
  | SetIndicated(p) =>
    switch (set_indicated(z, p)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | ToggleIndicated(p) =>
    switch (toggle_indicated(z, p)) {
    | Some(z) => Ok(z)
    | None => Error(Cant_project)
    }
  | Remove(id) => Ok(Update.remove(id, z))
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

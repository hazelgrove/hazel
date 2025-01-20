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

  let add_projector = (kind: ProjectorCore.kind, id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when Piece.id(syntax) == id =>
      switch (init(kind, pr.syntax)) {
      | Some(syntax) => syntax
      | None => syntax
      }
    | syntax when Piece.id(syntax) == id =>
      switch (init(kind, syntax)) {
      | Some(syntax) => syntax
      | None => syntax
      }
    | syntax => syntax
    };

  let remove_projector = (id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when pr.id == id => pr.syntax
    | x => x
    };

  let add_or_remove_projector =
      (kind: ProjectorCore.kind, id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when Piece.id(syntax) == id => pr.syntax
    | syntax when Piece.id(syntax) == id =>
      switch (init(kind, syntax)) {
      | Some(syntax) => syntax
      | None => syntax
      }
    | syntax => syntax
    };

  let update =
      (f: Base.projector => Base.projector, id: Id.t, z: ZipperBase.t)
      : ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(update_piece(f, id), id, z);

  let add = (k: ProjectorCore.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(add_projector(k, id), id, z);

  let add_or_remove =
      (k: ProjectorCore.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(add_or_remove_projector(k, id), id, z);

  let remove = (id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(remove_projector(id), id, z);

  let remove_all = (z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.remove_all_projectors(z);
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
    (jump_to_id_indicated, jump_to_side_of_id, a: Action.project, z: Zipper.t)
    : result(ZipperBase.t, Action.Failure.t) => {
  switch (a) {
  | SetIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, rel)) =>
      Ok(move_out_of_piece(d, rel, z) |> Update.add(p, Piece.id(piece)))
    }
  | ToggleIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, rel)) =>
      Ok(
        move_out_of_piece(d, rel, z)
        |> Update.add_or_remove(p, Piece.id(piece)),
      )
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

open Projector;
open ProjectorBase;

/* Updates the underlying piece of syntax for a projector */
module Update = {
  let update_piece =
      (
        f: Base.projector(Id.t) => Base.projector(Id.t),
        id: Id.t,
        syntax: syntax,
      ) =>
    switch (syntax) {
    | Projector(pr) when pr.extra == id => Base.Projector(f(pr))
    | x => x
    };

  let init = (kind: t, syntax: syntax): syntax => {
    /* We set the projector id equal to the Piece id for convienence
     * including cursor-info association. We maintain this invariant
     * when we update a projector's contained syntax */
    let (module P) = to_module(kind);
    switch (P.can_project(syntax) && minimum_projection_condition(syntax)) {
    | false => syntax
    | true =>
      Projector({extra: Piece.id(syntax), kind, model: P.init, syntax})
    };
  };

  let add_projector = (kind: Base.kind, id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when Piece.id(syntax) == id => init(kind, pr.syntax)
    | syntax when Piece.id(syntax) == id => init(kind, syntax)
    | x => x
    };

  let remove_projector = (id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when pr.extra == id => pr.syntax
    | x => x
    };

  let add_or_remove_projector = (kind: Base.kind, id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when Piece.id(syntax) == id => pr.syntax
    | syntax when Piece.id(syntax) == id => init(kind, syntax)
    | x => x
    };

  let remove_any_projector = (syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) => pr.syntax
    | x => x
    };

  let update =
      (
        f: Base.projector(Id.t) => Base.projector(Id.t),
        id: Id.t,
        z: ZipperBase.t,
      )
      : ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(update_piece(f, id), id, z);

  let add = (k: Base.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(add_projector(k, id), id, z);

  let add_or_remove = (k: Base.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(add_or_remove_projector(k, id), id, z);

  let remove = (id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(remove_projector(id), id, z);

  let remove_all = (z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.go(remove_any_projector, z);
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
    switch (Projector.indicated(z)) {
    | Some((_, p)) =>
      let (module P) = to_module(p.kind);
      P.focus((id, d));
      Ok(z);
    | None => Error(Cant_project)
    };
  | Escape(id, d) => Ok(jump_to_side_of_id(d, z, id))
  };
};

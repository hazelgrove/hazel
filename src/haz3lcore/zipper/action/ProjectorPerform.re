open Projector;

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
      Ok(
        move_out_of_piece(d, rel, z)
        |> Projector.Update.add(p, Piece.id(piece)),
      )
    }
  | ToggleIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, rel)) =>
      Ok(
        move_out_of_piece(d, rel, z)
        |> Projector.Update.add_or_remove(p, Piece.id(piece)),
      )
    }
  | Remove(id) => Ok(Projector.Update.remove(id, z))
  | SetSyntax(id, syntax) =>
    /* Note we update piece id to keep in sync with projector id */
    Ok(
      Projector.Update.update(
        p => {...p, syntax: Piece.replace_id(id, syntax)},
        id,
        z,
      ),
    )
  | SetModel(id, model) =>
    Ok(Projector.Update.update(pr => {...pr, model}, id, z))
  | Focus(id, d) =>
    let z =
      switch (d) {
      | None =>
        /* d==None means a mouse click */
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

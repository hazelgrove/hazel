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
    (
      jump_to_id,
      primary:
        (Zipper.chunkiness, Util.Direction.t, Zipper.t) => option(Zipper.t),
      a: Action.project,
      z: Zipper.t,
    )
    : result(ZipperBase.t, Action.Failure.t) => {
  let switch_side = z =>
    switch (primary(ByToken, Right, z)) {
    | Some(z) => z
    | None => z
    };
  let jump = (z, id) =>
    switch (jump_to_id(z, id)) {
    /* Moves to right side, as right side always implies it's indicated.
     * For example,"(|x)" or "!|x" wouldn't have "x" indicated */
    //TODO(andrew): just making this change breaks escape
    | Some(z) => z //switch_side(z)
    | None => z
    };
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
    Ok(Projector.Update.update(p => {...p, syntax}, id, z))
  | SetModel(id, model) =>
    Ok(Projector.Update.update(pr => {...pr, model}, id, z))
  | Focus(id, d) =>
    //TODO(andrew): this fails if moving to e.g. "![checkbox]"
    let z = jump(z, id);
    switch (Projector.indicated(z)) {
    | Some((_, p)) =>
      let (module P) = to_module(p.kind);
      P.focus((id, d));
      Ok(z);
    | None => Error(Cant_project)
    };
  | Escape(id, d) =>
    let z = jump(z, id);
    switch (d) {
    | Left => Ok(z)
    | Right => Ok(switch_side(z))
    };
  };
};

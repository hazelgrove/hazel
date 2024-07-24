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

// let set = (id: Id.t, p: option(Map.entry), z: Zipper.t) => {
//   ...z,
//   projectors: Map.update(id, _ => p, z.projectors),
// };

let add = (id: Id.t, z: Zipper.t, p, _piece, d, rel) =>
  Ok(move_out_of_piece(d, rel, z) |> ProjMeta.Update.add(p, id));
// switch (Projector.create(p, piece)) {
// | None => Error(Action.Failure.Cant_project)
// | opt_p => Ok(set(id, opt_p, z) |> move_out_of_piece(d, rel))
// };

let add_or_remove = (id: Id.t, z: Zipper.t, p, _piece, d, rel) =>
  Ok(ProjMeta.Update.add_or_remove(p, id, z) |> move_out_of_piece(d, rel));
// switch (Map.mem(id, z.projectors)) {
// | false => add(id, z, p, piece, d, rel)
// | true => Ok(set(id, None, z))
// };

let go =
    (
      jump_to_id,
      primary:
        (Zipper.chunkiness, Util.Direction.t, Zipper.t) => option(Zipper.t),
      a: Action.project,
      z: Zipper.t,
    )
    : result(ZipperBase.t, Action.Failure.t) => {
  let jump = (z, id) =>
    switch (jump_to_id(z, id)) {
    | Some(z) => z
    | None => z
    };
  let switch_side = z =>
    switch (primary(ByToken, Right, z)) {
    | Some(z) => z
    | None => z
    };
  switch (a) {
  | SetIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, rel)) => add(Piece.id(piece), z, p, piece, d, rel)
    }
  | ToggleIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Cant_project)
    | Some((piece, d, rel)) =>
      add_or_remove(Piece.id(piece), z, p, piece, d, rel)
    }
  | Remove(id) => Ok(ProjMeta.Update.remove(id, z))
  | SetSyntax(id, syntax) =>
    Ok(ProjMeta.Update.update(p => {...p, syntax}, id, z))
  | SetModel(id, model) =>
    Ok(ProjMeta.Update.update(pr => {...pr, model}, id, z))
  | Focus(id, d) =>
    let z = jump(z, id);
    switch (ProjMeta.indicated(z)) {
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

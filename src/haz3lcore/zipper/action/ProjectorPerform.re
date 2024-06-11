open Projector;

let move_out_of_piece =
    (d: Util.Direction.t, rel: Indicated.relation, z: Zipper.t): Zipper.t =>
  /* Might not work for pieces with more than 2 delims */
  switch (rel) {
  | Sibling => {...z, caret: Outer}
  | Parent =>
    switch (Zipper.move(d, {...z, caret: Outer})) {
    | Some(z) => z
    | None => z
    }
  };

let set = (id: Id.t, p: option(t), z: ZipperBase.t) => {
  ...z,
  projectors: Map.update(id, _ => p, z.projectors),
};

let add_or_remove = (id: Id.t, z: Zipper.t, info_map, p, piece, d, rel) =>
  switch (Map.mem(id, z.projectors)) {
  | false =>
    switch (Projector.create(p, piece, id, info_map)) {
    | None => Error(Action.Failure.Cant_project)
    | opt_p => Ok(set(id, opt_p, z) |> move_out_of_piece(d, rel))
    }
  | true => Ok(set(id, None, z))
  };

let go = (a: Action.project, info_map: Statics.Map.t, z: ZipperBase.t) =>
  //TODO(andrew): avoid bringing statics in here?
  //TODO(andrew): method to remotely get projected piece
  switch (a) {
  | ToggleIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Action.Failure.Cant_project)
    | Some((piece, d, rel)) =>
      add_or_remove(Piece.id(piece), z, info_map, p, piece, d, rel)
    }
  | Remove(id) =>
    switch (Map.mem(id, z.projectors)) {
    | false => Error(Action.Failure.Cant_project)
    | true => Ok(set(id, None, z))
    }
  | UpdateSyntax(id, f) => Ok(Projector.UpdateSyntax.go(f, id, z))
  };

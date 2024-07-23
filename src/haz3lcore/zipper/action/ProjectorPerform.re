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

let set = (id: Id.t, p: option(Map.entry), z: Zipper.t) => {
  ...z,
  projectors: Map.update(id, _ => p, z.projectors),
};

let add = (id: Id.t, z: Zipper.t, p, piece, d, rel) =>
  switch (Projector.create(p, piece)) {
  | None => Error(Action.Failure.Cant_project)
  | opt_p => Ok(set(id, opt_p, z) |> move_out_of_piece(d, rel))
  };

let add_or_remove = (id: Id.t, z: Zipper.t, p, piece, d, rel) =>
  switch (Map.mem(id, z.projectors)) {
  | false => add(id, z, p, piece, d, rel)
  | true => Ok(set(id, None, z))
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
  | Focus(id, d) =>
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
  | Remove(id) =>
    switch (Map.mem(id, z.projectors)) {
    | false => Error(Cant_project)
    | true => Ok(set(id, None, z))
    }
  | SetSyntax(id, syntax) => Ok(Projector.Syntax.update(_ => syntax, id, z))
  | SetModel(id, model) =>
    let update = entry => Option.map(e => Map.{model, kind: e.kind}, entry);
    Ok({...z, projectors: Map.update(id, update, z.projectors)});
  };
};

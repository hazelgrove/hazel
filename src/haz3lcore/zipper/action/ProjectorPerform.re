open Projector;

let move_out_of_piece =
    (d: Util.Direction.t, rel: Indicated.relation, z: Zipper.t): Zipper.t =>
  /* NOTE: Might not work for pieces with more than 2 delims */
  switch (rel) {
  | Sibling => {...z, caret: Outer}
  | Parent =>
    switch (Zipper.move(d, {...z, caret: Outer})) {
    | Some(z) => z
    | None => z
    }
  };

let set = (id: Id.t, p: option(entry), z: Zipper.t) => {
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

//TODO(andrew): dupe, rm
let indicated_proj_z = (z: Zipper.t) => {
  open Util.OptUtil.Syntax;
  let* id = Indicated.index(z);
  let+ projector = Projector.Map.find(id, z.projectors);
  (id, projector);
};

let go =
    (
      jump_to_id,
      primary:
        (Zipper.chunkiness, Util.Direction.t, Zipper.t) => option(Zipper.t),
      a: Action.project,
      z: Zipper.t,
    ) => {
  switch (a) {
  | Focus(id, d) =>
    let z =
      switch (jump_to_id(z, id)) {
      | Some(z) => z
      | None => z
      };
    switch (d) {
    | None =>
      let p = indicated_proj_z(z) |> Option.map(snd);
      switch (p) {
      | Some(p) =>
        let (module P) = to_module(p.kind);
        P.focus((id, Left));
        Ok(z);
      | None => Error(Action.Failure.Cant_project)
      };
    | Some(Left) => Ok(z)
    | Some(Right) =>
      let z =
        switch (primary(ByToken, Right, z)) {
        | Some(z) => z
        | None => z
        };
      Ok(z);
    };

  | Escape(id, Left) =>
    let z =
      switch (jump_to_id(z, id)) {
      | Some(z) => z
      | None => z
      };
    Ok(z);
  | Escape(id, Right) =>
    let z =
      switch (jump_to_id(z, id)) {
      | Some(z) => z
      | None => z
      };
    let z =
      switch (primary(ByToken, Right, z)) {
      | Some(z) => z
      | None => z
      };
    Ok(z);
  | SetIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Action.Failure.Cant_project)
    | Some((piece, d, rel)) => add(Piece.id(piece), z, p, piece, d, rel)
    }
  | ToggleIndicated(p) =>
    switch (Indicated.for_index(z)) {
    | None => Error(Action.Failure.Cant_project)
    | Some((piece, d, rel)) =>
      add_or_remove(Piece.id(piece), z, p, piece, d, rel)
    }
  | Remove(id) =>
    switch (Map.mem(id, z.projectors)) {
    | false => Error(Action.Failure.Cant_project)
    | true => Ok(set(id, None, z))
    }
  | SetSyntax(id, p) => Ok(Projector.Syntax.update(_ => p, id, z))
  | SetModel(id, model) =>
    Ok({
      ...z,
      projectors:
        Map.update(
          id,
          entry => Option.map(e => {model, kind: e.kind}, entry),
          z.projectors,
        ),
    })
  };
};

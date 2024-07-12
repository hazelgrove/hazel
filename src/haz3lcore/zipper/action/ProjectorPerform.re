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

let set = (id: Id.t, p: option(t), z: Zipper.t) => {
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
      syntax_map,
      z: Zipper.t,
    ) => {
  let crime = (_syntax, b, p) => {
    //TODO(andrew): remove this crime
    let (module P) = Projector.to_module(p);
    P.update(SetInside(b));
    //p;
  };
  let set_dispatch = (z: Zipper.t, id, b) =>
    switch (Id.Map.find_opt(id, syntax_map)) {
    | Some(syntax) =>
      Ok({
        ...z,
        projectors:
          Map.update(id, Option.map(crime(syntax, b)), z.projectors),
      })
    | None => Error(Action.Failure.Cant_project)
    };
  switch (a) {
  | FocusInternal(id) =>
    /* Note: jumping her normalizes position, so when exiting
     * we know we're intially to the left and can move or not accordingly */
    let z =
      switch (jump_to_id(z, id)) {
      | Some(z) => z
      | None => z
      };
    set_dispatch(z, id, true);
  // JsUtil.get_elem_by_selector(selector)##focus;
  | Escape(id, Left) =>
    // JsUtil.get_elem_by_selector(selector)##blur;
    let z =
      switch (primary(ByToken, Right, z)) {
      | Some(z) => z
      | None => z
      };
    set_dispatch(z, id, false);
  | Escape(id, Right) =>
    // JsUtil.get_elem_by_selector(selector)##blur;
    set_dispatch(z, id, false)
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
  | UpdateModel(id, f) =>
    Ok({...z, projectors: Map.update(id, Option.map(f), z.projectors)})
  };
};

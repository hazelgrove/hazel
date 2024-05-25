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

let set = (p: option(t), id: Id.t, ps: Map.t) => Map.update(id, _ => p, ps);

let set = (p: option(t), id: Id.t, z: ZipperBase.t) => {
  ...z,
  projectors: set(p, id, z.projectors),
};

let toggle_click = (id: Id.t, info: Projector.info, ps: Map.t): option(t) => {
  switch (Map.find(id, ps)) {
  | Some(Infer(_)) => Some(Fold())
  | Some(Fold ()) =>
    let (module I) = InferProjectorCore.mk({expected_ty: None});
    //TODO(andrew): get piece of target for I.can_project(piece)
    Some(I.update(info));
  | None => Some(Fold())
  };
};

let toggle_local =
    (id: Id.t, projectors: Map.t, piece: Piece.t): (option(t), option(t)) => {
  /* returns prev & new projector model */
  switch (Map.find(id, projectors)) {
  | Some(p) => (Some(p), None)
  | None =>
    let (module P) = FoldProjectorCore.mk();
    if (P.can_project(piece)) {
      (None, Some(Fold()));
    } else {
      (None, None);
    };
  };
};

let go = (a: Action.project, statics: CachedStatics.statics, z: ZipperBase.t) =>
  //TODO(andrew): avoid bringing statics in here?
  switch (Indicated.for_index(z)) {
  | None => None
  | Some((p, d, rel)) =>
    switch (a) {
    | ToggleIndicated =>
      let id = Piece.id(p);
      switch (toggle_local(id, z.projectors, p)) {
      | (None, None) => None
      | (None, opt_p) =>
        Some(set(opt_p, id, z) |> move_out_of_piece(d, rel))
      | _ => Some(set(None, id, z))
      };
    | Toggle(id) =>
      let info = Id.Map.find_opt(id, statics.info_map);
      Some(set(toggle_click(id, {info: info}, z.projectors), id, z));
    }
  };

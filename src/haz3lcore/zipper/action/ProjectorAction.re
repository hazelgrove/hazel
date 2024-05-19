open Zipper;

let of_siblings =
    (projectors: Projector.Map.t, siblings: Siblings.t): Siblings.t => {
  let l_sibs = Projector.of_segment(projectors, fst(siblings));
  let r_sibs = Projector.of_segment(projectors, snd(siblings));
  (l_sibs, r_sibs);
};

let of_ancestor =
    (projectors: Projector.Map.t, ancestor: Ancestor.t): Ancestor.t => {
  {
    ...ancestor,
    children: (
      List.map(Projector.of_segment(projectors), fst(ancestor.children)),
      List.map(Projector.of_segment(projectors), snd(ancestor.children)),
    ),
  };
};

let of_generation =
    (projectors: Projector.Map.t, generation: Ancestors.generation)
    : Ancestors.generation => (
  of_ancestor(projectors, fst(generation)),
  of_siblings(projectors, snd(generation)),
);

let of_ancestors =
    (projectors: Projector.Map.t, ancestors: Ancestors.t): Ancestors.t =>
  List.map(of_generation(projectors), ancestors);

let of_selection =
    (projectors: Projector.Map.t, selection: Selection.t): Selection.t => {
  {
    ...selection,
    content: Projector.of_segment(projectors, selection.content),
  };
};

let of_zipper = (z: Zipper.t): Zipper.t =>
  if (Id.Map.is_empty(z.projectors)) {
    z;
  } else {
    {
      ...z,
      selection: of_selection(z.projectors, z.selection),
      relatives: {
        ancestors: of_ancestors(z.projectors, z.relatives.ancestors),
        siblings: of_siblings(z.projectors, z.relatives.siblings),
      },
    };
  };

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

let update = (f, id, z) => {
  ...z,
  projectors: Projector.Map.update(id, f, z.projectors),
};

let set = (prj, id, z) => update(_ => prj, id, z);

let can_project = (prj: Projector.t, p: Piece.t): bool =>
  switch (prj) {
  | Infer(_) =>
    Piece.is_convex(p)
    && (
      switch (p) {
      | Tile(t) => t.mold.out == Exp || t.mold.out == Pat
      | _ => false
      }
    )
  | Fold => Piece.is_convex(p)
  };

let project = (prj, id, d, rel, z) =>
  z |> set(Some(prj), id) |> move_out_of_piece(d, rel) |> Option.some;

let toggle = (id: Id.t, z: Zipper.t, piece, d, rel) =>
  switch (Projector.Map.find(id, z.projectors)) {
  | Some(Fold) =>
    let default_infer = Projector.Infer({expected_ty: None});
    if (can_project(default_infer, piece)) {
      project(default_infer, id, d, rel, z);
    } else {
      Some(set(None, id, z));
    };
  | Some(Infer(_)) => Some(set(None, id, z))
  | None when Piece.is_convex(piece) =>
    if (can_project(Fold, piece)) {
      project(Fold, id, d, rel, z);
    } else {
      None;
    }
  | None => None
  };

let go = (a: Action.project, z: Zipper.t) =>
  switch (Indicated.for_index(z)) {
  | None => None
  | Some((p, d, rel)) =>
    switch (a) {
    | ToggleIndicated => toggle(Piece.id(p), z, p, d, rel)
    | Toggle(id) => toggle(id, z, p, d, rel)
    }
  };

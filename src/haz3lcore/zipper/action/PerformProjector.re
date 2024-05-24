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

let project = (z: Zipper.t): Zipper.t =>
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

let set = (p: option(Projector.t), id: Id.t, ps: Projector.Map.t) =>
  Projector.Map.update(id, _ => p, ps);

let set = (p: option(Projector.t), id: Id.t, z: Zipper.t) => {
  ...z,
  projectors: set(p, id, z.projectors),
};

let go = (a: Action.project, statics: CachedStatics.statics, z: Zipper.t) =>
  //TODO(andrew): avoid bringing statics in here?
  switch (Indicated.for_index(z)) {
  | None => None
  | Some((p, d, rel)) =>
    switch (a) {
    | ToggleIndicated =>
      let id = Piece.id(p);
      switch (Projector.toggle_local(id, z.projectors, p)) {
      | (None, None) => None
      | (None, opt_p) =>
        Some(set(opt_p, id, z) |> move_out_of_piece(d, rel))
      | _ => Some(set(None, id, z))
      };
    | Toggle(id) =>
      let info = Id.Map.find_opt(id, statics.info_map);
      let (opt_p, id) = Projector.toggle_click(id, info, z.projectors);
      Some(set(opt_p, id, z));
    }
  };

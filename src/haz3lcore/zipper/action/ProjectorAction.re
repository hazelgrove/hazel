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

let set_project =
    (
      prj: Projector.t,
      id: Id.t,
      d: Util.Direction.t,
      rel: Indicated.relation,
      z: Zipper.t,
    ) =>
  z |> set(Some(prj), id) |> move_out_of_piece(d, rel) |> Option.some;

let toggle_fold = (id, _info, z: Zipper.t, piece, d, rel) => {
  switch (Projector.Map.find(id, z.projectors)) {
  | Some(p) =>
    let (module P) = p;
    switch (P.proj_type) {
    | Fold(_) => Some(set(None, id, z))
    | Infer(_) => None
    };
  | _ =>
    let p: module Projector.P = Projector.mkFold();
    let (module P) = p;
    if (P.can_project(piece)) {
      set_project(p, id, d, rel, z);
    } else {
      None;
    };
  };
};

let toggle_infer = (id, info, z: Zipper.t) => {
  //TODO: get piece of target for predicate
  switch (Projector.Map.find(id, z.projectors)) {
  | Some(p) =>
    let (module P) = p;
    let infer = Projector.mkFInfer({expected_ty: None});
    let (module I) = infer;
    I.update(info);
    //TODO(andrew): does this nonsense make sense?
    Some(set(Some(infer), id, z));
  | _ =>
    let p: module Projector.P = Projector.mkFold();
    Some(set(Some(p), id, z));
  };
};

let go = (a: Action.project, statics: CachedStatics.statics, z: Zipper.t) =>
  //TODO(andrew): avoid bringing statics in here?
  switch (Indicated.for_index(z)) {
  | None => None
  | Some((p, d, rel)) =>
    switch (a) {
    | ToggleIndicated =>
      let id = Piece.id(p);
      let info = Id.Map.find_opt(id, statics.info_map);
      toggle_fold(id, info, z, p, d, rel);
    | Toggle(id) =>
      let info = Id.Map.find_opt(id, statics.info_map);
      toggle_infer(id, info, z);
    }
  };

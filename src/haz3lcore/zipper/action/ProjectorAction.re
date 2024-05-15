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

let of_zipper = (z: Zipper.t): Zipper.t => {
  {
    ...z,
    selection: of_selection(z.projectors, z.selection),
    relatives: {
      ancestors: of_ancestors(z.projectors, z.relatives.ancestors),
      siblings: of_siblings(z.projectors, z.relatives.siblings),
    },
  };
};

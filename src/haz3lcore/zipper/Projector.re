open Util;
open ZipperBase;

[@deriving (show({with_path: false}), sexp, yojson)]
module Map = ProjectorMap;

[@deriving (show({with_path: false}), sexp, yojson)]
type info = ZipperBase.projector_info;

[@deriving (show({with_path: false}), sexp, yojson)]
type fold = ZipperBase.fold;

[@deriving (show({with_path: false}), sexp, yojson)]
type infer = ZipperBase.infer;

let to_module = (p: projector): projector_core =>
  switch ((p: projector)) {
  | Fold(data) => FoldProjectorCore.mk(data)
  | Infer(data) => InferProjectorCore.mk(data)
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type t = projector;

let placeholder = (p: t, id: Id.t): Piece.t => {
  let (module P) = to_module(p);
  Piece.Tile({
    id,
    label: [String.make(P.placeholder_length(), ' ')],
    mold: Mold.mk_op(Any, []),
    shards: [0],
    children: [],
  });
};

let create =
    (p: t, piece: Piece.t, id: Id.t, info_map: Statics.Map.t): option(t) => {
  let (module P) = to_module(p);
  P.can_project(piece)
    ? Some(P.update({info: Id.Map.find_opt(id, info_map)})) : None;
};

let piece_is = (ps: Map.t, piece: option(Piece.t)): option(Id.t) =>
  switch (piece) {
  | Some(p) when Map.mem(Piece.id(p), ps) =>
    Map.mem(Piece.id(p), ps) ? Some(Piece.id(p)) : None
  | _ => None
  };

let neighbor_is = (ps, s: Siblings.t): (option(Id.t), option(Id.t)) => (
  piece_is(ps, Siblings.left_neighbor(s)),
  piece_is(ps, Siblings.right_neighbor(s)),
);

module Select = {
  let skip_grow_left =
      ({relatives: {siblings: (ls, rs), _}, _} as z: ZipperBase.t) => {
    let (ls, content) = Segment.push_right((ls, z.selection.content));
    z
    |> put_selection_content(content)
    |> put_siblings((ls, rs))
    |> Option.some;
  };

  let skip_grow_right =
      ({relatives: {siblings: (ls, rs), _}, _} as z: ZipperBase.t) => {
    let (content, rs) = Segment.push_left((z.selection.content, rs));
    z
    |> put_selection_content(content)
    |> put_siblings((ls, rs))
    |> Option.some;
  };

  let grow =
      ({relatives: {siblings, _}, projectors, _} as z: ZipperBase.t)
      : option(ZipperBase.t) =>
    switch (z.selection.focus, neighbor_is(projectors, siblings)) {
    | (Left, (Some(_), _)) => skip_grow_left(z)
    | (Right, (_, Some(_))) => skip_grow_right(z)
    | _ => None
    };

  let skip_shrink_left =
      ({relatives: {siblings: (ls, rs), _}, _} as z: ZipperBase.t) => {
    let (ls, content) = Segment.push_left((ls, z.selection.content));
    z
    |> put_selection_content(content)
    |> put_siblings((ls, rs))
    |> Option.some;
  };

  let skip_shrink_right =
      ({relatives: {siblings: (ls, rs), _}, _} as z: ZipperBase.t) => {
    let (content, rs) = Segment.push_right((z.selection.content, rs));
    z
    |> put_selection_content(content)
    |> put_siblings((ls, rs))
    |> Option.some;
  };

  let selection_sides_is =
      (projectors, s: Selection.t): (option(Id.t), option(Id.t)) => (
    piece_is(projectors, ListUtil.hd_opt(s.content)),
    piece_is(projectors, ListUtil.last_opt(s.content)),
  );

  let shrink =
      ({selection, projectors, _} as z: ZipperBase.t): option(ZipperBase.t) =>
    switch (selection.focus, selection_sides_is(projectors, selection)) {
    | (Left, (Some(_), _)) => skip_shrink_left(z)
    | (Right, (_, Some(_))) => skip_shrink_right(z)
    | _ => None
    };
};

module Move = {
  let go = (d: Direction.t, z: ZipperBase.t): option(ZipperBase.t) =>
    switch (d, neighbor_is(z.projectors, z.relatives.siblings)) {
    | (Left, (Some(_), _)) =>
      Some(put_siblings(Segment.push_right(z.relatives.siblings), z))
    | (Right, (_, Some(_))) =>
      Some(put_siblings(Segment.push_left(z.relatives.siblings), z))
    | _ => None
    };
};

module Project = {
  let placehold = (ps: Map.t, p: Piece.t) =>
    switch (Map.find(Piece.id(p), ps)) {
    | None => p
    | Some(pr) => placeholder(pr, Piece.id(p))
    };

  let rec of_segment = (projectors, seg: Segment.t): Segment.t => {
    seg
    |> List.map(placehold(projectors))
    |> List.map(of_piece(projectors));
  }
  and of_piece = (projectors, p: Piece.t): Piece.t => {
    switch (p) {
    | Tile(t) => Tile(of_tile(projectors, t))
    | Grout(_) => p
    | Secondary(_) => p
    };
  }
  and of_tile = (projectors, t: Tile.t): Tile.t => {
    {...t, children: List.map(of_segment(projectors), t.children)};
  };

  let of_siblings = (projectors: Map.t, siblings: Siblings.t): Siblings.t => {
    let l_sibs = of_segment(projectors, fst(siblings));
    let r_sibs = of_segment(projectors, snd(siblings));
    (l_sibs, r_sibs);
  };

  let of_ancestor = (projectors: Map.t, ancestor: Ancestor.t): Ancestor.t => {
    {
      ...ancestor,
      children: (
        List.map(of_segment(projectors), fst(ancestor.children)),
        List.map(of_segment(projectors), snd(ancestor.children)),
      ),
    };
  };

  let of_generation =
      (projectors: Map.t, generation: Ancestors.generation)
      : Ancestors.generation => (
    of_ancestor(projectors, fst(generation)),
    of_siblings(projectors, snd(generation)),
  );

  let of_ancestors = (projectors: Map.t, ancestors: Ancestors.t): Ancestors.t =>
    List.map(of_generation(projectors), ancestors);

  let of_selection = (projectors: Map.t, selection: Selection.t): Selection.t => {
    {...selection, content: of_segment(projectors, selection.content)};
  };

  let go = (z: ZipperBase.t): ZipperBase.t =>
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
};

open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Secondary(Secondary.t)
  | Projector(projector)
and tile = {
  // invariants:
  // - length(mold.in_) + 1 == length(label)
  // - length(shards) <= length(label)
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  [@equal (_, _) => true]
  id: Id.t,
  label: Label.t,
  mold: Mold.t,
  shards: list(int),
  children: list(segment),
}
and projector = ProjectorCore.t(piece);

let rec map_piece = (~f_piece, x: piece) => {
  let rec_call = (piece: piece) => {
    switch (piece) {
    | Tile(t) =>
      Tile({
        ...t,
        children: t.children |> List.map(List.map(map_piece(~f_piece))),
      })
    | Secondary(_)
    | Projector(_) => piece
    };
  };
  x |> f_piece(rec_call);
};
/* If the piece is parentheses, return the child. Otherwise,
 * return a singleton segment consisting of the piece */
let unparenthesize = (piece: piece): segment =>
  switch (piece) {
  | Tile({
      label: ["(", ")"],
      mold: {nibs: ({shape: Convex, _}, {shape: Convex, _}), _},
      children: [seg],
      _,
    }) => seg
  | _ => [piece]
  };

/* Compute the outermost nib shapes of a tile (inlined to avoid
   Base -> Tile -> Base dependency cycle) */
let tile_shapes = (t: tile): (Nib.Shape.t, Nib.Shape.t) => {
  let l_shard = List.hd(t.shards);
  let r_shard = ListUtil.last(t.shards);
  let (l, _) = Mold.nibs(~index=l_shard, t.mold);
  let (_, r) = Mold.nibs(~index=r_shard, t.mold);
  (l.shape, r.shape);
};

/* Emit hole string at a shape conflict boundary */
let hole_string =
    (~holes: string, ~concave_holes: string, prev_r: Nib.Shape.t): string =>
  switch (Nib.Shape.flip(prev_r)) {
  | Convex => holes
  | Concave(_) => concave_holes
  };

let rec segment_to_string =
        (
          ~holes=" ",
          ~concave_holes=" ",
          ~refractors: list((Id.t, _))=[],
          ~refractor_seg_to_seg:
             (list((Id.t, _)), segment) => (list((Id.t, _)), segment),
          ~projector_to_segment,
          seg: segment,
        )
        : string => {
  let (refractors, seg) = refractor_seg_to_seg(refractors, seg);
  let boundary = Nib.Shape.concave();
  let (strs_rev, last_r) =
    List.fold_left(
      ((strs, prev_r), p: piece) =>
        switch (p) {
        | Secondary(w) => (
            [Secondary.get_string(w.content), ...strs],
            prev_r,
          )
        | Tile(t) =>
          let (l_shape, r_shape) = tile_shapes(t);
          let conflict =
            Nib.Shape.fits(prev_r, l_shape)
              ? "" : hole_string(~holes, ~concave_holes, prev_r);
          let s =
            tile_to_string(
              ~holes,
              ~concave_holes,
              ~refractors,
              ~refractor_seg_to_seg,
              ~projector_to_segment,
              t,
            );
          ([s, conflict, ...strs], r_shape);
        | Projector(pr) =>
          let (l_shape, r_shape) = ProjectorCore.shapes(pr);
          let conflict =
            Nib.Shape.fits(prev_r, l_shape)
              ? "" : hole_string(~holes, ~concave_holes, prev_r);
          let s =
            segment_to_string(
              ~holes,
              ~concave_holes,
              ~refractors,
              ~refractor_seg_to_seg,
              ~projector_to_segment,
              projector_to_segment(pr),
            );
          ([s, conflict, ...strs], r_shape);
        },
      ([], boundary),
      seg,
    );
  /* Check trailing boundary */
  let trailing =
    Nib.Shape.fits(last_r, boundary)
      ? "" : hole_string(~holes, ~concave_holes, last_r);
  [trailing, ...strs_rev] |> List.rev |> String.concat("");
}
and tile_to_string =
    (
      ~holes: string,
      ~concave_holes: string,
      ~refractors: list((Id.t, _)),
      ~refractor_seg_to_seg,
      ~projector_to_segment,
      t: tile,
    )
    : string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(
       List.nth(t.label),
       segment_to_string(
         ~holes,
         ~concave_holes,
         ~refractors,
         ~refractor_seg_to_seg,
         ~projector_to_segment,
       ),
     )
  |> String.concat("");

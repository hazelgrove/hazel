open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
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

let rec segment_to_string =
        (
          ~holes=" ",
          ~concave_holes=" ",
          ~refractors: Id.Map.t(_)=Id.Map.empty,
          ~refractor_seg_to_seg:
             (Id.Map.t(_), segment) => (Id.Map.t(_), segment),
          ~projector_to_segment,
          seg: segment,
        )
        : string => {
  //TODO(andrew): reinstate when whitespace issue is resolved
  //let (refractors, seg) = refractor_seg_to_seg(refractors, seg);
  seg
  |> List.map(
       piece_to_string(
         ~holes,
         ~concave_holes,
         ~refractors,
         ~refractor_seg_to_seg,
         ~projector_to_segment,
       ),
     )
  |> String.concat("");
}
and piece_to_string =
    (
      ~holes: string,
      ~concave_holes: string,
      ~refractors: Id.Map.t(_),
      ~refractor_seg_to_seg,
      ~projector_to_segment,
      p: piece,
    )
    : string =>
  switch (p) {
  | Tile(t) =>
    tile_to_string(
      ~holes,
      ~concave_holes,
      ~refractors,
      ~refractor_seg_to_seg,
      ~projector_to_segment,
      t,
    )
  | Grout({shape: Concave, _}) => concave_holes
  | Grout({shape: Convex, _}) => holes
  | Secondary(w) => Secondary.get_string(w.content)
  | Projector(p) =>
    segment_to_string(
      ~holes,
      ~concave_holes,
      ~refractors,
      ~refractor_seg_to_seg,
      ~projector_to_segment,
      projector_to_segment(p),
    )
  }
and tile_to_string =
    (
      ~holes: string,
      ~concave_holes: string,
      ~refractors: Id.Map.t(_),
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

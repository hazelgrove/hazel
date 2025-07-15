open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Secondary([@show.opaque] Secondary.t)
  | Projector(projector)
and tile = {
  // invariants:
  // - length(mold.in_) + 1 == length(label)
  // - length(shards) <= length(label)
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  [@equal (_, _) => true] [@show.opaque]
  id: Id.t,
  label: Label.t,
  [@show.opaque]
  mold: Mold.t,
  shards: list(int),
  children: list(segment),
}
and projector = ProjectorCore.t(piece);

// This is for comment insertion
let mk_secondary = (id, content) => [
  Secondary({
    id,
    content,
  }),
];

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
        (~holes=" ", ~concave_holes=" ", seg: segment): string =>
  seg
  |> List.map(piece_to_string(~holes, ~concave_holes))
  |> String.concat("")
and piece_to_string =
    (~holes: string, ~concave_holes: string, p: piece): string =>
  switch (p) {
  | Tile(t) => tile_to_string(~holes, ~concave_holes, t)
  | Grout({shape: Concave, _}) => concave_holes
  | Grout({shape: Convex, _}) => holes
  | Secondary(w) => Secondary.get_string(w.content)
  | Projector(p) =>
    segment_to_string(~holes, ~concave_holes, unparenthesize(p.syntax))
  }
and tile_to_string = (~holes: string, ~concave_holes: string, t: tile): string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(List.nth(t.label), segment_to_string(~holes, ~concave_holes))
  |> String.concat("");

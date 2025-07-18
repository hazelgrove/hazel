open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type segment('p) = list(piece('p))
and piece('p) =
  | Tile(tile('p))
  | Grout(Grout.t)
  | Secondary(Secondary.t)
  | Projector(projector('p))
and tile('p) = {
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
  children: list(segment('p)),
}
and projector('p) = {
  id: Id.t,
  mold: Mold.t,
  model: 'p,
};

// This is for comment insertion
let mk_secondary = (id, content) => [
  Secondary({
    id,
    content,
  }),
];

let mk_projector = (~sort: Sort.t, ~model) => {
  id: Id.mk(),
  mold: Mold.mk_op(sort, []), /* Projectors currently are all convex */
  model,
};

/* If the piece is parentheses, return the child. Otherwise,
 * return a singleton segment consisting of the piece */
let unparenthesize = (piece: piece('p)): segment('p) =>
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
        (~holes=" ", ~concave_holes=" ", seg: segment('p)): string =>
  seg
  |> List.map(piece_to_string(~holes, ~concave_holes))
  |> String.concat("")
and piece_to_string =
    (~holes: string, ~concave_holes: string, p: piece('p)): string =>
  switch (p) {
  | Tile(t) => tile_to_string(~holes, ~concave_holes, t)
  | Grout({shape: Concave, _}) => concave_holes
  | Grout({shape: Convex, _}) => holes
  | Secondary(w) => Secondary.get_string(w.content)
  | Projector(_p) => "🎦" // TODO(andrew): print projectors
  }
and tile_to_string =
    (~holes: string, ~concave_holes: string, t: tile('p)): string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(List.nth(t.label), segment_to_string(~holes, ~concave_holes))
  |> String.concat("");

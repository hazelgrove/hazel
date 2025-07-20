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

// This is for comment insertion
let mk_secondary = (id, content) => [
  Secondary({
    id,
    content,
  }),
];

let rec map_piece = (~f_piece, x: piece) => {
  let rec_call = (piece: piece) => {
    switch (piece) {
    | Tile(t) =>
      Tile({
        ...t,
        children: t.children |> List.map(List.map(map_piece(~f_piece))),
      })
    | Grout(_)
    | Secondary(_)
    | Projector(_) => piece
    };
  };
  x |> f_piece(rec_call);
};

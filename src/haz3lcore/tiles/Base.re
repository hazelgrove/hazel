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
  model: 'p,
};

// This is for comment insertion
let mk_secondary = (id, content) => [
  Secondary({
    id,
    content,
  }),
];

let mk_projector = model => {
  id: Id.mk(),
  model,
};

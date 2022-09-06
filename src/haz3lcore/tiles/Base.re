open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Whitespace(Whitespace.t)
and tile = {
  // invariants:
  // - length(mold.in_) + 1 == length(label)
  // - length(shards) <= length(label)
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  id: Id.t,
  label: Label.t,
  mold: Mold.t,
  shards: list(int),
  children: list(segment),
};

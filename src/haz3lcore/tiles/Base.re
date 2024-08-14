open Util;

/* The different kinds of projector. New projectors
 * types need to be registered here in order to be
 * able to create and update their instances */
[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Fold
  | Info
  | Checkbox
  | Slider
  | SliderF
  | TextArea;

[@deriving (show({with_path: false}), sexp, yojson)]
type segment('a) = list(piece('a))
and piece('a) =
  | Tile(tile('a))
  | Grout(Grout.t)
  | Secondary(Secondary.t)
  | Projector(projector('a))
and tile('a) = {
  // invariants:
  // - length(mold.in_) + 1 == length(label)
  // - length(shards) <= length(label)
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  extra: 'a,
  label: Label.t,
  mold: Mold.t,
  shards: list(int),
  children: list(segment('a)),
}
and projector('a) = {
  kind,
  extra: 'a,
  syntax: piece('a),
  model: string,
};

// This is for comment insertion
let mk_secondary = (id, content) => [Secondary({id, content})];

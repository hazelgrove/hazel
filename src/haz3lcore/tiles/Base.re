open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type segment = list(piece)
and piece =
  | Whitespace(Whitespace.t)
  // TODO rename to Hole
  | Grout(Hole.t)
  | Tile(tile)
and tile = Aba.t(Shard.t, segment);

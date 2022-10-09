open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type segment = Aba.t(Grout.t, tile)
and tile =
  | T(Aba.t(Shard.t, segment));

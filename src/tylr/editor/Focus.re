open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Point
  | Select(Dir.t, Zigg.t);

let is_empty =
  fun
  | Point => true
  | Select(_) => false;

// let clear =
//   fun
//   | Point => []
//   | Select(_, zigg) => Zigg.clear(zigg);

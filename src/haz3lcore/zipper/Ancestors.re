open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type generation = (Ancestor.t, Siblings.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = list(generation);

let empty = [];

let parent: t => option(Ancestor.t) =
  fun
  | [] => None
  | [(parent, _), ..._] => Some(parent);

let sort = (root: Sort.t) =>
  fun
  | [] => root
  | [(a, _), ..._] => Ancestor.sort(a);

let zip_gen = (seg: Segment.t, (a, (pre, suf)): generation): Segment.t =>
  pre @ [Piece.Tile(Ancestor.zip(seg, a)), ...suf];
let zip = (seg: Segment.t, ancs: t) => ancs |> List.fold_left(zip_gen, seg);

let local_missing_shards = (ancs: t): list(Tile.t) =>
  switch (ancs) {
  | [] => []
  | [(a, _), ..._] => Ancestor.missing_middle_shards(a)
  };

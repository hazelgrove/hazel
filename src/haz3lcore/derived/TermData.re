open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type data = {
  range: (int, int),
  base_seg: Segment.t,
  root_piece: Piece.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(data);

let mk = (p: Piece.t, skel: Skel.t, seg: Segment.t): data => {
  range: Skel.range(skel),
  base_seg: seg,
  root_piece: p,
};

let extremes = (id: Id.t, data: t) => {
  let {range: (l, r), base_seg, _} = Id.Map.find(id, data);
  try((List.nth(base_seg, l), List.nth(base_seg, r))) {
  | Not_found => failwith("TermData: Invalid range")
  };
};

let root_tile = (id: Id.t, data: t): Tile.t =>
  switch (Id.Map.find(id, data)) {
  | {root_piece: Tile(t), _} => t
  | _ => failwith("TermData: root_tile: invalid data")
  };

let root_tile_opt = (id: Id.t, data: t): option(Tile.t) =>
  switch (Id.Map.find(id, data)) {
  | {root_piece: Tile(t), _} => Some(t)
  | _ => None
  };

let segment = (id: Id.t, data: t): option(Segment.t) => {
  open OptUtil.Syntax;
  let+ {base_seg, range, _} = Id.Map.find_opt(id, data);
  ListUtil.sublist(range, base_seg);
};

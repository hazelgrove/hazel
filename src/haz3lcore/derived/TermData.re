open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type data = {
  skel: Skel.t,
  sort: Sort.t,
  base_seg: Segment.t,
  root_piece: Piece.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(data);

let empty: t = Id.Map.empty;

let mk = (p: Piece.t, sort: Sort.t, skel: Skel.t, base_seg: Segment.t): data => {
  skel,
  sort,
  base_seg,
  root_piece: p,
};

let root_tile = (id: Id.t, data: t): option(Tile.t) =>
  switch (Id.Map.find_opt(id, data)) {
  | Some({root_piece: Tile(t), _}) => Some(t)
  | _ => None
  };

let sort = (id: Id.t, data: t): Sort.t =>
  switch (Id.Map.find_opt(id, data)) {
  | Some({sort, _}) => sort
  | None => Any
  };

let extremes_opt = (id: Id.t, data: t) =>
  /* This currently fails for singleton labelled tuples due
     to their maketerm hack, otherwise the extreme functions
     could be failwiths instead of options */
  switch (Id.Map.find_opt(id, data)) {
  | Some({skel, base_seg, _}) =>
    let (l, r) = Skel.range(skel);
    switch (List.nth(base_seg, l), List.nth(base_seg, r)) {
    | exception _ => None
    | (l, r) => Some((l, r))
    };
  | None => None
  };

let extremes_shards = (id: Id.t, data: t): option((Piece.t, Piece.t)) =>
  switch (extremes_opt(id, data)) {
  | Some((Tile(l), Tile(r))) =>
    Some((
      Tile(Tile.shard_of(l, Tile.l_shard(l))),
      Tile(Tile.shard_of(r, Tile.r_shard(r))),
    ))
  | Some((l, r)) => Some((l, r))
  | None => None
  };

let root_shards = (id: Id.t, data: t): option((Piece.t, Piece.t)) =>
  switch (Id.Map.find_opt(id, data)) {
  | Some({root_piece: Tile(t), _}) =>
    Some((
      Tile(Tile.shard_of(t, Tile.l_shard(t))),
      Tile(Tile.shard_of(t, Tile.r_shard(t))),
    ))
  | Some({root_piece, _}) => Some((root_piece, root_piece))
  | _ => None
  };

let extreme_ids = (id: Id.t, data: t): option((Id.t, Id.t)) =>
  switch (extremes_opt(id, data)) {
  | Some((l, r)) => Some((Piece.id(l), Piece.id(r)))
  | None => None
  };

let extreme_measures = (id: Id.t, data: t, measured: Measured.t) =>
  switch (extremes_opt(id, data)) {
  | Some((l, r)) =>
    switch (
      Measured.find_p(l, measured).origin,
      Measured.find_p(r, measured).last,
    ) {
    | exception _ => None
    | (l, r) => Some((l, r))
    }
  | None => None
  };

/* The segment corresponding to the `id` term */
let segment = (id: Id.t, data: t): option(Segment.t) => {
  let+ {base_seg, skel, _} = Id.Map.find_opt(id, data);
  let (l, r) = Skel.range(skel);
  ListUtil.sublist((l, r + 1), base_seg);
};

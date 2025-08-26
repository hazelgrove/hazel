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

let extremes_opt = (id: Id.t, data: t) =>
  /* This currently fails for singleton labelled tuples due
     to their maketerm hack, otherwise the extreme functions
     could be failwiths instead of options */
  switch (Id.Map.find_opt(id, data)) {
  | Some({range: (l, r), base_seg, _}) =>
    switch (List.nth(base_seg, l), List.nth(base_seg, r)) {
    | exception _ => None
    | (l, r) => Some((l, r))
    }
  | None => None
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

let root_tile_opt = (id: Id.t, data: t): option(Tile.t) =>
  switch (Id.Map.find_opt(id, data)) {
  | Some({root_piece: Tile(t), _}) => Some(t)
  | _ => None
  };

/* The segment corresponding to the `id` term */
let segment = (id: Id.t, data: t): option(Segment.t) => {
  open OptUtil.Syntax;
  let+ {base_seg, range: (l, r), _} = Id.Map.find_opt(id, data);
  ListUtil.sublist((l, r + 1), base_seg);
};

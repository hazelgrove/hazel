open Util;

// module Prefix = Affix.Make(Orientation.L);
// module Suffix = Affix.Make(Orientation.R);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (Segment.t, Segment.t);

let empty = Segment.(empty, empty);

let unzip: (int, Segment.t) => t = ListUtil.split_n;
let zip = (~sel=Segment.empty, (pre, suf): t) =>
  Segment.concat([pre, sel, suf]);

let prepend = (d: Direction.t, seg: Segment.t, (l, r): t): t =>
  switch (d) {
  | Left => (l @ seg, r)
  | Right => (l, seg @ r)
  };

let concat = (sibss: list(t)): t =>
  sibss
  |> List.split
  |> PairUtil.map_fst(List.rev)
  |> PairUtil.map_fst(List.concat)
  |> PairUtil.map_snd(List.concat);

// let consistent_shards = ((pre, suf): t): bool => {
//   let shards_pre = Prefix.shards(pre);
//   let shards_suf = Suffix.shards(suf);
//   ListUtil.group_by(Shard.id, shards_pre @ shards_suf)
//   |> List.for_all(((_, shards)) => Shard.consistent_molds(shards) != []);
// };

let remold = ((pre, _) as sibs: t, s: Sort.t): t =>
  Segment.remold(zip(sibs), s) |> unzip(List.length(pre));

let shapes = ((pre, suf): t) => {
  let s = Nib.Shape.concave();
  let (_, l, _) = Segment.shape_affix(Left, pre, s);
  let (_, r, _) = Segment.shape_affix(Right, suf, s);
  (l, r);
};

let is_mismatch = ((l, r): t): bool => {
  /* predicts if grout is neccessary between siblings */
  switch (Segment.edge_shape_of(Left, r), Segment.edge_shape_of(Right, l)) {
  | (None, _)
  | (_, None) => false
  | (s1, s2) => s1 == s2
  };
};

let contains_matching = (t: Tile.t, (pre, suf): t) =>
  Segment.(contains_matching(t, pre) || contains_matching(t, suf));

let push = (onto: Direction.t, p: Piece.t, (pre, suf): t): t =>
  switch (onto) {
  | Left => (pre @ [p], suf)
  | Right => (pre, [p, ...suf])
  };

let pop = (from: Direction.t, (pre, suf): t): option((Piece.t, t)) =>
  switch (from) {
  | Left =>
    ListUtil.split_last_opt(pre)
    |> Option.map(((pre, p)) => {
         let (pre', p) = Piece.pop_r(p);
         (p, (pre @ pre', suf));
       })
  | Right =>
    ListUtil.split_first_opt(suf)
    |> Option.map(((p, suf)) => {
         let (p, suf') = Piece.pop_l(p);
         (p, (pre, suf' @ suf));
       })
  };

let incomplete_tiles = TupleUtil.map2(Segment.incomplete_tiles);

let split_by_matching = id => TupleUtil.map2(Segment.split_by_matching(id));

let reassemble = TupleUtil.map2(Segment.reassemble);

let regrout = ((pre, suf): t) => {
  open IdGen.Syntax;
  let s = Nib.Shape.concave();
  let* suf = Segment.regrout_affix(Right, suf, s);
  let+ (trim_l, s_l, pre) = Segment.regrout_affix(Left, pre, s);
  ((pre, s_l, trim_l), suf);
};

let neighbors: t => (option(Piece.t), option(Piece.t)) =
  ((l, r)) => (
    l == [] ? None : Some(ListUtil.last(l)),
    r == [] ? None : Some(List.hd(r)),
  );

let trim_secondary = ((l_sibs, r_sibs): t) => (
  Segment.trim_secondary(Right, l_sibs),
  Segment.trim_secondary(Left, r_sibs),
);

let trim_secondary_and_grout = ((l_sibs, r_sibs): t) => (
  Segment.trim_secondary_and_grout(Right, l_sibs),
  Segment.trim_secondary_and_grout(Left, r_sibs),
);

let direction_between = ((l, r): t): option(Direction.t) =>
  /* Facing direction of the shared nib between l & r */
  switch (Segment.edge_direction_of(Left, r)) {
  | None => Segment.edge_direction_of(Right, l)
  | d => d
  };

let sorted_children = TupleUtil.map2(Segment.sorted_children);

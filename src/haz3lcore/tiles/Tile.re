open Util;
open Base;
open Base.Tile;

// exception Ambiguous_molds;
// exception Invalid_mold;

type t = Base.Tile.t;

let id = t => t.id;

let is_complete = (t: t) => List.length(t.label) == List.length(t.shards);

let l_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.hd_opt(t.shards));
let r_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.last_opt(t.shards));

let has_end = (d: Direction.t, t) =>
  switch (d) {
  | Left => l_shard(t) == 0
  | Right => r_shard(t) == List.length(t.label) - 1
  };

let nibs = (t: t) => {
  let (l, _) = Mold.nibs(~index=l_shard(t), t.mold);
  let (_, r) = Mold.nibs(~index=r_shard(t), t.mold);
  (l, r);
};

let shapes = (t: t) => {
  let (l, r) = nibs(t);
  (l.shape, r.shape);
};

let sorted_children = ({mold, shards, children, _}: t) =>
  Aba.mk(shards, children)
  |> Aba.aba_triples
  |> List.map(((l, child, r)) => {
       let (_, l) = Mold.nibs(~index=l, mold);
       let (r, _) = Mold.nibs(~index=r, mold);
       (l.sort == r.sort ? l.sort : Any, child);
     });

let contained_children = (t: t): list((t, Base.segment, t)) =>
  Aba.mk(t.shards, t.children)
  |> Aba.aba_triples
  |> List.map(((l, child, r)) => {
       let l = {...t, shards: [l], children: []};
       let r = {...t, shards: [r], children: []};
       (l, child, r);
     });

let reassemble = (match: Aba.t(t, segment)): t => {
  let t = Aba.hd(match);
  let (shards, children) =
    match
    |> Aba.fold_right(
         (t, child, (shards, children)) =>
           (t.shards @ shards, t.children @ [child, ...children]),
         t => (t.shards, t.children),
       );
  // check lengths
  let _ = Aba.mk(shards, children);
  assert(List.sort(Int.compare, shards) == shards);
  {
    id: t.id,
    label: t.label,
    // note: this throws away molds on tiles other than hd.
    // in cases where those molds differ, reassembled tile
    // should undergo subsequent remolding.
    mold: t.mold,
    shards,
    children,
  };
};

let disassemble = Base.Tile.disassemble;
let to_piece = Base.Tile.to_piece;
let split_shards = Base.Tile.split_shards;
let disintegrate = Base.Tile.disintegrate;

open Util;
include Base;

exception Ambiguous_molds;
exception Invalid_mold;
exception Empty_tile;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = tile;

let ids = ((shards, _)) => List.map(Shard.id_, shards);

let is_complete = (t: t) => List.length(t.label) == List.length(t.shards);

let l_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.hd_opt(t.shards));
let r_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.last_opt(t.shards));

let complete = (d: Direction.t, t: t): list(Token.t) =>
  switch (d) {
  | Left =>
    ListUtil.range(l_shard(t)) |> List.rev_map(i => List.nth(t.label, i))
  | Right =>
    ListUtil.range(~lo=r_shard(t) + 1, List.length(t.label))
    |> List.map(i => List.nth(t.label, i))
  };

let has_end = (d: Direction.t, t) =>
  switch (d) {
  | Left => l_shard(t) == 0
  | Right => r_shard(t) == List.length(t.label) - 1
  };
let has_ends = t => has_end(Left, t) && has_end(Right, t);

let nibs = (t: t) => {
  let (l, _) = Mold.nibs(~index=l_shard(t), t.mold);
  let (_, r) = Mold.nibs(~index=r_shard(t), t.mold);
  (l, r);
};
let shapes = (t: t) => {
  let (l, r) = nibs(t);
  (l.shape, r.shape);
};

let to_piece = t => Tile(t);

let nibbed_children = ({mold, shards, children, _}: t) =>
  Aba.mk(shards, children)
  |> Aba.aba_triples
  |> List.map(((l, child, r)) => {
       let (_, l) = Mold.nibs(~index=l, mold);
       let (r, _) = Mold.nibs(~index=r, mold);
       ((l, r), child);
     });

let contained_children = (t: t): list((t, Base.segment, t)) =>
  Aba.mk(t.shards, t.children)
  |> Aba.aba_triples
  |> List.map(((l, child, r)) => {
       let l = {...t, shards: [l], children: []};
       let r = {...t, shards: [r], children: []};
       (l, child, r);
     });

// let remold = (t: t): list(t) =>
//   Molds.get(t.label) |> List.map(mold => {...t, mold});

let split_shards = (id, label, mold, shards) =>
  shards |> List.map(i => {id, label, mold, shards: [i], children: []});

// postcond: output segment is nonempty
let disassemble = ({id, label, mold, shards, children}: t): segment => {
  let shards = split_shards(id, label, mold, shards);
  Aba.mk(shards, children)
  |> Aba.join(s => [to_piece(s)], Fun.id)
  |> List.concat;
};

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

let pop_l = (tile: t): (piece, segment) =>
  disassemble(tile)
  |> ListUtil.split_first_opt
  |> OptUtil.get_or_raise(Empty_tile);
let pop_r = (tile: t): (segment, piece) =>
  disassemble(tile)
  |> ListUtil.split_last_opt
  |> OptUtil.get_or_raise(Empty_tile);


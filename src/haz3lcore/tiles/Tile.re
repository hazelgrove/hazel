open Util;
open Poly;
include Base;

exception Ambiguous_molds;
exception Invalid_mold;
exception Empty_tile;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = tile;

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

let to_piece = t => Tile(t);

let sorted_children = ({mold, shards, children, _}: t) =>
  Aba.mk(shards, children)
  |> Aba.aba_triples
  |> List.map(~f=((l, child, r)) => {
       let (_, l) = Mold.nibs(~index=l, mold);
       let (r, _) = Mold.nibs(~index=r, mold);
       (l.sort == r.sort ? l.sort : Any, child);
     });

let contained_children = (t: t): list((t, Base.segment, t)) =>
  Aba.mk(t.shards, t.children)
  |> Aba.aba_triples
  |> List.map(~f=((l, child, r)) => {
       let l = {
         ...t,
         shards: [l],
         children: [],
       };
       let r = {
         ...t,
         shards: [r],
         children: [],
       };
       (l, child, r);
     });

let shard_of = (t: t, i: int): t => {
  ...t,
  shards: [i],
  children: [],
};

let split_shards = (id, label, mold, shards) =>
  shards
  |> List.map(~f=i =>
       {
         id,
         label,
         mold,
         shards: [i],
         children: [],
       }
     );

let left_missing_shards = (t: t): list(t) =>
  List.init(l_shard(t), ~f=Fn.id) |> split_shards(t.id, t.label, t.mold);

let right_missing_shards = (t: t): list(t) =>
  List.init(List.length(t.label) - r_shard(t) - 1, ~f=i =>
    r_shard(t) + i + 1
  )
  |> split_shards(t.id, t.label, t.mold);

let missing_shards = (t: t): list(t) =>
  List.filter(
    ~f=i => !List.mem(t.shards, i, ~equal=Poly.equal),
    List.init(List.length(t.label), ~f=Fn.id),
  )
  |> split_shards(t.id, t.label, t.mold);

let effective_label = (t: t): list(string) =>
  List.map(~f=List.nth_exn(t.label), t.shards);

// postcond: output segment is nonempty
let disassemble = ({id, label, mold, shards, children}: t): segment => {
  let shards = split_shards(id, label, mold, shards);
  Aba.mk(shards, children)
  |> Aba.join(s => [to_piece(s)], Fn.id)
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
  assert(List.sort(~compare=Int.compare, shards) == shards);
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

open Util;
include Base;

exception Ambiguous_molds;
exception Invalid_mold;
exception Empty_tile;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = tile;

let id = t => t.id;

let is_complete = (t: t) => List.length(t.label) == List.length(t.shards);

let l_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.hd_opt(t.shards));
let r_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.last_opt(t.shards));

let has_ends = t =>
  l_shard(t) == 0 && r_shard(t) == List.length(t.label) - 1;

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

// let unique_mold = _ => failwith("todo unique_mold");

// module Match = {
//   type tile = t;

//   module Make = (O: Orientation.S) => {
//     [@deriving (show({with_path: false}), sexp, yojson)]
//     type t = Aba.t(Shard.t, segment);

//     let id = (m: t) => Aba.hd(m).tile_id;

//     let label = (m: t) => snd(Aba.hd(m).label);

//     let shards: t => list(Shard.t) = Aba.get_as;
//     // let children: t => list(segment) = Aba.get_bs;

//     let length = (m: t) => List.length(shards(m));

//     let mold = (m: t) => {
//       let molds =
//         switch (Shard.consistent_molds(shards(m))) {
//         | [] =>
//           // this should only happen upon construct/destruct,
//           // in which case everything will be subsequently remolded
//           Molds.get(label(m))
//         | [_, ..._] as molds => molds
//         };
//       assert(molds != []);
//       List.hd(molds);
//     };

//     let children = m =>
//       List.map(ListUtil.rev_if(O.d == Left), Aba.get_bs(m));

//     let join = (m: t): segment =>
//       m |> Aba.join(s => [Shard.to_piece(s)], Fun.id) |> List.flatten;

//     let complete = (m: t): option(tile) => {
//       let id = id(m);
//       let label = label(m);
//       let mold = mold(m);
//       length(m) == Label.length(label)
//         ? {
//           let children = ListUtil.rev_if(O.d == Left, children(m));
//           Some(Base.Tile.{id, label, mold, children});
//         }
//         : None;
//     };
//   };
// };

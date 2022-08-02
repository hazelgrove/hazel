open Sexplib.Std;
open Util;

exception Empty_shard_affix;

[@deriving (show({with_path: false}), sexp, yojson)]
type step = int;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  label: Label.t,
  mold: Mold.t,
  shards: (list(int), list(int)),
  children: (list(Segment.t), list(Segment.t)),
};

// TODO(d) revisit naming w.r.t. outer vs inner shards
let l_shard = a =>
  ListUtil.hd_opt(fst(a.shards)) |> OptUtil.get_or_raise(Empty_shard_affix);
let r_shard = a =>
  ListUtil.last_opt(snd(a.shards))
  |> OptUtil.get_or_raise(Empty_shard_affix);

let nibs = (a: t) => {
  let (l, _) = Mold.nibs(~index=l_shard(a), a.mold);
  let (_, r) = Mold.nibs(~index=r_shard(a), a.mold);
  (l, r);
};
let shapes = a => {
  let (l, r) = nibs(a);
  (l.shape, r.shape);
};

let zip = (child: Segment.t, {id, label, mold, shards, children}: t): Tile.t => {
  id,
  label,
  mold,
  shards: fst(shards) @ snd(shards),
  children: fst(children) @ [child, ...snd(children)],
};

let sorted_children = (a: t) => {
  let n = List.length(fst(a.children));
  let t = zip(Segment.empty, a);
  let (l, _, r) = ListUtil.split_nth(n, Tile.sorted_children(t));
  (l, r);
};

// TODO flatten with shard indices
// let step = (frame: t): step => {
//   let (prefix, _) = frame.children;
//   List.length(prefix);
// };

let remold = (a: t): list(t) =>
  Molds.get(a.label) |> List.map(mold => {...a, mold});

// let sort = (frame: t): Sort.t => {
//   assert(step(frame) >= 0 && step(frame) < List.length(frame.mold.in_));
//   List.nth(frame.mold.in_, step(frame));
// };
let sort = (a: t): Sort.t =>
  switch (a.shards) {
  | ([i, ..._], [j, ..._]) =>
    let (_, l) = Mold.nibs(~index=i, a.mold);
    let (r, _) = Mold.nibs(~index=j, a.mold);
    l.sort == r.sort ? l.sort : Any;
  | _ => raise(Empty_shard_affix)
  };

let disassemble =
    ({id, label, mold, shards, children: (kids_l, kids_r)}: t): Siblings.t => {
  let (shards_l, shards_r) =
    shards
    |> TupleUtil.map2(Tile.split_shards(id, label, mold))
    |> TupleUtil.map2(List.map(Tile.to_piece));
  let flatten = (shards, kids) =>
    Aba.mk(shards, kids) |> Aba.join(p => [p], Fun.id) |> List.flatten;
  (flatten(shards_l, kids_l), flatten(shards_r, kids_r));
};

let container_shards = (a: t): (Piece.t, Piece.t) => {
  let (shards_l, shards_r) =
    a.shards
    |> TupleUtil.map2(Tile.split_shards(a.id, a.label, a.mold))
    |> TupleUtil.map2(List.map(Tile.to_piece));
  let l =
    ListUtil.last_opt(shards_l) |> OptUtil.get_or_raise(Empty_shard_affix);
  let r =
    ListUtil.hd_opt(shards_r) |> OptUtil.get_or_raise(Empty_shard_affix);
  (l, r);
};

let reassemble = (match_l: Aba.t(Tile.t, Segment.t) as 'm, match_r: 'm): t => {
  // TODO(d) bit hacky, need to do a flip/orientation pass
  // let match_l = Aba.map_b(Segment.rev, match_l);
  let (t_l, t_r) = Tile.(reassemble(match_l), reassemble(match_r));
  assert(t_l.id == t_r.id);
  {
    id: t_l.id,
    label: t_l.label,
    mold: t_l.mold,
    shards: (t_l.shards, t_r.shards),
    children: (t_l.children, t_r.children),
  };
};

// module Match = {
//   module Prefix = Tile.Match.Make(Orientation.L);
//   module Suffix = Tile.Match.Make(Orientation.R);

//   type ancestor = t;
//   type t = (Prefix.t, Suffix.t);

//   let id = ((pre, _): t) => Prefix.id(pre);

//   let shards = ((pre, suf): t) =>
//     List.rev(Prefix.shards(pre)) @ Suffix.shards(suf);

//   let label = ((_, suf)) => Suffix.label(suf);

//   let length = ((pre, suf)) => Prefix.length(pre) + Suffix.length(suf);

//   let children = ((pre, suf)) => (
//     Prefix.children(pre),
//     Suffix.children(suf),
//   );

//   let mold = (m: t) => {
//     let molds =
//       switch (Shard.consistent_molds(shards(m))) {
//       | [] =>
//         // this should only happen upon construct/destruct,
//         // in which case everything will be subsequently remolded
//         Molds.get(label(m))
//       | [_, ..._] as molds => molds
//       };
//     assert(molds != []);
//     List.hd(molds);
//   };

//   let join = ((pre, suf): t) => (Prefix.join(pre), Suffix.join(suf));

//   let complete = (m: t): option(ancestor) => {
//     let id = id(m);
//     let label = label(m);
//     let mold = mold(m);
//     length(m) == Tile.Label.length(label)
//       ? Some({id, label, mold, children: children(m)}) : None;
//   };
// };

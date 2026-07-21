open Util;
include Base;

exception Ambiguous_molds;
exception Invalid_mold;
exception Empty_tile;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = tile;

let id = (t: t) => t.id;

let label = (t: t): Label.t => Form.label_of(t.form);
let mold = (t: t): Mold.t => Form.mold_of(t.form);
let has_label = (t: t, lbl: Label.t): bool => label(t) == lbl;
let arity = (t: t): int => List.length(label(t));
let token = (t: t, i: int): Token.t => List.nth(label(t), i);

let is_complete = (t: t) => arity(t) == List.length(t.shards);

let l_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.hd_opt(t.shards));
let r_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.last_opt(t.shards));

let has_end = (d: Direction.t, t) =>
  switch (d) {
  | Left => l_shard(t) == 0
  | Right => r_shard(t) == arity(t) - 1
  };

let nibs = (t: t) => {
  let (l, _) = Mold.nibs(~index=l_shard(t), mold(t));
  let (_, r) = Mold.nibs(~index=r_shard(t), mold(t));
  (l, r);
};

let shapes = (t: t) => {
  let (l, r) = nibs(t);
  (l.shape, r.shape);
};

let to_piece = t => Tile(t);

let sorted_children = (t: t) => {
  let mold = mold(t);
  Aba.mk(t.shards, t.children)
  |> Aba.aba_triples
  |> List.map(((l, child, r)) => {
       let (_, l) = Mold.nibs(~index=l, mold);
       let (r, _) = Mold.nibs(~index=r, mold);
       (l.sort == r.sort ? l.sort : Any, child);
     });
};

let contained_children = (t: t): list((t, Base.segment, t)) =>
  Aba.mk(t.shards, t.children)
  |> Aba.aba_triples
  |> List.map(((l, child, r)) => {
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

let split_shards = (id, form, shards) =>
  shards
  |> List.map(i =>
       {
         id,
         form,
         shards: [i],
         children: [],
       }
     );

let left_missing_shards = (t: t): list(t) =>
  List.init(l_shard(t), Fun.id) |> split_shards(t.id, t.form);

let right_missing_shards = (t: t): list(t) =>
  List.init(arity(t) - r_shard(t) - 1, i => r_shard(t) + i + 1)
  |> split_shards(t.id, t.form);

let missing_shards = (t: t): list(t) =>
  List.filter(i => !List.mem(i, t.shards), List.init(arity(t), Fun.id))
  |> split_shards(t.id, t.form);

let effective_label = (t: t): list(string) =>
  List.map(List.nth(label(t)), t.shards);

// postcond: output segment is nonempty
let disassemble = ({id, form, shards, children}: t): segment => {
  let shards = split_shards(id, form, shards);
  Aba.mk(shards, children)
  |> Aba.join(s => [to_piece(s)], Fun.id)
  |> List.concat;
};

let disintegrate = ({id, form, shards, _}: t): list(tile) => {
  split_shards(id, form, shards);
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
    // note: this throws away forms on tiles other than hd.
    // in cases where those forms differ (pending remold),
    // reassembled tile should undergo subsequent remolding.
    form: t.form,
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

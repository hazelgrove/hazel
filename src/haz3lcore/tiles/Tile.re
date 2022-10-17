open Util;
include Base;

exception Ambiguous_molds;
exception Invalid_mold;
exception Empty_tile;

module Form = {
  type t = list(Shard.Form.t);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = tile;

let shards: t => list(Shard.t) = Aba.get_as;

let ids = t => List.map(Shard.id_, shards(t));

let form = t => List.map(Shard.form_, shards(t));

let l_shard = t => Aba.first_a(t);
let r_shard = t => Aba.last_a(t);

let nibs = (t: t): Nibs.t => {
  let (l, _) = Shard.nibs(l_shard(t));
  let (_, r) = Shard.nibs(r_shard(t));
  (l, r);
};
let shapes = (t: t) => {
  let (l, r) = nibs(t);
  (l.shape, r.shape);
};

let to_piece = t => Tile(t);

let mono = s => Aba.mk([s], []);

let nibbed_children = t =>
  Aba.aba_triples(t)
  |> List.map(((l, child, r)) => {
       let (_, l) = Shard.nibs(l);
       let (r, _) = Shard.nibs(r);
       ((l, r), child);
     });

let contained_children = (t: t): list((t, Base.segment, t)) =>
  Aba.aba_triples(t)
  |> List.map(((l, child, r)) => (mono(l), child, mono(r)));

// postcond: output segment is nonempty
let disassemble = (t: t): segment => {
  t |> Aba.join(s => [to_piece(mono(s))], Fun.id) |> List.concat;
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

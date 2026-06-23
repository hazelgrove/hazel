open Util;
include Base;

exception Ambiguous_molds;
exception Invalid_mold;
exception Empty_tile;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = tile;

let id = (t: t) => t.id;

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
  |> List.map(((l, child, r)) => {
       let (_, l) = Mold.nibs(~index=l, mold);
       let (r, _) = Mold.nibs(~index=r, mold);
       (l.sort == r.sort ? l.sort : Any, child);
     });

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

let split_shards = (id, label, mold, shards) =>
  shards
  |> List.map(i =>
       {
         id,
         label,
         mold,
         shards: [i],
         children: [],
       }
     );

let left_missing_shards = (t: t): list(t) =>
  List.init(l_shard(t), Fun.id) |> split_shards(t.id, t.label, t.mold);

let right_missing_shards = (t: t): list(t) =>
  List.init(List.length(t.label) - r_shard(t) - 1, i => r_shard(t) + i + 1)
  |> split_shards(t.id, t.label, t.mold);

let missing_shards = (t: t): list(t) =>
  List.filter(
    i => !List.mem(i, t.shards),
    List.init(List.length(t.label), Fun.id),
  )
  |> split_shards(t.id, t.label, t.mold);

let effective_label = (t: t): list(string) =>
  List.map(List.nth(t.label), t.shards);

// postcond: output segment is nonempty
let disassemble = ({id, label, mold, shards, children}: t): segment => {
  let shards = split_shards(id, label, mold, shards);
  Aba.mk(shards, children)
  |> Aba.join(s => [to_piece(s)], Fun.id)
  |> List.concat;
};

let disintegrate = ({id, label, mold, shards, _}: t): list(tile) => {
  split_shards(id, label, mold, shards);
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
  /* DIAGNOSTIC (study crash hunt): out-of-order shards reach here in two
     distinct ways — duplicate tile ids (the match grouped >1 distinct id,
     e.g. [0,1,0,1]) vs a SINGLE tile whose shards are mis-ordered in the
     segment (1 id, e.g. [1,0]). Replaces a bare `assert` so the message
     identifies which; kept fatal so the editor's revert-to-previous-state
     path still fires (Printexc.to_string carries this into the crash screen). */
  if (List.sort(Int.compare, shards) != shards) {
    /* This match was grouped by a single id (split_by_matching), so all
       tiles below share that id. The two faults are told apart by whether
       shard indices REPEAT: more shard pieces than distinct indices means
       multiple physical tiles shared one id (duplicate-id fault, e.g.
       [0,1,0,1]); equal counts but unsorted means one tile's shards landed
       out of order in the segment (shard-order fault, e.g. [1,0]). */
    let tiles = Aba.get_as(match);
    let n_pieces = List.length(shards);
    let n_distinct = List.length(List.sort_uniq(Int.compare, shards));
    let per_tile =
      tiles
      |> List.map((t: t) =>
           Id.to_string(t.id)
           ++ ":["
           ++ String.concat(",", List.map(string_of_int, t.shards))
           ++ "]"
         )
      |> String.concat(" ");
    failwith(
      "Tile.reassemble: out-of-order shards ["
      ++ String.concat(",", List.map(string_of_int, shards))
      ++ "] grouped under id "
      ++ (
        switch (tiles) {
        | [t, ..._] => Id.to_string(t.id)
        | [] => "?"
        }
      )
      ++ " from pieces { "
      ++ per_tile
      ++ " } "
      ++ (
        n_pieces > n_distinct
          ? "=> DUPLICATE-ID fault (indices repeat: "
            ++ string_of_int(n_pieces)
            ++ " pieces, "
            ++ string_of_int(n_distinct)
            ++ " distinct indices)"
          : "=> single-tile SHARD-ORDER fault"
      ),
    );
  };
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

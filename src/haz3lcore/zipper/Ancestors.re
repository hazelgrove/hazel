open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type generation = (Ancestor.t, Siblings.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = list(generation);

let empty = [];

let parent: t => option(Ancestor.t) =
  fun
  | [] => None
  | [(parent, _), ..._] => Some(parent);

let sort =
  fun
  | [] => Sort.root
  | [(a, _), ..._] => Ancestor.sort(a);

/* === Shallow interning for pointer-stable zip-up === */

/* Tile interning: reuse a previously cached tile if its children
   are all pointer-identical. Keyed by tile ID. */
let tile_cache: Hashtbl.t(Id.t, Tile.t) = Hashtbl.create(128);
let () =
  ResettableMemo.register_resetter(() => Hashtbl.clear(tile_cache));

let shallow_eq_tile = (t1: Tile.t, t2: Tile.t): bool =>
  t1.label == t2.label
  && t1.mold == t2.mold
  && t1.shards == t2.shards
  && ListUtil.phys_equal_pointwise(t1.children, t2.children);

let intern_tile = (tile: Tile.t): Tile.t =>
  switch (Hashtbl.find_opt(tile_cache, tile.id)) {
  | Some(prev) when shallow_eq_tile(prev, tile) => prev
  | _ =>
    Hashtbl.replace(tile_cache, tile.id, tile);
    tile;
  };

/* Segment interning: reuse a previously cached segment at each
   ancestor level if all pieces are pointer-identical. Keyed by ancestor ID. */
let seg_cache: Hashtbl.t(Id.t, Segment.t) = Hashtbl.create(128);
let () =
  ResettableMemo.register_resetter(() => Hashtbl.clear(seg_cache));

let shallow_eq_piece = (p1: Piece.t, p2: Piece.t): bool =>
  switch (p1, p2) {
  | (Tile(t1), Tile(t2)) => t1 === t2
  | (Grout(g1), Grout(g2)) => g1 === g2
  | (Secondary(s1), Secondary(s2)) => s1 === s2
  | (Projector(pr1), Projector(pr2)) => pr1 === pr2
  | _ => false
  };

let rec shallow_eq_seg = (s1: Segment.t, s2: Segment.t): bool =>
  switch (s1, s2) {
  | ([], []) => true
  | ([p1, ...r1], [p2, ...r2]) =>
    shallow_eq_piece(p1, p2) && shallow_eq_seg(r1, r2)
  | _ => false
  };

let intern_seg = (key: Id.t, seg: Segment.t): Segment.t =>
  switch (Hashtbl.find_opt(seg_cache, key)) {
  | Some(prev) when shallow_eq_seg(prev, seg) => prev
  | _ =>
    Hashtbl.replace(seg_cache, key, seg);
    seg;
  };

let zip_gen = (seg: Segment.t, (a, (pre, suf)): generation): Segment.t => {
  let tile = intern_tile(Ancestor.zip(seg, a));
  let result = pre @ [Piece.Tile(tile), ...suf];
  intern_seg(a.id, result);
};
let zip = (seg: Segment.t, ancs: t) => ancs |> List.fold_left(zip_gen, seg);

let regrout = (ancs: t) =>
  List.fold_right(
    ((a, sibs): generation, regrouted) => {
      let regrouted = regrouted;
      let ((pre, l, trim_l), (trim_r, r, suf)) = Siblings.regrout(sibs);
      let (l', r') = TupleUtil.map2(Nib.shape, Ancestor.nibs(a));
      let trim_l = Segment.Trim.regrout((l, l'), trim_l);
      let trim_r = Segment.Trim.regrout((r', r), trim_r);
      let pre = pre @ Segment.Trim.to_seg(trim_l);
      let suf = Segment.Trim.to_seg(trim_r) @ suf;
      [(a, (pre, suf)), ...regrouted];
    },
    ancs,
    empty,
  );

let local_missing_shards = (ancs: t): list(Tile.t) =>
  switch (ancs) {
  | [] => []
  | [(a, _), ..._] => Ancestor.missing_middle_shards(a)
  };

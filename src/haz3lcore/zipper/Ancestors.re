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

let zip_gen = (seg: Segment.t, (a, (pre, suf)): generation): Segment.t =>
  pre @ [Piece.Tile(Ancestor.zip(seg, a)), ...suf];
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

let rec non_local_incomplete_tiles = (~idx as _=0, ancs: t) =>
  switch (ancs) {
  | [] => []
  | [(_a, (l, r)), ...rest] =>
    /* Skip first ancestor as those shards can be used in child */
    //TODO(andrew): ancestor incompletes
    //TODO(andrew): sibling deep incompletes (but not shallow ones)
    //let anc_miss = idx == 0 ? [] : Ancestor.container_shards_missing(a);

    Segment.incomplete_tiles_deep(l)
    @ Segment.incomplete_tiles_deep(r)
    @ non_local_incomplete_tiles(rest)
  };

let rec non_local_missing_shards = (~idx=0, ancs: t) =>
  switch (ancs) {
  | [] => []
  | [(a, (l, r)), ...rest] =>
    /* Skip first ancestor as those shards can be used in child */
    //TODO: non-middle missing anc shards when in diff children
    let anc_cont_miss =
      idx == 0 ? [] : Ancestor.container_shards_missing_middle(a);
    let anc_child_miss =
      fst(a.children)
      @ snd(a.children)
      |> List.concat
      |> Segment.incomplete_tiles_deep
      |> Segment.incomplete_tiles_to_missing_shards;
    let gen_missing =
      Segment.incomplete_tiles_to_missing_shards(
        Segment.incomplete_tiles_deep(l) @ Segment.incomplete_tiles_deep(r),
      );
    gen_missing
    @ anc_cont_miss
    @ anc_child_miss
    @ non_local_missing_shards(~idx=idx + 1, rest);
  };

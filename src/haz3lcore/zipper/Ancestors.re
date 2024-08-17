open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type generation = (Ancestor.t, Siblings.t);

[@deriving (show({with_path: false}), sexp, yojson)]
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
      let (l', r') = TupleUtil.map2(Nib.shape, Mold.nibs(a.mold));
      let trim_l = Segment.Trim.regrout(Left, (l, l'), trim_l);
      let trim_r = Segment.Trim.regrout(Right, (r', r), trim_r);
      let pre = pre @ Segment.Trim.to_seg(trim_l);
      let suf = Segment.Trim.to_seg(trim_r) @ suf;
      [(a, (pre, suf)), ...regrouted];
    },
    ancs,
    empty,
  );

let parent_matches = (t: Tile.t, ancs: t) =>
  switch (ancs) {
  | [] => false
  | [(a, _), ..._] => a.id == t.id
  };

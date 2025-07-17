open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type generation('p) = (Ancestor.t('p), Siblings.t('p));

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('p) = list(generation('p));

let empty = [];

let parent: t('p) => option(Ancestor.t('p)) =
  fun
  | [] => None
  | [(parent, _), ..._] => Some(parent);

let sort =
  fun
  | [] => Sort.root
  | [(a, _), ..._] => Ancestor.sort(a);

let zip_gen =
    (seg: Segment.t('p), (a, (pre, suf)): generation('p)): Segment.t('p) =>
  pre @ [Piece.Tile(Ancestor.zip(seg, a)), ...suf];
let zip = (seg: Segment.t('p), ancs: t('p)) =>
  ancs |> List.fold_left(zip_gen, seg);

let regrout = (ancs: t('p)) =>
  List.fold_right(
    ((a, sibs): generation('p), regrouted) => {
      let regrouted = regrouted;
      let ((pre, l, trim_l), (trim_r, r, suf)) = Siblings.regrout(sibs);
      let (l', r') = TupleUtil.map2(Nib.shape, Ancestor.nibs(a));
      let trim_l = Segment.Trim.regrout(Left, (l, l'), trim_l);
      let trim_r = Segment.Trim.regrout(Right, (r', r), trim_r);
      let pre = pre @ Segment.Trim.to_seg(trim_l);
      let suf = Segment.Trim.to_seg(trim_r) @ suf;
      [(a, (pre, suf)), ...regrouted];
    },
    ancs,
    empty,
  );

let parent_matches = (t: Tile.t('p), ancs: t('p)) =>
  switch (ancs) {
  | [] => false
  | [(a, _), ..._] => a.id == t.id
  };

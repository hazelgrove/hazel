open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('p) = (Segment.t('p), Segment.t('p));

let empty = Segment.(empty, empty);

let no_siblings: t('p) => bool = s => s == empty;

let unzip: (int, Segment.t('p)) => t('p) = ListUtil.split_n;
let zip = (~sel=Segment.empty, (pre, suf): t('p)) =>
  Segment.concat([pre, sel, suf]);

let prepend = (d: Direction.t, seg: Segment.t('p), (l, r): t('p)): t('p) =>
  switch (d) {
  | Left => (l @ seg, r)
  | Right => (l, seg @ r)
  };

let concat = (sibss: list(t('p))): t('p) =>
  sibss
  |> List.split
  |> PairUtil.map_fst(List.rev)
  |> PairUtil.map_fst(List.concat)
  |> PairUtil.map_snd(List.concat);

let remold = ((pre, _) as sibs: t('p), s: Sort.t): t('p) =>
  Segment.remold(zip(sibs), s) |> unzip(List.length(pre));

let shapes = ((pre, suf): t('p)) => {
  let s = Nib.Shape.concave();
  let (_, l, _) = Segment.shape_affix(Left, pre, s);
  let (_, r, _) = Segment.shape_affix(Right, suf, s);
  (l, r);
};

let contains_matching = (t: Tile.t('p), (pre, suf): t('p)) =>
  Segment.(contains_matching(t, pre) || contains_matching(t, suf));

let push = (onto: Direction.t, p: Piece.t('p), (pre, suf): t('p)): t('p) =>
  switch (onto) {
  | Left => (pre @ [p], suf)
  | Right => (pre, [p, ...suf])
  };

let pop =
    (from: Direction.t, (pre, suf): t('p)): option((Piece.t('p), t('p))) =>
  switch (from) {
  | Left =>
    ListUtil.split_last_opt(pre)
    |> Option.map(((pre, p)) => {
         let (pre', p) = Piece.pop_r(p);
         (p, (pre @ pre', suf));
       })
  | Right =>
    ListUtil.split_first_opt(suf)
    |> Option.map(((p, suf)) => {
         let (p, suf') = Piece.pop_l(p);
         (p, (pre, suf' @ suf));
       })
  };

let incomplete_tiles = TupleUtil.map2(Segment.incomplete_tiles, _);

let split_by_matching = id => TupleUtil.map2(Segment.split_by_matching(id));

let reassemble = TupleUtil.map2(Segment.reassemble, _);

let regrout = ((pre, suf): t('p)) => {
  let s = Nib.Shape.concave();
  let suf = Segment.regrout_affix(Right, suf, s);
  let (trim_l, s_l, pre) = Segment.regrout_affix(Left, pre, s);
  ((pre, s_l, trim_l), suf);
};

let left_neighbor: t('p) => option(Piece.t('p)) =
  ((l, _)) => ListUtil.last_opt(l);

let right_neighbor: t('p) => option(Piece.t('p)) =
  ((_, r)) => ListUtil.hd_opt(r);

let neighbors: t('p) => (option(Piece.t('p)), option(Piece.t('p))) =
  n => (left_neighbor(n), right_neighbor(n));

let trim_secondary = ((l_sibs, r_sibs): t('p)) => (
  Segment.trim_secondary(Right, l_sibs),
  Segment.trim_secondary(Left, r_sibs),
);

let direction_between = ((l, r): t('p)): option(Direction.t) =>
  /* Facing direction of the shared nib between l & r */
  switch (Segment.edge_direction_of(Left, r)) {
  | None => Segment.edge_direction_of(Right, l)
  | d => d
  };

let mold_fitting_between =
    (sort: Sort.t, p: Precedence.t, sibs: t('p)): Mold.t =>
  switch (direction_between(sibs)) {
  | Some(d) => Mold.chevron(sort, p, d)
  | None => Mold.mk_op(sort, [])
  };

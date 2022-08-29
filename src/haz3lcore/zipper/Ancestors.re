open Sexplib.Std;
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

let disassemble = ancs =>
  ancs
  |> List.map(((a, sibs)) =>
       Siblings.concat([Ancestor.disassemble(a), sibs])
     )
  |> Siblings.concat;

// let remold = (ancestors: t): list(t) =>
//   List.fold_right(
//     ((a, sibs), remolded) => {
//       open ListUtil.Syntax;
//       let+ ancestors = remolded
//       and+ sibs = Siblings.remold(sibs)
//       and+ a = Ancestor.remold(a);
//       [(a, sibs), ...ancestors];
//     },
//     ancestors,
//     [empty],
//   );

let skel = ((a, (pre, suf)): generation): Skel.t => {
  let n = List.length(pre);
  let a = (n, Ancestor.shapes(a));
  let pre =
    pre
    |> List.mapi((i, p) => (i, p))
    |> List.filter_map(((i, p)) =>
         Piece.shapes(p) |> Option.map(ss => (i, ss))
       );
  let suf =
    suf
    |> List.mapi((i, p) => (n + 1 + i, p))
    |> List.filter_map(((i, p)) =>
         Piece.shapes(p) |> Option.map(ss => (i, ss))
       );
  Skel.mk(pre @ [a, ...suf]);
};

// let sorts = (i, (a, (pre, suf)): generation) => {
//   let n = List.length(pre);
//   if (i < List.length(pre)) {
//     List.nth_opt(pre, i)
//     |> Option.map(Piece.sort)
//     |> OptUtil.get_or_raise(Invalid_argument("Ancestors.sort_out"))
//   } else if (i > n) {
//     List.nth_opt(suf, i - 1 - n)
//     |> Option.map(Piece.sort)
//     |> OptUtil.get_or_raise(Invalid_argument("Ancestors.sort_out"))
//   } else {
//     a.mold.out;
//   };
// };

// // messed up sorts using in_ instead of nib
let sort_rank_gen = ((a, sibs) as gen: generation, sort: Sort.t) => {
  let root_rank = Segment.sort_rank_root(zip_gen(Segment.empty, gen), sort);
  let cousins_rank = {
    let (l, r) = Ancestor.sorted_children(a);
    let (l', r') = Siblings.sorted_children(sibs);
    List.concat([l', l, r, r'])
    |> List.map(((s, seg)) => Segment.sort_rank(seg, s))
    |> List.fold_left((+), 0);
  };
  root_rank + cousins_rank;
};

let sort_rank = (ancestors: t) =>
  List.fold_right(
    ((a, _) as gen, (s, rank)) =>
      (Ancestor.sort(a), sort_rank_gen(gen, s) + rank),
    ancestors,
    (Sort.root, 0),
  )
  |> snd;

let shape_rank = (ancestors: t): int =>
  List.fold_right(
    ((a, sibs), rank) => {
      let (l, r) = Siblings.shapes(sibs);
      let (l', r') = Ancestor.shapes(a);
      Bool.to_int(!Nib.Shape.fits(l, l'))
      + Bool.to_int(!Nib.Shape.fits(r', r))
      + Siblings.shape_rank(sibs)
      + rank;
    },
    ancestors,
    0,
  );

let regrout = (ancs: t) =>
  List.fold_right(
    ((a, sibs): generation, regrouted) => {
      open IdGen.Syntax;
      let* regrouted = regrouted;
      let* ((pre, l, trim_l), (trim_r, r, suf)) = Siblings.regrout(sibs);
      let (l', r') = TupleUtil.map2(Nib.shape, Mold.nibs(a.mold));
      let* trim_l = Segment.Trim.regrout((l, l'), trim_l);
      let+ trim_r = Segment.Trim.regrout((r', r), trim_r);
      let pre = pre @ Segment.Trim.to_seg(trim_l);
      let suf = Segment.Trim.to_seg(trim_r) @ suf;
      [(a, (pre, suf)), ...regrouted];
    },
    ancs,
    IdGen.return(empty),
  );

let parent_matches = (t: Tile.t, ancs: t) =>
  switch (ancs) {
  | [] => false
  | [(a, _), ..._] => a.id == t.id
  };

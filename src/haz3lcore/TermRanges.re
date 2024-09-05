open Util;

include Id.Map;
type range = (Piece.t, Piece.t);
type nonrec t = t(range);

let union = union((_, range, _) => Some(range));

/* PERF: Up to 50% reduction in some cases by memoizing
 * this function. Might be better though to just do an
 * unmemoized traversal building a hashtbl avoiding unioning.

   TODO(andrew): Consider setting a limit for the hashtbl size */
let range_hash: Hashtbl.t(Tile.segment, Id.Map.t(range)) =
  Hashtbl.create(1000);

// NOTE: this calculation is out of sync with
// MakeTerm, which matches things like list brackets
// and case...end to separators inside eg list commas
// and rules `| p =>`. this calculation does not
// include the container in the ranges for those inner
// separators.
// TODO(d) fix or derive from other info
//
// tail-recursive in outer recursion
let rec mk' = (seg: Segment.t) => {
  let rec go = (skel: Skel.t): (range, t) => {
    let root = Skel.root(skel) |> Aba.map_a(List.nth(seg));
    let root_l = Aba.first_a(root);
    let root_r = Aba.last_a(root);
    let (range, unichild_map) =
      switch (skel) {
      | Op(_) => ((root_l, root_r), empty)
      | Pre(_, r) =>
        let ((_, r), map) = go(r);
        ((root_l, r), map);
      | Post(l, _) =>
        let ((l, _), map) = go(l);
        ((l, root_r), map);
      | Bin(l, _, r) =>
        let ((l, _), map_l) = go(l);
        let ((_, r), map_r) = go(r);
        ((l, r), union(map_l, map_r));
      };
    let between_child_map =
      Aba.get_bs(root)
      |> List.map(go)
      |> List.map(snd)
      |> List.fold_left(union, empty);
    let map =
      Aba.get_as(root)
      |> List.map(Piece.id)
      |> List.fold_left(
           (map, id) => Id.Map.add(id, range, map),
           union(between_child_map, unichild_map),
         );
    (range, map);
  };
  Segment.children(seg)
  |> List.fold_left(
       (map, kid) => union(map, mk(kid)),
       union(empty, snd(go(Segment.skel(seg)))),
     );
}
and mk = seg =>
  try(Hashtbl.find(range_hash, seg)) {
  | _ =>
    let res = mk'(seg);
    Hashtbl.add(range_hash, seg, res);
    res;
  };

// type sgek = {
//   seg: Segment.t,
//   skel: Skel.t,
// };

// let rec go = (f: sgek => sgek, {skel, seg}): sgek => {
//   switch (skel) {
//   | Op(r) => f({skel: Op(root_of(f, r)), seg})
//   | Pre(r, skel_r) =>
//     let skeg_r = go(f, {skel: skel_r, seg});
//     f({skel: Pre(root_of(f, r), skeg_r.skel), seg: skeg_r.seg});
//   | Post(skel_l, r) =>
//     let skeg_l = go(f, {skel: skel_l, seg});
//     f({skel: Post(skeg_l.skel, root_of(f, r)), seg: skeg_l.seg});
//   | Bin(skel_l, root, skel_r) =>
//     let skeg_l = go(f, {skel: skel_l, seg});
//     let skeg_r = go(f, {skel: skel_r, seg: skeg_l.seg});
//     f({
//       skel: Bin(skeg_l.skel, root_of(f, root), skeg_r.skel),
//       seg: skeg_r.seg,
//     });
//   };
// }
// and root_of = (f, r: Aba.t(int, Skel.t)): Skel.root => {
//   let g = Aba.map_b(skel => f({skel, seg}), r);
//   f({skel, seg});
// };

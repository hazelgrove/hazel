open Util;

include Id.Map;
type range = (Piece.t(Id.t), Piece.t(Id.t));
type nonrec t = t(range);

let union = union((_, range, _) => Some(range));

/* PERF: Up to 50% reduction in some cases by memoizing
 * this function. Might be better though to just do an
 * unmemoized traversal building a hashtbl avoiding unioning.

   TODO(andrew): Consider setting a limit for the hashtbl size */
let range_hash: Hashtbl.t(Tile.segment(Id.t), Id.Map.t(range)) =
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
let rec mk' = (seg: Segment.t(Id.t)) => {
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

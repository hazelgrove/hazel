open Util;

include Id.Map;
type range = (Piece.t, Piece.t);
type nonrec t = t(range);

//TODO(andrew): reinstate memo

/* PERF: Up to 50% reduction in some cases by memoizing
 * this function. Might be better though to just do an
 * unmemoized traversal building a hashtbl avoiding unioning */
// let range_hash = (type p): Hashtbl.t(Tile.segment(p), Id.Map.t(range(p))) =>
//   Hashtbl.create(1000);

// NOTE: this calculation is out of sync with
// MakeTerm, which matches things like list brackets
// and case...end to separators inside eg list commas
// and rules `| p =>`. this calculation does not
// include the container in the ranges for those inner
// separators.
// TODO(d) fix or derive from other info
//
// tail-recursive in outer recursion
let rec mk = (seg: Segment.t) => {
  assert(seg != []);
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
        (
          (l, r),
          Id.Map.union((_, range, _) => Some(range), map_l, map_r),
        );
      };
    let between_child_map =
      Aba.get_bs(root)
      |> List.map(go)
      |> List.map(snd)
      |> List.fold_left(
           (map1, map2) =>
             Id.Map.union((_, range, _) => Some(range), map1, map2),
           empty,
         );
    let map =
      Aba.get_as(root)
      |> List.map(Piece.id)
      |> List.fold_left(
           (map, id) => Id.Map.add(id, range, map),
           Id.Map.union(
             (_, range, _) => Some(range),
             between_child_map,
             unichild_map,
           ),
         );
    (range, map);
  };
  Segment.children(seg)
  |> List.fold_left(
       (map, kid) =>
         Id.Map.union((_, range, _) => Some(range), map, mk(kid)),
       Id.Map.union(
         (_, range, _) => Some(range),
         empty,
         snd(go(Segment.skel(seg))),
       ),
     );
};

// and mk = seg =>
//   try(Hashtbl.find(range_hash, seg)) {
//   | _ =>
//     let res = mk'(seg);
//     Hashtbl.add(range_hash, seg, res);
//     res;
//   };

let subseg = (seg: Segment.t, (start_idx: int, end_idx: int)): Segment.t =>
  ListUtil.sublist((start_idx, end_idx + 1), seg);

let rec split = (ids: list(Id.t), seg: Segment.t): Id.Map.t(Segment.t) => {
  let union = Id.Map.union((_, s, _) => Some(s));
  let rec go = (skel: Skel.t): ((int, int), Id.Map.t(Segment.t)) => {
    let root = Skel.root(skel);
    let root_l = Aba.first_a(root);
    let root_r = Aba.last_a(root); /* always the same as root_l except for bin? */
    let add_maybe = (range, map) => {
      /* This is rep_id i think? */
      let this_id = List.nth(seg, root_l) |> Piece.id;
      List.mem(this_id, ids)
        ? Id.Map.add(this_id, subseg(seg, range), map) : map;
    };
    let (range, outer_kids_map) =
      switch (skel) {
      | Op(_) =>
        let range = (root_l, root_r);
        (range, add_maybe(range, empty));
      | Pre(_, r) =>
        let ((_, r), map) = go(r);
        let range = (root_l, r);
        (range, add_maybe(range, map));
      | Post(l, _) =>
        let ((l, _), map) = go(l);
        let range = (l, root_r);
        (range, add_maybe(range, map));
      | Bin(l, _, r) =>
        let ((l, _), map_l) = go(l);
        let ((_, r), map_r) = go(r);
        let range = (l, r);
        (range, add_maybe(range, union(map_l, map_r)));
      };
    let inner_kids_map =
      Aba.get_bs(root)
      |> List.map(go)
      |> List.map(snd)
      |> List.fold_left(union, empty);
    (range, union(inner_kids_map, outer_kids_map));
  };
  Segment.children(seg)
  |> List.fold_left(
       (map: Id.Map.t(Segment.t), kid: Segment.t) =>
         union(map, split(ids, kid)),
       snd(go(Segment.skel(seg))),
     );
};

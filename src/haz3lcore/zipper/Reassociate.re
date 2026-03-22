open Util;

type t = ZipperBase.t;

module ShardKey = {
  type t = (Id.t, list(int));
  let compare = compare;
};

module ShardKeySet = Set.Make(ShardKey);

/* Deep reassociation: repair the smallest scope implicated by the
   current unresolved multi-delimiter demand. Complete forms inside
   that scope act as anchors: keep them unless the repair yields a
   strictly better complete interpretation locally. */
/* Give fresh IDs to ancestor shard pieces on the right side.
   This prevents duplicate (id, shard_index) after rescan converts
   a newly-inserted delimiter to the ancestor's ID. The left-side
   shards keep their original IDs (first-seen in L-to-R order).
   Returns (freshened_segment, updated_fresh_map). */
let freshen_ancestor_shards =
    (
      ancestor_id: Id.t,
      fresh_map: Id.Map.t((Id.t, list(int))),
      seg: Segment.t,
    )
    : (Segment.t, Id.Map.t((Id.t, list(int)))) =>
  List.fold_right(
    (p, (acc_seg, acc_map)) =>
      switch (p) {
      | Piece.Tile(t) when t.id == ancestor_id =>
        let fresh_id = Id.mk();
        (
          [
            Piece.Tile({
              ...t,
              id: fresh_id,
            }),
            ...acc_seg,
          ],
          Id.Map.add(fresh_id, (ancestor_id, t.shards), acc_map),
        );
      | _ => ([p, ...acc_seg], acc_map)
      },
    seg,
    ([], fresh_map),
  );

/* Flatten ancestors into siblings, stopping when the outstanding
   reassociation demand has been covered by traversed ancestors.
   Freshens right-side ancestor shards to prevent ID collisions during
   rescan. Once the current demand is satisfied,
   remaining outer ancestors are preserved — keeping reassociation local
   to the affected repair scope instead of the whole program. */
let rec flatten_to_cover_demand = (demand, siblings, affected_rev, ancestors, fresh_map) =>
  switch (ancestors) {
  | [] => (siblings, List.rev(affected_rev), [], fresh_map)
  | _ when ReassociateDemand.is_satisfied(demand) =>
    (siblings, List.rev(affected_rev), ancestors, fresh_map)
  | [((ancestor, parent_sibs) as generation), ...rest] =>
    let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
    let (right_dis, fresh_map) =
      freshen_ancestor_shards(ancestor.id, fresh_map, right_dis);
    let siblings =
      Siblings.concat([siblings, (left_dis, right_dis), parent_sibs]);
    let demand = ReassociateDemand.cover_by_label(demand, ancestor.label);
    flatten_to_cover_demand(
      demand,
      siblings,
      [generation, ...affected_rev],
      rest,
      fresh_map,
    );
  };

/* Repair incidental breakage from freshening: if a freshened shard
   survived rescan (still has its fresh ID) and no other piece was
   converted to take its original (id, shard_index), then it was
   merely shadowed by rescan's LIFO stack — not genuinely displaced.
   Restore its original ID so reassembly can group it with its siblings. */
let repair_fresh_ids =
    (fresh_map: Id.Map.t((Id.t, list(int))), siblings: Siblings.t)
    : Siblings.t =>
  if (Id.Map.is_empty(fresh_map)) {
    siblings;
  } else {
    let stolen_originals =
      List.fold_left(
        (acc, p) =>
          switch (p) {
          | Piece.Tile(t) => ShardKeySet.add((t.id, t.shards), acc)
          | _ => acc
          },
        ShardKeySet.empty,
        fst(siblings) @ snd(siblings),
      );
    let repair =
      List.map(
        fun
        | Piece.Tile(t) =>
          switch (Id.Map.find_opt(t.id, fresh_map)) {
          | Some((original_id, shards))
              when !ShardKeySet.mem((original_id, shards), stolen_originals) =>
            Piece.Tile({
              ...t,
              id: original_id,
            })
          | _ => Piece.Tile(t)
          }
        | p => p,
      );
    TupleUtil.map2(repair, siblings);
  };

let go = (z: t): t =>
  switch (ReassociateDemand.of_relatives(z.relatives)) {
  | None => z
  | Some(demand) =>
    let any_ancestor_match =
      List.exists(
        ((a: Ancestor.t, _)) => ReassociateDemand.touches_ancestor(demand, a),
        z.relatives.ancestors,
      );
    if (any_ancestor_match) {
      /* Cross-scope path: collect the smallest ancestor scope that
         can satisfy the current unresolved delimiter demand, repair only that
         scope, and keep the rewrite only if it improves local
         completeness without sacrificing anchored complete forms. */
      let (siblings, affected_ancestors, outer_ancestors, fresh_map) =
        flatten_to_cover_demand(
          demand,
          z.relatives.siblings,
          [],
          z.relatives.ancestors,
          Id.Map.empty,
        );
      let siblings =
        ReassociateDemand.crack_siblings(~demand, siblings)
        |> Siblings.rescan
        |> repair_fresh_ids(fresh_map);
      let base_scope =
        {
          Relatives.siblings: z.relatives.siblings,
          ancestors: affected_ancestors,
        };
      ReassociateScore.accept_candidate(
        ~base_scope,
        ~candidate_siblings=siblings,
        ~outer_ancestors,
        z,
      );
    } else {
      /* Local path: no ancestor participates, so repair only the
         current siblings and keep the rewrite only if the local scope
         becomes strictly better. */
      let cracked = ReassociateDemand.crack_siblings(~demand, z.relatives.siblings);
      if (cracked == z.relatives.siblings) {
        z;
      } else {
        let siblings = Siblings.rescan(cracked);
        let base_scope =
          {
            Relatives.siblings: z.relatives.siblings,
            ancestors: [],
          };
        ReassociateScore.accept_candidate(
          ~base_scope,
          ~candidate_siblings=siblings,
          ~outer_ancestors=z.relatives.ancestors,
          z,
        );
      };
    }
  };

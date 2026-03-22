open Util;

type t = ZipperBase.t;

type repair_stats = {
  complete_multitiles: int,
  incomplete_multitiles: int,
  preserved_anchors: int,
};

let empty_repair_stats = {
  complete_multitiles: 0,
  incomplete_multitiles: 0,
  preserved_anchors: 0,
};

let add_repair_stats = (a: repair_stats, b: repair_stats): repair_stats => {
  complete_multitiles: a.complete_multitiles + b.complete_multitiles,
  incomplete_multitiles: a.incomplete_multitiles + b.incomplete_multitiles,
  preserved_anchors: a.preserved_anchors + b.preserved_anchors,
};

let is_multidelimiter_label = (label: Label.t): bool => List.length(label) > 1;

module TokenKey = {
  type t = Token.t;
  let compare = compare;
};

module TokenSet = Set.Make(TokenKey);

type demand = {
  target_labels: list(Label.t),
  target_tokens: TokenSet.t,
};

let labels_of_shards = (shards: list(Tile.t)): list(Label.t) =>
  shards |> List.map((shard: Tile.t) => shard.label) |> List.sort_uniq(compare);

let token_set_of_shards = (shards: list(Tile.t)): TokenSet.t =>
  List.fold_left(
    (acc, shard) =>
      switch (Tile.effective_label(shard)) {
      | [tok] => TokenSet.add(tok, acc)
      | _ => acc
      },
    TokenSet.empty,
    shards,
  );

let rec deep_local_missing_shards_segment =
    (~side: Direction.t, seg: Segment.t): list(Tile.t) =>
  List.concat_map(
    fun
    | Piece.Tile(t) => {
        let self =
          if (!Tile.is_complete(t) && is_multidelimiter_label(t.label)) {
            switch (side) {
            | Left => Tile.right_missing_shards(t)
            | Right => Tile.left_missing_shards(t)
            };
          } else {
            [];
          };
        self
        @ List.concat_map(deep_local_missing_shards_segment(~side), t.children);
      }
    | _ => [],
    seg,
  );

let demand_of_relatives = ({siblings: (pre, suf), ancestors}: Relatives.t)
    : option(demand) => {
  let target_shards =
    deep_local_missing_shards_segment(~side=Left, pre)
    @ deep_local_missing_shards_segment(~side=Right, suf)
    @ Ancestors.local_missing_shards(ancestors);
  switch (target_shards) {
  | [] => None
  | shards =>
    Some({
        target_labels: labels_of_shards(shards),
        target_tokens: token_set_of_shards(shards),
      })
  };
};

let score_multitile =
    (~anchor_ids: list(Id.t), ~id: Id.t, ~label: Label.t, ~complete: bool)
    : repair_stats =>
  if (!is_multidelimiter_label(label)) {
    empty_repair_stats;
  } else {
    {
      complete_multitiles: complete ? 1 : 0,
      incomplete_multitiles: complete ? 0 : 1,
      preserved_anchors: complete && List.mem(id, anchor_ids) ? 1 : 0,
    };
  };

let rec collect_complete_anchor_ids_segment =
    (acc: list(Id.t), seg: Segment.t): list(Id.t) =>
  List.fold_left(
    (acc, p) =>
      switch (p) {
      | Piece.Tile(t) =>
        let acc =
          List.fold_left(collect_complete_anchor_ids_segment, acc, t.children);
        is_multidelimiter_label(t.label) && Tile.is_complete(t)
          ? [t.id, ...acc] : acc;
      | _ => acc
      },
    acc,
    seg,
  );

let collect_complete_anchor_ids_siblings =
    (acc: list(Id.t), ((pre, suf): Siblings.t)): list(Id.t) => {
  let acc = collect_complete_anchor_ids_segment(acc, pre);
  collect_complete_anchor_ids_segment(acc, suf);
};

let collect_complete_anchor_ids_ancestors =
    (acc: list(Id.t), ancs: Ancestors.t): list(Id.t) =>
  List.fold_left(
    (acc, (a, parent_sibs): Ancestors.generation) => {
      let acc =
        List.fold_left(collect_complete_anchor_ids_segment, acc, fst(a.children));
      let acc =
        List.fold_left(collect_complete_anchor_ids_segment, acc, snd(a.children));
      let total_shards =
        List.length(fst(a.shards)) + List.length(snd(a.shards));
      let acc =
        is_multidelimiter_label(a.label) && total_shards == List.length(a.label)
          ? [a.id, ...acc] : acc;
      collect_complete_anchor_ids_siblings(acc, parent_sibs);
    },
    acc,
    ancs,
  );

let complete_anchor_ids_of_relatives = (rs: Relatives.t): list(Id.t) => {
  let ids =
    collect_complete_anchor_ids_siblings([], rs.siblings)
    |> acc => collect_complete_anchor_ids_ancestors(acc, rs.ancestors);
  List.sort_uniq(compare, ids);
};

let rec repair_stats_of_segment =
    (~anchor_ids: list(Id.t), seg: Segment.t): repair_stats =>
  List.fold_left(
    (acc, p) =>
      switch (p) {
      | Piece.Tile(t) =>
        let self =
          score_multitile(
            ~anchor_ids,
            ~id=t.id,
            ~label=t.label,
            ~complete=Tile.is_complete(t),
          );
        let children =
          List.fold_left(
            (acc, child) =>
              add_repair_stats(acc, repair_stats_of_segment(~anchor_ids, child)),
            empty_repair_stats,
            t.children,
          );
        let acc = add_repair_stats(acc, self);
        add_repair_stats(acc, children)
      | _ => acc
      },
    empty_repair_stats,
    seg,
  );

let repair_stats_of_segments =
    (~anchor_ids: list(Id.t), segs: list(Segment.t)): repair_stats =>
  List.fold_left(
    (acc, seg) =>
      add_repair_stats(acc, repair_stats_of_segment(~anchor_ids, seg)),
    empty_repair_stats,
    segs,
  );

let repair_stats_of_siblings =
    (~anchor_ids: list(Id.t), ((pre, suf): Siblings.t)): repair_stats =>
  add_repair_stats(
    repair_stats_of_segment(~anchor_ids, pre),
    repair_stats_of_segment(~anchor_ids, suf),
  );

let repair_stats_of_ancestors =
    (~anchor_ids: list(Id.t), ancs: Ancestors.t): repair_stats =>
  List.fold_left(
    (acc, (a, parent_sibs): Ancestors.generation) => {
      let total_shards =
        List.length(fst(a.shards)) + List.length(snd(a.shards));
      let self =
        score_multitile(
          ~anchor_ids,
          ~id=a.id,
          ~label=a.label,
          ~complete=total_shards == List.length(a.label),
        );
      let children =
        repair_stats_of_segments(~anchor_ids, fst(a.children) @ snd(a.children));
      let parent_sibs = repair_stats_of_siblings(~anchor_ids, parent_sibs);
      let acc = add_repair_stats(acc, self);
      let acc = add_repair_stats(acc, children);
      add_repair_stats(acc, parent_sibs)
    },
    empty_repair_stats,
    ancs,
  );

let repair_stats_of_relatives =
    (~anchor_ids: list(Id.t), rs: Relatives.t): repair_stats =>
  add_repair_stats(
    repair_stats_of_siblings(~anchor_ids, rs.siblings),
    repair_stats_of_ancestors(~anchor_ids, rs.ancestors),
  );

let should_accept_local_repair =
    (base_stats: repair_stats, candidate_stats: repair_stats): bool =>
  candidate_stats.complete_multitiles > base_stats.complete_multitiles
  || (
    candidate_stats.complete_multitiles == base_stats.complete_multitiles
    && candidate_stats.preserved_anchors == base_stats.preserved_anchors
    && candidate_stats.incomplete_multitiles < base_stats.incomplete_multitiles
  );

let shard_pieces = (t: Tile.t) =>
  List.map(
    s =>
      Piece.Tile({
        ...t,
        shards: [s],
        children: [],
      }),
    t.shards,
  );

let tile_has_target_token_demand =
    (~side: Direction.t, ~target_tokens: TokenSet.t, t: Tile.t): bool =>
  (
    switch (side) {
    | Left => Tile.right_missing_shards(t)
    | Right => Tile.left_missing_shards(t)
    }
  )
  |> List.exists(shard =>
       switch (Tile.effective_label(shard)) {
       | [tok] => TokenSet.mem(tok, target_tokens)
       | _ => false
       }
     );

/* Recursively crack only along paths that lead to the currently
   relevant unresolved delimiter demand. This is narrower than
   "any incomplete descendant": unrelated complete tiles stay intact,
   but complete wrappers on the path to relevant demand still crack. */
let rec flatten_tiles_with_relevant_incomplete =
    (~side: Direction.t, ~target_tokens: TokenSet.t, seg: Segment.t)
    : (bool, Segment.t) =>
  List.fold_right(
    (p, (acc_has_relevant, acc_seg)) =>
      switch (p) {
      | Piece.Tile(t) =>
        let child_results =
          List.map(
            flatten_tiles_with_relevant_incomplete(~side, ~target_tokens),
            t.children,
          );
        let child_has_relevant =
          List.exists(((has_relevant, _)) => has_relevant, child_results);
        let self_has_relevant =
          !Tile.is_complete(t)
          && is_multidelimiter_label(t.label)
          && tile_has_target_token_demand(~side, ~target_tokens, t);
        let has_relevant = self_has_relevant || child_has_relevant;
        if (
          Tile.is_complete(t)
          && is_multidelimiter_label(t.label)
          && child_has_relevant
        ) {
          let flattened_children = List.map(snd, child_results);
          let cracked =
            Aba.mk(shard_pieces(t), flattened_children)
            |> Aba.join(p => [p], Fun.id)
            |> List.flatten;
          (has_relevant || acc_has_relevant, cracked @ acc_seg);
        } else {
          (has_relevant || acc_has_relevant, [p, ...acc_seg]);
        };
      | _ => (acc_has_relevant, [p, ...acc_seg])
      },
    seg,
    (false, []),
  );

let flatten_tiles_with_target_demand =
    (~side: Direction.t, ~target_tokens: TokenSet.t, seg: Segment.t)
    : Segment.t =>
  flatten_tiles_with_relevant_incomplete(~side, ~target_tokens, seg) |> snd;

let crack_siblings_to_target_demand =
    (~target_tokens: TokenSet.t, (pre, suf): Siblings.t): Siblings.t => (
  flatten_tiles_with_target_demand(~side=Left, ~target_tokens, pre),
  flatten_tiles_with_target_demand(~side=Right, ~target_tokens, suf),
);

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

/* Flatten ancestors into siblings, stopping when all target labels
   have been found. Freshens right-side ancestor shards to prevent
   ID collisions during rescan. When a matching ancestor is found,
   its label is removed from targets; once all targets are satisfied,
   remaining outer ancestors are preserved — keeping reassociation local
   to the affected repair scope instead of the whole program. */
let rec flatten_to_match =
    (target_labels, siblings, affected_rev, ancestors, fresh_map) =>
  switch (ancestors) {
  | [] => (siblings, List.rev(affected_rev), [], fresh_map)
  | _ when target_labels == [] =>
    (siblings, List.rev(affected_rev), ancestors, fresh_map)
  | [((ancestor, parent_sibs) as generation), ...rest] =>
    let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
    let (right_dis, fresh_map) =
      freshen_ancestor_shards(ancestor.id, fresh_map, right_dis);
    let siblings =
      Siblings.concat([siblings, (left_dis, right_dis), parent_sibs]);
    let target_labels = List.filter(l => l != ancestor.label, target_labels);
    flatten_to_match(
      target_labels,
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

let maybe_accept_local_repair =
    (
      ~base_scope: Relatives.t,
      ~candidate_siblings: Siblings.t,
      ~outer_ancestors: Ancestors.t,
      z: t,
    )
    : t => {
  let anchor_ids = complete_anchor_ids_of_relatives(base_scope);
  let base_stats = repair_stats_of_relatives(~anchor_ids, base_scope);
  let local_relatives =
    {
      Relatives.siblings: candidate_siblings,
      ancestors: [],
    }
    |> Relatives.reassemble;
  let candidate_stats = repair_stats_of_relatives(~anchor_ids, local_relatives);
  if (should_accept_local_repair(base_stats, candidate_stats)) {
    {
      ...z,
      relatives: {
        siblings: local_relatives.siblings,
        ancestors: local_relatives.ancestors @ outer_ancestors,
      },
    };
  } else {
    z;
  };
};

let go = (z: t): t =>
  switch (demand_of_relatives(z.relatives)) {
  | None => z
  | Some({target_labels, target_tokens}) =>
    let any_ancestor_match =
      List.exists(
        ((a: Ancestor.t, _)) => List.mem(a.label, target_labels),
        z.relatives.ancestors,
      );
    if (any_ancestor_match) {
      /* Cross-scope path: collect the smallest ancestor scope that
         can satisfy the current unresolved labels, repair only that
         scope, and keep the rewrite only if it improves local
         completeness without sacrificing anchored complete forms. */
      let (siblings, affected_ancestors, outer_ancestors, fresh_map) =
        flatten_to_match(
          target_labels,
          z.relatives.siblings,
          [],
          z.relatives.ancestors,
          Id.Map.empty,
        );
      let siblings =
        crack_siblings_to_target_demand(~target_tokens, siblings)
        |> Siblings.rescan
        |> repair_fresh_ids(fresh_map);
      let base_scope =
        {
          Relatives.siblings: z.relatives.siblings,
          ancestors: affected_ancestors,
        };
      maybe_accept_local_repair(
        ~base_scope,
        ~candidate_siblings=siblings,
        ~outer_ancestors,
        z,
      );
    } else {
      /* Local path: no ancestor participates, so repair only the
         current siblings and keep the rewrite only if the local scope
         becomes strictly better. */
      let cracked =
        crack_siblings_to_target_demand(~target_tokens, z.relatives.siblings);
      if (cracked == z.relatives.siblings) {
        z;
      } else {
        let siblings = Siblings.rescan(cracked);
        let base_scope =
          {
            Relatives.siblings: z.relatives.siblings,
            ancestors: [],
          };
        maybe_accept_local_repair(
          ~base_scope,
          ~candidate_siblings=siblings,
          ~outer_ancestors=z.relatives.ancestors,
          z,
        );
      };
    }
  };

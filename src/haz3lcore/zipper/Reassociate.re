open Util;

type t = ZipperBase.t;

/* Reassociation tries to reconcile two competing notions of intent:
   textual completion and structural stability.

   Terms used here:
   - Anchor: an already-complete multi-delimiter form in the affected scope.
     Anchors are evidence of previously committed user intent and should not
     be broken casually, because probe placement and intermediate feedback
     depend on them staying stable through incomplete edit states.
   - Demand: unresolved delimiter obligations induced by the local edit cone.
     The current implementation tracks these as side-specific compatible
     token obligations, not by rigid form identity.
   - Repair scope: the smallest sibling/ancestor region we choose to crack,
     rescan, and potentially rewrite after an edit.

   Intended behavior:
   - If the edited region is still incomplete, preserve anchored complete
     structure as much as possible.
   - If the edited region becomes textually delimiter-complete, realize a
     structurally complete interpretation rather than preserving stale history.
   - Compatibility should be token-based (`end` with `end`, `->` with `->`),
     even across different forms like `case`/`test` or `fun`/`fix`; we do not
     want historical form identity to prevent sensible rematching.

   Current approximation:
   - Demand collection and cracking are directional and token-guided.
   - Scope expansion is driven by actual tokens exposed by each ancestor
     generation, not whole-label matching.
   - Acceptance is local and anchor-preserving.

   Remaining gap:
   - The ideal formulation is path-sensitive token obligations: still token
     compatible across forms, but more precise than the current side-specific
     obligation lists about which outstanding obligations are nearest/relevant
     and which ancestor paths actually expose them. */

module ShardKey = {
  type t = (Id.t, list(int));
  let compare = compare;
};

module ShardKeySet = Set.Make(ShardKey);

module TokenKey = {
  type t = Token.t;
  let compare = compare;
};

module TokenSet = Set.Make(TokenKey);

type demand = {
  left_obligations: list(Token.t),
  right_obligations: list(Token.t),
  left_target_tokens: TokenSet.t,
  right_target_tokens: TokenSet.t,
};

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

/* Demand */

let token_of_shard = (shard: Tile.t): option(Token.t) =>
  switch (Tile.effective_label(shard)) {
  | [tok] => Some(tok)
  | _ => None
  };

let tokens_of_shards = (shards: list(Tile.t)): list(Token.t) =>
  List.filter_map(token_of_shard, shards);

let token_set_of_tokens = (tokens: list(Token.t)): TokenSet.t =>
  List.fold_left((acc, tok) => TokenSet.add(tok, acc), TokenSet.empty, tokens);

let rec deep_local_missing_shards_segment =
    (~side: Direction.t, seg: Segment.t): list(Tile.t) =>
  (
    switch (side) {
    | Left => List.rev(seg)
    | Right => seg
    }
  )
  |> List.concat_map(
       fun
       | Piece.Tile(tile) => {
           let self =
             if (!Tile.is_complete(tile) && is_multidelimiter_label(tile.label)) {
               switch (side) {
               | Left => Tile.right_missing_shards(tile)
               | Right => Tile.left_missing_shards(tile)
               };
             } else {
               [];
             };
           let children =
             switch (side) {
             | Left => List.rev(tile.children)
             | Right => tile.children
             }
             |> List.concat_map(deep_local_missing_shards_segment(~side));
           self @ children;
         }
       | _ => [],
     );

let left_target_tokens = (demand: demand): TokenSet.t => demand.left_target_tokens;

let right_target_tokens = (demand: demand): TokenSet.t => demand.right_target_tokens;

let rec tokens_of_segment = (seg: Segment.t): list(Token.t) =>
  List.concat_map(tokens_of_piece, seg)

and tokens_of_piece = (piece: Piece.t): list(Token.t) =>
  switch (piece) {
  | Piece.Tile(tile) when List.length(tile.shards) == 1 && tile.children == [] =>
    tokens_of_shards([tile])
  | Piece.Tile(tile) => tokens_of_segment(Tile.disassemble(tile))
  | _ => []
  };

let consume_tokens_in_order =
    (obligations: list(Token.t), available: list(Token.t)): list(Token.t) => {
  let rec go = (obligations, available) =>
    switch (obligations, available) {
    | ([], _) => []
    | (_, []) => obligations
    | ([need, ...rest_need] as obligations, [tok, ...rest_available]) =>
      need == tok ? go(rest_need, rest_available) : go(obligations, rest_available)
    };
  go(obligations, available);
};

let demand_of_relatives = ({siblings: (pre, suf), ancestors}: Relatives.t): option(demand) => {
  let left_obligations =
    tokens_of_shards(deep_local_missing_shards_segment(~side=Left, pre))
    @ tokens_of_shards(Ancestors.local_missing_shards(ancestors));
  let right_obligations =
    tokens_of_shards(deep_local_missing_shards_segment(~side=Right, suf));
  switch (left_obligations, right_obligations) {
  | ([], []) => None
  | _ =>
    Some({
        left_obligations,
        right_obligations,
        left_target_tokens: token_set_of_tokens(left_obligations),
        right_target_tokens: token_set_of_tokens(right_obligations),
      })
  };
};

let demand_touches_generation =
    (demand: demand, ((ancestor, parent_sibs): Ancestors.generation)): bool => {
  let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
  let left_available = tokens_of_segment(fst(parent_sibs) @ left_dis);
  let right_available = tokens_of_segment(right_dis @ snd(parent_sibs));
  List.exists(tok => TokenSet.mem(tok, left_target_tokens(demand)), right_available)
  || List.exists(tok => TokenSet.mem(tok, right_target_tokens(demand)), left_available);
};

let demand_is_satisfied = (demand: demand): bool =>
  switch (demand.left_obligations, demand.right_obligations) {
  | ([], []) => true
  | _ => false
  };

let cover_demand_by_generation =
    (demand: demand, ((ancestor, parent_sibs): Ancestors.generation)): demand => {
  let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
  let left_available = tokens_of_segment(fst(parent_sibs) @ left_dis);
  let right_available = tokens_of_segment(right_dis @ snd(parent_sibs));
  let left_obligations =
    consume_tokens_in_order(demand.left_obligations, right_available);
  let right_obligations =
    consume_tokens_in_order(demand.right_obligations, left_available);
  {
    left_obligations,
    right_obligations,
    left_target_tokens: token_set_of_tokens(left_obligations),
    right_target_tokens: token_set_of_tokens(right_obligations),
  };
};

let tile_has_target_token_demand =
    (~side: Direction.t, ~target_tokens: TokenSet.t, tile: Tile.t): bool =>
  (
    switch (side) {
    | Left => Tile.right_missing_shards(tile)
    | Right => Tile.left_missing_shards(tile)
    }
  )
  |> List.exists(shard =>
       switch (Tile.effective_label(shard)) {
       | [tok] => TokenSet.mem(tok, target_tokens)
       | _ => false
       }
     );

/* Candidate Construction */

let shard_pieces = (tile: Tile.t) =>
  List.map(
    shard =>
      Piece.Tile({
        ...tile,
        shards: [shard],
        children: [],
      }),
    tile.shards,
  );

/* Recursively crack only along paths that lead to the currently
   relevant unresolved delimiter demand. This is narrower than
   "any incomplete descendant": unrelated complete tiles stay intact,
   but complete wrappers on the path to relevant demand still crack. */
let rec flatten_tiles_with_relevant_incomplete =
    (~side: Direction.t, ~target_tokens: TokenSet.t, seg: Segment.t)
    : (bool, Segment.t) =>
  List.fold_right(
    (piece, (acc_has_relevant, acc_seg)) =>
      switch (piece) {
      | Piece.Tile(tile) =>
        let child_results =
          List.map(
            flatten_tiles_with_relevant_incomplete(~side, ~target_tokens),
            tile.children,
          );
        let child_has_relevant =
          List.exists(((has_relevant, _)) => has_relevant, child_results);
        let self_has_relevant =
          !Tile.is_complete(tile)
          && is_multidelimiter_label(tile.label)
          && tile_has_target_token_demand(~side, ~target_tokens, tile);
        let has_relevant = self_has_relevant || child_has_relevant;
        if (
          Tile.is_complete(tile)
          && is_multidelimiter_label(tile.label)
          && child_has_relevant
        ) {
          let flattened_children = List.map(snd, child_results);
          let cracked =
            Aba.mk(shard_pieces(tile), flattened_children)
            |> Aba.join(piece => [piece], Fun.id)
            |> List.flatten;
          (has_relevant || acc_has_relevant, cracked @ acc_seg);
        } else {
          (has_relevant || acc_has_relevant, [piece, ...acc_seg]);
        };
      | _ => (acc_has_relevant, [piece, ...acc_seg])
      },
    seg,
    (false, []),
  );

let flatten_tiles_with_target_demand =
    (~side: Direction.t, ~target_tokens: TokenSet.t, seg: Segment.t): Segment.t =>
  flatten_tiles_with_relevant_incomplete(~side, ~target_tokens, seg) |> snd;

let crack_siblings = (~demand, (pre, suf): Siblings.t): Siblings.t => (
  flatten_tiles_with_target_demand(
    ~side=Left,
    ~target_tokens=left_target_tokens(demand),
    pre,
  ),
  flatten_tiles_with_target_demand(
    ~side=Right,
    ~target_tokens=right_target_tokens(demand),
    suf,
  ),
);

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
    (piece, (acc_seg, acc_map)) =>
      switch (piece) {
      | Piece.Tile(tile) when tile.id == ancestor_id =>
        let fresh_id = Id.mk();
        (
          [
            Piece.Tile({
              ...tile,
              id: fresh_id,
            }),
            ...acc_seg,
          ],
          Id.Map.add(fresh_id, (ancestor_id, tile.shards), acc_map),
        );
      | _ => ([piece, ...acc_seg], acc_map)
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
  | _ when demand_is_satisfied(demand) =>
    (siblings, List.rev(affected_rev), ancestors, fresh_map)
  | [((ancestor, parent_sibs) as generation), ...rest] =>
    let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
    let (right_dis, fresh_map) =
      freshen_ancestor_shards(ancestor.id, fresh_map, right_dis);
    let siblings =
      Siblings.concat([siblings, (left_dis, right_dis), parent_sibs]);
    let demand = cover_demand_by_generation(demand, generation);
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
    (fresh_map: Id.Map.t((Id.t, list(int))), siblings: Siblings.t): Siblings.t =>
  if (Id.Map.is_empty(fresh_map)) {
    siblings;
  } else {
    let stolen_originals =
      List.fold_left(
        (acc, piece) =>
          switch (piece) {
          | Piece.Tile(tile) => ShardKeySet.add((tile.id, tile.shards), acc)
          | _ => acc
          },
        ShardKeySet.empty,
        fst(siblings) @ snd(siblings),
      );
    let repair =
      List.map(
        fun
        | Piece.Tile(tile) =>
          switch (Id.Map.find_opt(tile.id, fresh_map)) {
          | Some((original_id, shards))
              when !ShardKeySet.mem((original_id, shards), stolen_originals) =>
            Piece.Tile({
              ...tile,
              id: original_id,
            })
          | _ => Piece.Tile(tile)
          }
        | piece => piece,
      );
    TupleUtil.map2(repair, siblings);
  };

/* Acceptance */

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
    (acc, piece) =>
      switch (piece) {
      | Piece.Tile(tile) =>
        let acc =
          List.fold_left(collect_complete_anchor_ids_segment, acc, tile.children);
        is_multidelimiter_label(tile.label) && Tile.is_complete(tile)
          ? [tile.id, ...acc] : acc;
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
    (acc, (ancestor, parent_sibs): Ancestors.generation) => {
      let acc =
        List.fold_left(
          collect_complete_anchor_ids_segment,
          acc,
          fst(ancestor.children),
        );
      let acc =
        List.fold_left(
          collect_complete_anchor_ids_segment,
          acc,
          snd(ancestor.children),
        );
      let total_shards =
        List.length(fst(ancestor.shards)) + List.length(snd(ancestor.shards));
      let acc =
        is_multidelimiter_label(ancestor.label)
        && total_shards == List.length(ancestor.label)
          ? [ancestor.id, ...acc] : acc;
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
    (acc, piece) =>
      switch (piece) {
      | Piece.Tile(tile) =>
        let self =
          score_multitile(
            ~anchor_ids,
            ~id=tile.id,
            ~label=tile.label,
            ~complete=Tile.is_complete(tile),
          );
        let children =
          List.fold_left(
            (acc, child) =>
              add_repair_stats(acc, repair_stats_of_segment(~anchor_ids, child)),
            empty_repair_stats,
            tile.children,
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
    (acc, (ancestor, parent_sibs): Ancestors.generation) => {
      let total_shards =
        List.length(fst(ancestor.shards)) + List.length(snd(ancestor.shards));
      let self =
        score_multitile(
          ~anchor_ids,
          ~id=ancestor.id,
          ~label=ancestor.label,
          ~complete=total_shards == List.length(ancestor.label),
        );
      let children =
        repair_stats_of_segments(
          ~anchor_ids,
          fst(ancestor.children) @ snd(ancestor.children),
        );
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

let accept_candidate =
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

/* Orchestration */

let go = (z: t): t =>
  switch (demand_of_relatives(z.relatives)) {
  | None => z
  | Some(demand) =>
    let any_ancestor_match =
      List.exists(
        generation => demand_touches_generation(demand, generation),
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
        crack_siblings(~demand, siblings)
        |> Siblings.rescan
        |> repair_fresh_ids(fresh_map);
      let base_scope =
        {
          Relatives.siblings: z.relatives.siblings,
          ancestors: affected_ancestors,
        };
      accept_candidate(
        ~base_scope,
        ~candidate_siblings=siblings,
        ~outer_ancestors,
        z,
      );
    } else {
      /* Local path: no ancestor participates, so repair only the
         current siblings and keep the rewrite only if the local scope
         becomes strictly better. */
      let cracked = crack_siblings(~demand, z.relatives.siblings);
      if (cracked == z.relatives.siblings) {
        z;
      } else {
        let siblings = Siblings.rescan(cracked);
        let base_scope =
          {
            Relatives.siblings: z.relatives.siblings,
            ancestors: [],
          };
        accept_candidate(
          ~base_scope,
          ~candidate_siblings=siblings,
          ~outer_ancestors=z.relatives.ancestors,
          z,
        );
      };
    }
  };

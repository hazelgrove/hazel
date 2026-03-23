open Util;

type t = ZipperBase.t;

/* Reassociation tries to reconcile two competing notions of intent:
   textual completion and structural stability.

   Terms used here:
   - Anchor: an already-complete multi-delimiter form in the affected scope.
     Anchors are evidence of previously committed user intent and should not
     be broken casually, because probe placement and intermediate feedback
     depend on them staying stable through incomplete edit states.
   - Request: unresolved delimiter obligations induced by the local edit cone.
     Requests are directional and token-compatible (`end` with `end`, `->`
     with `->`), but do not preserve historical form identity.
   - Repair scope: the smallest sibling/ancestor region we choose to crack,
     rescan, and potentially rewrite after an edit.

   Intended behavior:
   - If the edited region is still incomplete, preserve anchored complete
     structure as much as possible.
   - If the edited region becomes textually delimiter-complete, realize a
     structurally complete interpretation rather than preserving stale history.

   Current shape:
   - Scope expansion is driven by ordered requests consumed against the tokens
     each ancestor generation actually exposes.
   - Cracking inside that expanded scope is coarse: complete wrappers crack
     whenever they contain incomplete multi-delimiter descendants.
   - Acceptance is local and anchor-preserving. */

module ShardKey = {
  type t = (Id.t, list(int));
  let compare = compare;
};

module ShardKeySet = Set.Make(ShardKey);

type request = {
  left: list(Token.t),
  right: list(Token.t),
};

type repair_stats = {
  complete_multitiles: int,
  incomplete_multitiles: int,
  preserved_anchors: int,
};

type base_summary = {
  anchors: Id.Map.t(unit),
  complete_multitiles: int,
  incomplete_multitiles: int,
};

let empty_repair_stats = {
  complete_multitiles: 0,
  incomplete_multitiles: 0,
  preserved_anchors: 0,
};

let empty_base_summary = {
  anchors: Id.Map.empty,
  complete_multitiles: 0,
  incomplete_multitiles: 0,
};

let add_repair_stats = (a: repair_stats, b: repair_stats): repair_stats => {
  complete_multitiles: a.complete_multitiles + b.complete_multitiles,
  incomplete_multitiles: a.incomplete_multitiles + b.incomplete_multitiles,
  preserved_anchors: a.preserved_anchors + b.preserved_anchors,
};

let is_multidelimiter_label = (label: Label.t): bool => List.length(label) > 1;

/* Requests */

let token_of_shard = (shard: Tile.t): option(Token.t) =>
  switch (Tile.effective_label(shard)) {
  | [tok] => Some(tok)
  | _ => None
  };

let tokens_of_shards = (shards: list(Tile.t)): list(Token.t) =>
  List.filter_map(token_of_shard, shards);

let rec local_missing_tokens_segment =
    (~side: Direction.t, seg: Segment.t): list(Token.t) =>
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
               }
               |> tokens_of_shards;
             } else {
               [];
             };
           let children =
             switch (side) {
             | Left => List.rev(tile.children)
             | Right => tile.children
             }
             |> List.concat_map(local_missing_tokens_segment(~side));
           self @ children;
         }
       | _ => [],
     );

let request_of_relatives = ({siblings: (pre, suf), ancestors}: Relatives.t): option(request) => {
  let left =
    local_missing_tokens_segment(~side=Left, pre)
    @ tokens_of_shards(Ancestors.local_missing_shards(ancestors));
  let right = local_missing_tokens_segment(~side=Right, suf);
  switch (left, right) {
  | ([], []) => None
  | _ => Some({left, right})
  };
};

let request_is_empty = (request: request): bool =>
  switch (request.left, request.right) {
  | ([], []) => true
  | _ => false
  };

let request_changed = (a: request, b: request): bool =>
  compare(a.left, b.left) != 0 || compare(a.right, b.right) != 0;

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

let rec tokens_of_segment = (seg: Segment.t): list(Token.t) =>
  List.concat_map(tokens_of_piece, seg)

and tokens_of_piece = (piece: Piece.t): list(Token.t) =>
  switch (piece) {
  | Piece.Tile(tile) when List.length(tile.shards) == 1 && tile.children == [] =>
    tokens_of_shards([tile])
  | Piece.Tile(tile) => tokens_of_segment(Tile.disassemble(tile))
  | _ => []
  };

let consume_request =
    (~left_available: list(Token.t), ~right_available: list(Token.t), request: request)
    : request => {
  left: consume_tokens_in_order(request.left, right_available),
  right: consume_tokens_in_order(request.right, left_available),
};

let consume_request_by_generation =
    (request: request, ((ancestor, parent_sibs): Ancestors.generation)): request => {
  let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
  consume_request(
    ~left_available=tokens_of_segment(fst(parent_sibs) @ left_dis),
    ~right_available=tokens_of_segment(right_dis @ snd(parent_sibs)),
    request,
  );
};

let rec any_generation_consumes_request = (request: request, ancestors: Ancestors.t): bool =>
  switch (ancestors) {
  | [] => false
  | [generation, ...rest] =>
    let next = consume_request_by_generation(request, generation);
    request_changed(request, next) || any_generation_consumes_request(next, rest)
  };

/* Candidate Construction */

let shard_pieces = (tile: Tile.t): list(Piece.t) =>
  List.map(
    shard =>
      Piece.Tile({
        ...tile,
        shards: [shard],
        children: [],
      }),
    tile.shards,
  );

let rec has_incomplete_multi_deep = (seg: Segment.t): bool =>
  List.exists(
    fun
    | Piece.Tile(tile) =>
      (!Tile.is_complete(tile) && is_multidelimiter_label(tile.label))
      || List.exists(has_incomplete_multi_deep, tile.children)
    | _ => false,
    seg,
  );

let rec flatten_tiles_with_incomplete = (seg: Segment.t): Segment.t =>
  List.concat_map(
    fun
    | Piece.Tile(tile)
        when
          Tile.is_complete(tile)
          && is_multidelimiter_label(tile.label)
          && List.exists(has_incomplete_multi_deep, tile.children) =>
      Aba.mk(
        shard_pieces(tile),
        List.map(flatten_tiles_with_incomplete, tile.children),
      )
      |> Aba.join(piece => [piece], Fun.id)
      |> List.flatten
    | Piece.Tile(tile) => [
        Piece.Tile({
          ...tile,
          children: List.map(flatten_tiles_with_incomplete, tile.children),
        }),
      ]
    | piece => [piece],
    seg,
  );

let crack_siblings = ((pre, suf): Siblings.t): Siblings.t =>
  TupleUtil.map2(flatten_tiles_with_incomplete, (pre, suf));

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

/* Flatten ancestors into siblings until the current request has been
   satisfied. Irrelevant generations may still be traversed to reach an
   outer generation that can discharge the request, but once the request
   is empty the remaining outer context is preserved. */
let rec expand_scope =
    (
      request: request,
      siblings: Siblings.t,
      affected_rev: Ancestors.t,
      ancestors: Ancestors.t,
      fresh_map: Id.Map.t((Id.t, list(int))),
    ) =>
  switch (ancestors) {
  | [] => (siblings, List.rev(affected_rev), [], fresh_map)
  | _ when request_is_empty(request) =>
    (siblings, List.rev(affected_rev), ancestors, fresh_map)
  | [((ancestor, parent_sibs) as generation), ...rest] =>
    let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
    let (right_dis, fresh_map) =
      freshen_ancestor_shards(ancestor.id, fresh_map, right_dis);
    let siblings =
      Siblings.concat([siblings, (left_dis, right_dis), parent_sibs]);
    let request = consume_request_by_generation(request, generation);
    expand_scope(
      request,
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

let base_stats_of_summary = (summary: base_summary): repair_stats => {
  complete_multitiles: summary.complete_multitiles,
  incomplete_multitiles: summary.incomplete_multitiles,
  preserved_anchors: summary.complete_multitiles,
};

let rec collect_base_summary =
    (summary: base_summary, seg: Segment.t): base_summary =>
  List.fold_left(
    (summary, piece) =>
      switch (piece) {
      | Piece.Tile(tile) =>
        let summary =
          List.fold_left(collect_base_summary, summary, tile.children);
        if (!is_multidelimiter_label(tile.label)) {
          summary;
        } else if (Tile.is_complete(tile)) {
          {
            anchors: Id.Map.add(tile.id, (), summary.anchors),
            complete_multitiles: summary.complete_multitiles + 1,
            incomplete_multitiles: summary.incomplete_multitiles,
          };
        } else {
          {
            ...summary,
            incomplete_multitiles: summary.incomplete_multitiles + 1,
          };
        };
      | _ => summary
      },
    summary,
    seg,
  );

let score_multitile =
    (~anchors: Id.Map.t(unit), ~id: Id.t, ~label: Label.t, ~complete: bool)
    : repair_stats =>
  if (!is_multidelimiter_label(label)) {
    empty_repair_stats;
  } else {
    {
      complete_multitiles: complete ? 1 : 0,
      incomplete_multitiles: complete ? 0 : 1,
      preserved_anchors: complete && Id.Map.mem(id, anchors) ? 1 : 0,
    };
  };

let rec collect_candidate_stats =
    (~anchors: Id.Map.t(unit), stats: repair_stats, seg: Segment.t): repair_stats =>
  List.fold_left(
    (stats, piece) =>
      switch (piece) {
      | Piece.Tile(tile) =>
        let stats =
          add_repair_stats(
            stats,
            score_multitile(
              ~anchors,
              ~id=tile.id,
              ~label=tile.label,
              ~complete=Tile.is_complete(tile),
            ),
          );
        List.fold_left(
          (stats, child) => collect_candidate_stats(~anchors, stats, child),
          stats,
          tile.children,
        );
      | _ => stats
      },
    stats,
    seg,
  );

let should_accept_repair =
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
  let base_summary = collect_base_summary(empty_base_summary, Relatives.zip(base_scope));
  let base_stats = base_stats_of_summary(base_summary);
  let local_relatives =
    {
      Relatives.siblings: candidate_siblings,
      ancestors: [],
    }
    |> Relatives.reassemble;
  let candidate_stats =
    collect_candidate_stats(
      ~anchors=base_summary.anchors,
      empty_repair_stats,
      Relatives.zip(local_relatives),
    );
  if (should_accept_repair(base_stats, candidate_stats)) {
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
  switch (request_of_relatives(z.relatives)) {
  | None => z
  | Some(request) =>
    if (any_generation_consumes_request(request, z.relatives.ancestors)) {
      let (siblings, affected_ancestors, outer_ancestors, fresh_map) =
        expand_scope(
          request,
          z.relatives.siblings,
          [],
          z.relatives.ancestors,
          Id.Map.empty,
        );
      let siblings =
        crack_siblings(siblings)
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
      let cracked = crack_siblings(z.relatives.siblings);
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

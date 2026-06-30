open Util;

type t = ZipperBase.t;

/* Anchors (complete multi-delimiter forms) stay stable through incomplete
   edits — probe placement depends on that. Once an edit is delimiter-complete
   we realize the new structure instead of preserving stale history. */

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

let is_multidelimiter_label = (label: Label.t): bool =>
  List.length(label) > 1;

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
             if (!Tile.is_complete(tile)
                 && is_multidelimiter_label(tile.label)) {
               (
                 switch (side) {
                 | Left => Tile.right_missing_shards(tile)
                 | Right => Tile.left_missing_shards(tile)
                 }
               )
               |> tokens_of_shards;
             } else {
               [];
             };
           let children =
             (
               switch (side) {
               | Left => List.rev(tile.children)
               | Right => tile.children
               }
             )
             |> List.concat_map(local_missing_tokens_segment(~side));
           self @ children;
         }
       | _ => [],
     );

let request_of_relatives =
    ({siblings: (pre, suf), ancestors}: Relatives.t): option(request) => {
  let left =
    local_missing_tokens_segment(~side=Left, pre)
    @ tokens_of_shards(Ancestors.local_missing_shards(ancestors));
  let right = local_missing_tokens_segment(~side=Right, suf);
  switch (left, right) {
  | ([], []) => None
  | _ =>
    Some({
      left,
      right,
    })
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
      need == tok
        ? go(rest_need, rest_available) : go(obligations, rest_available)
    };
  go(obligations, available);
};

let rec tokens_of_segment = (seg: Segment.t): list(Token.t) =>
  List.concat_map(tokens_of_piece, seg)

and tokens_of_piece = (piece: Piece.t): list(Token.t) =>
  switch (piece) {
  | Piece.Tile(tile)
      when List.length(tile.shards) == 1 && tile.children == [] =>
    tokens_of_shards([tile])
  | Piece.Tile(tile) => tokens_of_segment(Tile.disassemble(tile))
  | _ => []
  };

let consume_request =
    (
      ~left_available: list(Token.t),
      ~right_available: list(Token.t),
      request: request,
    )
    : request => {
  left: consume_tokens_in_order(request.left, right_available),
  right: consume_tokens_in_order(request.right, left_available),
};

let consume_request_by_generation =
    (request: request, (ancestor, parent_sibs): Ancestors.generation)
    : request => {
  let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
  consume_request(
    ~left_available=tokens_of_segment(fst(parent_sibs) @ left_dis),
    ~right_available=tokens_of_segment(right_dis @ snd(parent_sibs)),
    request,
  );
};

let rec any_generation_consumes_request =
        (request: request, ancestors: Ancestors.t): bool =>
  switch (ancestors) {
  | [] => false
  | [generation, ...rest] =>
    let next = consume_request_by_generation(request, generation);
    request_changed(request, next)
    || any_generation_consumes_request(next, rest);
  };

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
      !Tile.is_complete(tile)
      && is_multidelimiter_label(tile.label)
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

/* Fresh ids for right-side ancestor shards: prevents a duplicate
   (id, shard_index) when rescan reassigns a new delimiter to the
   ancestor's id. Left-side shards keep their original ids. */
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

let flatten_generation =
    (
      (ancestor, parent_sibs): Ancestors.generation,
      siblings: Siblings.t,
      fresh_map: Id.Map.t((Id.t, list(int))),
    )
    : (Siblings.t, Id.Map.t((Id.t, list(int)))) => {
  let (left_dis, right_dis) = Ancestor.disassemble(ancestor);
  let (right_dis, fresh_map) =
    freshen_ancestor_shards(ancestor.id, fresh_map, right_dis);
  (
    Siblings.concat([siblings, (left_dis, right_dis), parent_sibs]),
    fresh_map,
  );
};

/* Flatten ancestor generations until the request is discharged; once it's
   empty, the remaining outer context is preserved. */
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
  | _ when request_is_empty(request) => (
      siblings,
      List.rev(affected_rev),
      ancestors,
      fresh_map,
    )
  | [generation, ...rest] =>
    let (siblings, fresh_map) =
      flatten_generation(generation, siblings, fresh_map);
    let request = consume_request_by_generation(request, generation);
    expand_scope(
      request,
      siblings,
      [generation, ...affected_rev],
      rest,
      fresh_map,
    );
  };

/* Restore a freshened shard's original id when rescan didn't actually take
   that (id, shard): it was only shadowed by rescan's LIFO, not displaced,
   so reassembly must regroup it with its siblings. */
let repair_fresh_ids =
    (fresh_map: Id.Map.t((Id.t, list(int))), siblings: Siblings.t)
    : Siblings.t =>
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

let crack_rescan_repair =
    (~fresh_map=Id.Map.empty, siblings: Siblings.t): Siblings.t =>
  crack_siblings(siblings) |> Siblings.rescan |> repair_fresh_ids(fresh_map);

let rec collect_multitiles = (seg: Segment.t): (list(Id.t), int) =>
  List.fold_left(
    ((complete_ids, incomplete), piece) =>
      switch (piece) {
      | Piece.Tile(tile) =>
        let (complete_ids, incomplete) =
          List.fold_left(
            ((complete_ids, incomplete), child) => {
              let (child_ids, child_incomplete) = collect_multitiles(child);
              (child_ids @ complete_ids, child_incomplete + incomplete);
            },
            (complete_ids, incomplete),
            tile.children,
          );
        if (!is_multidelimiter_label(tile.label)) {
          (complete_ids, incomplete);
        } else if (Tile.is_complete(tile)) {
          ([tile.id, ...complete_ids], incomplete);
        } else {
          (complete_ids, incomplete + 1);
        };
      | _ => (complete_ids, incomplete)
      },
    ([], 0),
    seg,
  );

let stats_of = (~anchors: Id.Map.t(unit), seg: Segment.t): repair_stats => {
  let (complete_ids, incomplete_multitiles) = collect_multitiles(seg);
  {
    complete_multitiles: List.length(complete_ids),
    incomplete_multitiles,
    preserved_anchors:
      List.length(List.filter(id => Id.Map.mem(id, anchors), complete_ids)),
  };
};

let should_accept_repair =
    (base_stats: repair_stats, candidate_stats: repair_stats): bool =>
  candidate_stats.complete_multitiles > base_stats.complete_multitiles
  || candidate_stats.complete_multitiles == base_stats.complete_multitiles
  && candidate_stats.preserved_anchors == base_stats.preserved_anchors
  && candidate_stats.incomplete_multitiles < base_stats.incomplete_multitiles;

let accept_candidate =
    (
      ~base_scope: Relatives.t,
      ~candidate_siblings: Siblings.t,
      ~outer_ancestors: Ancestors.t,
      z: t,
    )
    : t => {
  let (base_complete_ids, base_incomplete) =
    collect_multitiles(Relatives.zip(base_scope));
  /* base's complete multi-delimiter tiles are the anchors (all trivially preserved) */
  let anchors =
    List.fold_left(
      (m, id) => Id.Map.add(id, (), m),
      Id.Map.empty,
      base_complete_ids,
    );
  let base_stats = {
    complete_multitiles: List.length(base_complete_ids),
    incomplete_multitiles: base_incomplete,
    preserved_anchors: List.length(base_complete_ids),
  };
  let local_relatives =
    {
      Relatives.siblings: candidate_siblings,
      ancestors: [],
    }
    |> Relatives.reassemble;
  let candidate_stats = stats_of(~anchors, Relatives.zip(local_relatives));
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

let siblings_have_incomplete = ((pre, suf): Siblings.t): bool =>
  has_incomplete_multi_deep(pre) || has_incomplete_multi_deep(suf);

/* Incomplete multi-delim tile missing a LEADING shard (lone closer / dangling
   then/else/end/->): a cross-scope edit local L-to-R rescan can't fix. Does NOT
   fire for an ordinary unclosed opener, keeping mid-typing off the spine-flatten path. */
let rec has_orphaned_trailing_shard = (seg: Segment.t): bool =>
  List.exists(
    fun
    | Piece.Tile(tile) =>
      !Tile.is_complete(tile)
      && is_multidelimiter_label(tile.label)
      && Tile.left_missing_shards(tile) != []
      || List.exists(has_orphaned_trailing_shard, tile.children)
    | _ => false,
    seg,
  );

let siblings_have_orphaned_trailing_shard = ((pre, suf): Siblings.t): bool =>
  has_orphaned_trailing_shard(pre) || has_orphaned_trailing_shard(suf);

let flatten_to_depth =
    (
      ~fresh_map=Id.Map.empty,
      depth: int,
      siblings: Siblings.t,
      ancestors: Ancestors.t,
    )
    : (Siblings.t, Ancestors.t, Ancestors.t, Id.Map.t((Id.t, list(int)))) => {
  let rec go = (i, siblings, affected_rev, ancestors, fresh_map) =>
    if (i >= depth) {
      (siblings, List.rev(affected_rev), ancestors, fresh_map);
    } else {
      switch (ancestors) {
      | [] => (siblings, List.rev(affected_rev), [], fresh_map)
      | [generation, ...rest] =>
        let (siblings, fresh_map) =
          flatten_generation(generation, siblings, fresh_map);
        go(i + 1, siblings, [generation, ...affected_rev], rest, fresh_map);
      };
    };
  go(0, siblings, [], ancestors, fresh_map);
};

/* Fast path: caret-local request-driven reassociation. Returns z physically
   unchanged when there's no local request or no candidate improves the base. */
let go_request = (z: t): t =>
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
      let siblings = crack_rescan_repair(~fresh_map, siblings);
      /* still incomplete with unflattened outer ancestors: flatten them and
         retry (cascading reassociation when one layer reveals outer work). */
      let (siblings, affected_ancestors, outer_ancestors) =
        if (siblings_have_incomplete(siblings) && outer_ancestors != []) {
          let (siblings, more_affected, outer, more_fresh) =
            flatten_to_depth(
              ~fresh_map,
              List.length(outer_ancestors),
              siblings,
              outer_ancestors,
            );
          let siblings = crack_rescan_repair(~fresh_map=more_fresh, siblings);
          (siblings, affected_ancestors @ more_affected, outer);
        } else {
          (siblings, affected_ancestors, outer_ancestors);
        };
      let base_scope = {
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
        let base_scope = {
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

/* cap so a degenerate, never-matching orphan (e.g. a lone `)` with no opener)
   can't rescan the whole spine; real matches resolve within a few generations. */
let max_repair_depth = 32;

/* Fallback for incomplete forms outside the caret-local cone. Ascend one
   generation at a time and accept the first depth that strictly improves
   completeness, so cost tracks the local nesting that held the match, not the
   whole document; a non-matching orphan rejects at every depth. */
let flatten_and_repair = (z: t): t => {
  let limit = min(List.length(z.relatives.ancestors), max_repair_depth);
  let rec ascend = (depth: int): t =>
    if (depth > limit) {
      z;
    } else {
      let (flattened, affected_ancestors, outer_ancestors, fresh_map) =
        flatten_to_depth(depth, z.relatives.siblings, z.relatives.ancestors);
      let candidate_siblings = crack_rescan_repair(~fresh_map, flattened);
      let result =
        accept_candidate(
          ~base_scope={
            Relatives.siblings: z.relatives.siblings,
            ancestors: affected_ancestors,
          },
          ~candidate_siblings,
          ~outer_ancestors,
          z,
        );
      result !== z ? result : ascend(depth + 1);
    };
  ascend(1);
};

/* Request-driven path first; fall back to spine-flattening only when warranted.
   The guard keeps the hot per-keystroke path O(local): a single-char edit
   flattens only on an orphaned trailing shard, never an ordinary unclosed
   opener. ~thorough (Paste) admits a broader scan; should_accept_repair still
   rejects any flatten that doesn't strictly improve completeness. */
let go_with = (~thorough: bool, z: t): t => {
  let repaired = go_request(z);
  if (repaired !== z) {
    repaired;
  } else {
    let needs_repair =
      thorough
        ? has_incomplete_multi_deep(Relatives.zip(z.relatives))
        : siblings_have_orphaned_trailing_shard(z.relatives.siblings);
    needs_repair ? flatten_and_repair(z) : z;
  };
};

let go = (z: t): t => go_with(~thorough=false, z);

let go_thorough = (z: t): t => go_with(~thorough=true, z);

open Util;
open OptUtil.Syntax;
include ZipperBase;

let init: unit => t =
  () => {
    selection: Selection.mk([]),
    relatives: {
      siblings: (
        [],
        [
          Grout({
            id: Id.mk(),
            shape: Convex,
          }),
        ],
      ),
      ancestors: [],
    },
    caret: Outer,
    refractors: Refractor.init,
  };

let next_blank = _ => Id.mk();

let delete_parent = (z: t): t => {
  ...z,
  relatives: Relatives.delete_parent(z.relatives),
};

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let unzip = (~direction: Direction.t=Right, seg: Segment.t): t => {
  selection: Selection.mk([]),
  relatives: {
    siblings:
      switch (direction) {
      | Right => (seg, [])
      | Left => ([], seg)
      },
    ancestors: [],
  },
  caret: Outer,
  refractors: Refractor.init,
};

let regrout = (d: Direction.t, z: t): t => {
  assert(Selection.is_empty(z.selection));
  let relatives = Relatives.regrout(d, z.relatives);
  {
    ...z,
    relatives,
  };
};

let remold = (z: t): t => {
  assert(Selection.is_empty(z.selection));
  {
    ...z,
    relatives: Relatives.remold(z.relatives),
  };
};

let remold_regrout = (d: Direction.t, z: t): t => z |> remold |> regrout(d);

/* Rescan siblings for label-based shard conversion, then
 * reassemble + remold + regrout. This handles the case where
 * a standalone monotile should retroactively become a shard
 * of an incomplete tile (e.g. standalone `->` matching `fun`).
 * Should be called after edits, not during cursor movement. */
let rescan_reassemble = (d: Direction.t, z: t): t => {
  let siblings = Siblings.rescan(z.relatives.siblings);
  if (siblings == z.relatives.siblings) {
    z;
  } else {
    let relatives =
      {
        ...z.relatives,
        siblings,
      }
      |> Relatives.reassemble
      |> Relatives.remold
      |> Relatives.regrout(d);
    {
      ...z,
      relatives,
    };
  };
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

module TokenKey = {
  type t = Token.t;
  let compare = compare;
};

module TokenSet = Set.Make(TokenKey);

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

let target_shards_of_relatives = ({siblings: (pre, suf), ancestors}: Relatives.t)
    : list(Tile.t) =>
  deep_local_missing_shards_segment(~side=Left, pre)
  @ deep_local_missing_shards_segment(~side=Right, suf)
  @ Ancestors.local_missing_shards(ancestors);

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

let deep_reassociate = (z: t): t => {
  /* Early exit: only repair when the local repair cone has concrete
     unresolved shard demand. This includes trapped descendant demand
     inside the adjacent sibling trees, which is what powers the
     out-of-order paren/bracket wrap cases. */
  let target_shards = target_shards_of_relatives(z.relatives);
  if (target_shards == []) {
    z;
  } else {
    let target_labels = labels_of_shards(target_shards);
    let target_tokens = token_set_of_shards(target_shards);
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
        (
          flatten_tiles_with_target_demand(
            ~side=Left,
            ~target_tokens,
            fst(siblings),
          ),
          flatten_tiles_with_target_demand(
            ~side=Right,
            ~target_tokens,
            snd(siblings),
          ),
        )
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
      let (pre, suf) = z.relatives.siblings;
      let cracked =
        (
          flatten_tiles_with_target_demand(~side=Left, ~target_tokens, pre),
          flatten_tiles_with_target_demand(~side=Right, ~target_tokens, suf),
        );
      if (cracked == (pre, suf)) {
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
    };
  };
};

let clear_unparsed_buffer = (z: t) =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {
      ...z,
      selection: Selection.empty,
    }
  | _ => z
  };

let unselect = (~erase_buffer=false, z: t): t => {
  /* NOTE(andrew): Erase buffer flag only applies to unparsed buffer,
   * that is, the buffer style that just contains a single flat token.
   * Erasing a buffer that contains arbitrary tiles would be more complex
   * as we can't just empty the selection without regrouting */
  let z = erase_buffer ? clear_unparsed_buffer(z) : z;
  let relatives =
    z.relatives
    |> Relatives.prepend(z.selection.focus, z.selection.content)
    |> Relatives.reassemble;
  let selection = Selection.empty;
  {
    ...z,
    selection,
    relatives,
  };
};

let destroy_selection: t => t =
  z =>
    unselect({
      ...z,
      selection: Selection.empty,
    });

let unselect_and_zip = (~erase_buffer=false, z: t): Segment.t =>
  z |> unselect(~erase_buffer) |> zip;

let replace_selection = (focus, segment, z: t): t => {
  ...z,
  selection: Selection.mk(~focus, segment),
};

let grow_selection = (z: t): option(t) => {
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
  let selection = Selection.push(p, z.selection);
  {
    ...z,
    selection,
    relatives,
  };
};

// toggles focus and grows if selection is empty
let shrink_selection = (z: t): option(t) => {
  switch (Selection.pop(z.selection)) {
  | None =>
    let selection = Selection.toggle_focus(z.selection);
    grow_selection({
      ...z,
      selection,
    });
  | Some((p, selection)) =>
    let relatives =
      z.relatives
      |> Relatives.push(selection.focus, p)
      |> Relatives.reassemble;
    Some({
      ...z,
      selection,
      relatives,
    });
  };
};

let toggle_focus = (z: t): t => {
  ...z,
  selection: Selection.toggle_focus(z.selection),
};

let set_focus = (z: t, d: Direction.t): t => {
  let selection = {
    ...z.selection,
    focus: d,
  };
  {
    ...z,
    selection,
  };
};

let directional_unselect = (d: Direction.t, z: t): t => {
  let selection = {
    ...z.selection,
    focus: Direction.toggle(d),
  };
  unselect({
    ...z,
    selection,
  });
};

let unselect = (z: t): t =>
  z.selection.content == [] ? z : directional_unselect(z.selection.focus, z);

let move = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    let+ (p, relatives) = Relatives.pop(d, z.relatives);
    let relatives =
      relatives
      |> Relatives.push(Direction.toggle(d), p)
      |> Relatives.reassemble;
    {
      ...z,
      relatives,
    };
  } else {
    Some(directional_unselect(d, z));
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

/* As opposed to the Siblings.neighbor functions, which simply returns
 * the adjacent piece (if any) in the focal segment, this function is a
 * more general notion of 'the token to the left/right' of the cursor'.
 * It agrees with Sibling.neighbor whenever you are in the middle of
 * the focal segment; it returns None only if you are at the start/end
 * of the entire program, and if you are at an extreme of the focal
 * segment it returns the ADJACENT SHARD of the containing parent.
 * Note that this last case necessarily returns an incomplete tile and
 * thus does not retain knowledge of the tile's in-situ completeness */
let generalized_neighbor = (d: Direction.t, z: t): option(Piece.t) => {
  let* z = select(d, unselect(z));
  switch (z.selection.content) {
  | [p] => Some(p)
  | _ => None
  };
};

type neighbors = (option(Piece.t), option(Piece.t));

let generalized_neighbors = (z: t): neighbors => (
  generalized_neighbor(Left, z),
  generalized_neighbor(Right, z),
);

let neighbor_token = (d: Direction.t, z: t): option(Token.t) => {
  let* p = generalized_neighbor(d, z);
  Piece.token_of(p);
};

let neighbor_tokens = (z: t): (option(Token.t), option(Token.t)) => (
  neighbor_token(Left, z),
  neighbor_token(Right, z),
);

/* Iterative version to avoid stack overflow on large programs */
let do_until_piece =
    (action: t => option(t), p_n: neighbors => bool, z: t): option(t) => {
  let current = ref(action(z));
  let result = ref(None);
  let done_ = ref(false);
  while (! done_^) {
    switch (current^) {
    | None =>
      result := None;
      done_ := true;
    | Some(z) =>
      if (p_n(Siblings.neighbors(z.relatives.siblings))) {
        result := Some(z);
        done_ := true;
      } else {
        current := action(z);
      }
    };
  };
  result^;
};

/* Do `action` until the predicate on the generalized neigbors of the
   caret becomes true. A generalized neighbor is the neighboring piece, unless
   the neighbor is a polytile, in which case it's the relevant shard, or
   we are at the edge of a segment, in which case it's the relevant shard
   of the parent. The None case strictly means the beginning/end of the program.
   If no such piece is found, don't move. Does not check predicate before
   moving; caller should handle that case if necessary.

   NOTE: This is implemented iteratively to avoid stack overflow on large
   programs. The previous recursive implementation would overflow when
   traversing documents with thousands of tokens. */
let do_until =
    (action: t => option(t), p_n: neighbors => bool, z: t): option(t) => {
  let current = ref(action(z));
  let result = ref(None);
  let done_ = ref(false);
  while (! done_^) {
    switch (current^) {
    | None =>
      result := None;
      done_ := true;
    | Some(z) =>
      if (p_n(generalized_neighbors(z))) {
        result := Some(z);
        done_ := true;
      } else {
        current := action(z);
      }
    };
  };
  result^;
};

let do_to_extreme = (action: t => option(t), z: t): t =>
  do_until(
    action,
    (neighbors: neighbors) =>
      switch (neighbors) {
      | (None, _) => true
      | (_, None) => true
      | _ => false
      },
    z,
  )
  |> Option.value(~default=z);

let linebreak_on = (d: Direction.t, neighbors: neighbors): bool =>
  switch (neighbors) {
  | (_, Some(Secondary(s))) when d == Right && Secondary.is_linebreak(s) =>
    true
  | (_, None) when d == Right => true
  | (Some(Secondary(s)), _) when d == Left && Secondary.is_linebreak(s) =>
    true
  | (None, _) when d == Left => true
  | _ => false
  };

let do_until_linebreak =
    (f: t => option(t), d: Direction.t, z: t): option(t) =>
  linebreak_on(d, generalized_neighbors(z))
    ? Some(z) : do_until(f, linebreak_on(d), z);

let local_backpack = (z: t): list(Tile.t) =>
  Relatives.local_missing_shards(z.relatives);

let backpack_hd = (z: t): option(Tile.t) =>
  z |> local_backpack |> ListUtil.hd_opt;

let backpack_find = (tok: Token.t, z: t): option(Tile.t) =>
  if (Form.is_ambiguous_polymorph(tok)) {
    /* Special case for ambiguous polymorphs. These tokens
       occur both on their own as infix ops and as delimiters of
       multi-delimiter forms. To give the singleton form a chance, we
       only match these to incomplete tiles to form their multi forms
       when they're on the top of the stack */
    backpack_hd(z) |> Option.map(Tile.effective_label) == Some([tok])
      ? backpack_hd(z) : None;
  } else {
    List.find_map(
      t => Tile.effective_label(t) == [tok] ? Some(t) : None,
      local_backpack(z),
    );
  };

let insert_segment = (z: t, seg: Segment.t): t =>
  z |> replace_selection(Right, seg) |> unselect |> remold_regrout(Right);

let adj_pos = (d: Direction.t, z: t): t =>
  switch (d) {
  | Left => z
  | Right =>
    switch (move(Left, z)) {
    | None => z
    | Some(z) => z
    }
  };

let put_down_core = (seg: Segment.t, z: t): t =>
  z |> replace_selection(Right, seg) |> unselect;

let put_down_seg = (d: Direction.t, seg: Segment.t, z: t): t =>
  z |> put_down_core(seg) |> adj_pos(d);

let can_put_down = z =>
  switch (local_backpack(z)) {
  | [] => false
  | _ => z.caret == Outer
  };

let put_down_target = (d: Direction.t, target: Tile.t, z: t): t =>
  z |> put_down_core([Tile(target)]) |> remold_regrout(Left) |> adj_pos(d);

let put_down = (z: t): option(t) =>
  z.caret == Outer
    ? {
      let+ target = backpack_hd(z);
      put_down_target(Left, target, z);
    }
    : None;

let delete = (d: Direction.t, z: t): option(t) =>
  z |> select(d) |> Option.map(destroy_selection);

let adjacent_monotile_id = (d: Direction.t, z: t): option(Id.t) =>
  switch (Siblings.neighbors(z.relatives.siblings)) {
  | (Some(Tile({id, label: [_], _})), _) when d == Left => Some(id)
  | (_, Some(Tile({id, label: [_], _}))) when d == Right => Some(id)
  | _ => None
  };

let adjacent_monotile_or_new_id = (d, z) =>
  switch (adjacent_monotile_id(d, z)) {
  | Some(id) => id
  | None => Id.mk()
  };

let representative_piece = (z: t): option((Piece.t, Direction.t)) => {
  /* The piece to the left of the caret, or if none exists, the piece to the right */
  switch (Siblings.neighbors(sibs_with_sel(z))) {
  | (Some(l), _) => Some((l, Left))
  | (_, Some(r)) => Some((r, Right))
  | _ => None
  };
};

let base_point = (measured: Measured.t, z: t): Point.t => {
  switch (representative_piece(z)) {
  | Some((p, d)) =>
    let seg = Piece.disassemble(p);
    switch (d) {
    | Left =>
      let p = ListUtil.last(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.last;
    | Right =>
      let p = List.hd(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.origin;
    };
  | None => {
      row: 0,
      col: 0,
    }
  };
};

module Caret = {
  /* String shards can span multiple columns because emoji render wider than
     ASCII.  Translate an inner caret index into measured columns by consulting
     the token width table. */
  let string_offset = (token: Token.t, idx: int): int =>
    1 + Token.string_prefix_columns(token, idx);

  /* Determine how many columns to advance for an Inner caret.  Prefer the
     token on the left; if none exists fall back to the token on the right.
     Non-strings retain the classic one-column-per-character behaviour. */
  let inner_offset = (idx: int, z: t): int =>
    switch (neighbor_token(Left, z)) {
    | Some(token) when Token.is_string(token) => string_offset(token, idx)
    | _ =>
      switch (neighbor_token(Right, z)) {
      | Some(token) when Token.is_string(token) => string_offset(token, idx)
      | _ => idx + 1
      }
    };

  let offset = (z: t): int =>
    switch (z.caret) {
    | Outer => 0
    | Inner(idx) => inner_offset(idx, z)
    };

  let set = (caret: caret, z: t): t => {
    ...z,
    caret,
  };

  /* Max internal index of the shard the caret is adjacent to */
  let nhbr_max_idx = (d: Direction.t, z: t): option(int) => {
    let* t =
      switch (d, neighbor_tokens(z)) {
      | (Left, (Some(t), _)) => Some(t)
      | (Right, (_, Some(t))) => Some(t)
      | _ => None
      };
    let max_idx = Token.length(t) - 2;
    max_idx < 0 ? None : Some(max_idx);
  };

  /* Returns the delimiter index that the caret is adjacent to.
   * For non-tiles and monotiles this is always zero */
  let delim_idx = (z: t) =>
    switch (snd(z.relatives.siblings), z.relatives.ancestors) {
    | ([], [({shards: (l, _), _}, _), ..._]) => List.length(l)
    | _ => 0
    };

  /* Direction the caret is facing in */
  let direction = (z: t): option(Direction.t) =>
    switch (z.caret) {
    | Inner(_) => None
    | Outer =>
      switch (Siblings.neighbors(sibs_with_sel(z))) {
      | (Some(l), Some(r))
          when
            Piece.is_secondary(l)
            && Piece.is_secondary(r)
            && Selection.is_empty(z.selection) =>
        None
      | _ => Siblings.direction_between(sibs_with_sel(z))
      }
    };

  /* Grid position of the caret */
  /* Convert a caret to a concrete grid point for rendering and hit testing. */
  let point = (measured: Measured.t, z: t): Point.t => {
    let Point.{row, col} = base_point(measured, z);
    {
      row,
      col: col + offset(z),
    };
  };

  type t = ZipperBase.caret;
};

let do_towards_point =
    (
      ~anchor: option(Measured.Point.t)=?,
      ~measured: Measured.t,
      ~force_progress: bool=false,
      f: (Direction.t, t) => option(t),
      goal: Measured.Point.t,
      z: t,
    )
    : option(t) => {
  let caret_point = Caret.point(measured);

  let is_at_side_of_row = (d: Direction.t, z: t) => {
    let Point.{row, col} = caret_point(z);
    switch (move(d, z)) {
    | None => true
    | Some(z) =>
      let Point.{row: rowp, col: colp} = caret_point(z);
      row != rowp || col == colp;
    };
  };

  let direction_to_from = (p1: Point.t, p2: Point.t): Direction.t => {
    let before_row = p1.row < p2.row;
    let at_row = p1.row == p2.row;
    let before_col = p1.col < p2.col;
    before_row || at_row && before_col ? Left : Right;
  };

  let closer_to_prev = (curr, prev, goal: Point.t) =>
    /* Default to true if equal */
    abs(caret_point(prev).col - goal.col)
    < abs(caret_point(curr).col - goal.col);

  let init = caret_point(z);
  let d_to_goal = direction_to_from(goal, init);
  let rec go = (prev: t, curr: t) => {
    let curr_p = caret_point(curr);
    let x_progress = Point.dcomp(d_to_goal, curr_p.col, goal.col);
    let y_progress = Point.dcomp(d_to_goal, curr_p.row, goal.row);
    switch (y_progress, x_progress) {
    /* If we're not there yet, keep going */
    | (Under, Over | Exact | Under)
    | (Exact, Under) =>
      switch (f(d_to_goal, curr)) {
      | Some(next) => go(curr, next)
      | None => curr /* Should only occur at start/end of program */
      }
    /* If we're there, stop */
    | (Exact, Exact) => curr
    /* If we've overshot, meaning the exact goal is inaccessible,
     * we choose between current and previous (undershot) positions */
    | (Over, Over | Exact | Under) =>
      switch (force_progress) {
      /* Ideally we would use the same logic as from the below
       * anchor case here; however that results in strange
       * behavior when accidentally starting a drag at the end
       * of a line, which triggers the (invisible) selection of
       * a linebreak, making it appear that the caret has jumped
       * to the next line. The downside of leaving this as-is is
       * that multiline tokens (projectors) do not become part of
       * the selection when dragging until you're all the way
       * over them, which is slightly visually jarring */
      | false => prev
      /* Up/down kb movement works by setting a goal one row
       * below the current. When adjacent to a multiline token,
       * the nearest next caret position may be multiple lines down.
       * We must allow this overshoot in order to make progress. */
      | true => caret_point(prev) == init ? curr : prev
      }
    | (Exact, Over) =>
      switch (anchor) {
      | None =>
        /* If you're trying to (eg) move down at the end of a row
         * but the first position of the next row is further right
         * than the currentrow's end, we want to make progress
         * regardless of whether the new position would be closer
         * or further from the goal.  Otherwise, we try to just
         * get as close as we can  */
        is_at_side_of_row(Direction.toggle(d_to_goal), curr)
          ? curr : closer_to_prev(curr, prev, goal) ? prev : curr
      | Some(anchor) =>
        /* If we're dragging to make a selection, decide whether or
         * not to force progress based on the relative position of the
         * anchor (the position where the drag was started) */
        direction_to_from(goal, anchor) == d_to_goal ? curr : prev
      }
    };
  };
  let res = go(z, z);
  Measured.Point.equals(caret_point(res), caret_point(z))
    ? None : Some(res);
};

let selection_anchor_point = (measured, z: t): option(Point.t) => {
  switch (z.selection) {
  | {content: [], _} => None
  | {content, focus: Right, _} =>
    Some(
      Measured.find_p(
        ~msg="selection_anchor_point",
        List.hd(content),
        measured,
      ).
        origin,
    )
  | {content, focus: Left, _} =>
    Some(
      Measured.find_p(
        ~msg="selection_anchor_point",
        ListUtil.last(content),
        measured,
      ).
        last,
    )
  };
};

let serialize = (z: t): string => {
  sexp_of_t(z) |> Sexplib.Sexp.to_string;
};

let to_sexp = (z: t): Sexplib.Sexp.t => sexp_of_t(z);

let deserialize = (data: string): t => {
  Sexplib.Sexp.of_string(data) |> t_of_sexp;
};

let set_buffer = (z: t, ~mode: Selection.buffer, ~content: Segment.t): t => {
  ...z,
  selection: Selection.mk_buffer(mode, content),
};

let is_linebreak_to_right_of_caret =
    ({relatives: {siblings: (_, r), _}, _}: t): bool => {
  switch (r) {
  | [Secondary(s), ..._] when Secondary.is_linebreak(s) => true
  | _ => false
  };
};

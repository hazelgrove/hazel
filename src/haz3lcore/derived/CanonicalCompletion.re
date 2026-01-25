/* CanonicalCompletion: Complete incomplete syntax to enable term creation
 *
 * Algorithm:
 * 1. Use blank-line heuristic to find insertion points
 * 2. Collect trailing shards from all incomplete tiles (inner first, outer last)
 * 3. Insert shards at the insertion points
 * 4. Regrout the whole segment
 * 5. Reassemble to combine same-ID shards into complete tiles
 *
 * Performance note: The syntax cache tracks global_missing_shards (cached_backpack).
 * If cached_backpack is empty, we can skip completion entirely since there are
 * no incomplete tiles. This check should be done at the call site (e.g., MakeTerm)
 * before invoking completion.
 */

open Util;

/* Record of which shards were originally present in an incomplete tile */
[@deriving (show({with_path: false}), sexp, yojson)]
type shard_record = {
  tile_id: Id.t,
  original_shards: list(int),
};

/* Result of completing a segment */
[@deriving (show({with_path: false}), sexp, yojson)]
type completion_result = {
  completed_seg: Segment.t,
  shard_records: list(shard_record),
};

/* Get trailing missing shard indices for a tile */
let trailing_shards = (t: Tile.t): list(int) =>
  Tile.right_missing_shards(t) |> List.map(s => Tile.r_shard(s));

/* Collect trailing shards from incomplete tiles in a segment.
 * Returns shards in order: innermost first, outermost last. */
let collect_trailing_shards = (seg: Segment.t): list(Piece.t) => {
  let incomplete = Segment.incomplete_tiles(seg);
  /* incomplete_tiles returns left-to-right order, which is outer-to-inner.
   * We want inner-first, so reverse. */
  let inner_first = List.rev(incomplete);
  inner_first
  |> List.map((t: Tile.t) =>
       trailing_shards(t)
       |> List.map(idx => Piece.Tile(Tile.shard_of(t, idx)))
     )
  |> List.concat;
};

/* Single-pass partitioning at blank lines (double linebreaks) after incomplete tiles.
 * Returns list of subsegments. Split only occurs at blank lines that follow
 * at least one incomplete tile. O(n) instead of O(n²) recursive scanning. */
let partition_at_blank_lines = (seg: Segment.t): list(Segment.t) => {
  let rec go =
          (seg: Segment.t, acc: Segment.t, incomplete_before: bool)
          : list(Segment.t) => {
    switch (seg) {
    | [] =>
      /* End of segment - return accumulated subsegment */
      [List.rev(acc)]
    | [Secondary(w1) as p, Secondary(w2), ...rest]
        when Secondary.is_linebreak(w1) && Secondary.is_linebreak(w2) =>
      let incomplete_before = incomplete_before || !Piece.is_complete(p);
      if (incomplete_before) {
        /* Split here: finish current subsegment, start new one */
        let current = List.rev([Piece.Secondary(w1), ...acc]);
        let remaining = go(rest, [Secondary(w2)], false);
        [current, ...remaining];
      } else {
        /* No split - continue accumulating */
        go(
          rest,
          [Secondary(w2), Secondary(w1), ...acc],
          incomplete_before,
        );
      };
    | [p, ...rest] =>
      go(rest, [p, ...acc], incomplete_before || !Piece.is_complete(p))
    };
  };
  go(seg, [], false);
};

/* Get first split point only (for Indentation.re compatibility).
 * Returns None if no split, Some((before, after)) otherwise. */
let incomplete_subseg_before_blank_line =
    (seg: Segment.t): option((Segment.t, Segment.t)) => {
  switch (partition_at_blank_lines(seg)) {
  | [] => None
  | [_single] => None /* No split occurred */
  | [first, ...rest] => Some((first, List.concat(rest)))
  };
};

/* Insert shards at the end of each subsegment, then concatenate.
 * Single O(n) pass for partitioning, then O(n) for shard collection. */
let insert_shards_at_splits = (seg: Segment.t): Segment.t => {
  partition_at_blank_lines(seg)
  |> List.map(subseg => subseg @ collect_trailing_shards(subseg))
  |> List.concat;
};

let complete_segment = (sort: Sort.t, seg: Segment.t): completion_result => {
  /* Collect shard records before modification */
  let incomplete = Segment.incomplete_tiles(seg);
  let shard_records =
    List.map(
      (t: Tile.t) =>
        {
          tile_id: t.id,
          original_shards: t.shards,
        },
      incomplete,
    );

  if (List.length(incomplete) == 0) {
    {
      /* No changes needed */
      completed_seg: seg,
      shard_records,
    };
  } else {
    /* Phase 1: Insert all shards at appropriate split points */
    let seg_with_shards = insert_shards_at_splits(seg);

    /* Phase 2: Regrout once to fix shape inconsistencies */
    let regrouted =
      seg_with_shards
      |> Segment.regrout((Nib.Shape.concave(), Nib.Shape.concave()), _);

    /* Phase 3: Reassemble to combine same-ID shards; remold in case sort changed */
    let completed_seg =
      Segment.reassemble(regrouted) |> Segment.remold(_, sort);

    {
      completed_seg,
      shard_records,
    };
  };
};

/* Complete a segment recursively (descends into tile children) */
let rec complete_segment_deep = (~sort, seg: Segment.t): completion_result => {
  /* First, complete children of all tiles using their expected sorts */
  let seg_with_completed_children =
    List.map(
      fun
      | Piece.Tile(t) => {
          /* Get each child paired with its expected sort from the mold */
          let completed_children =
            Tile.sorted_children(t)
            |> List.map(((child_sort, child)) => {
                 let result = complete_segment_deep(~sort=child_sort, child);
                 result.completed_seg;
               });
          Piece.Tile({
            ...t,
            children: completed_children,
          });
        }
      | p => p,
      seg,
    );
  complete_segment(sort, seg_with_completed_children);
};

/* === Integration Points === */

let for_make_term = (seg: Segment.t): (Segment.t, list(shard_record)) => {
  let result = complete_segment_deep(~sort=Sort.Exp, seg);
  (result.completed_seg, result.shard_records);
};

let for_editor = (seg: Segment.t): completion_result => {
  complete_segment_deep(~sort=Sort.Exp, seg);
};

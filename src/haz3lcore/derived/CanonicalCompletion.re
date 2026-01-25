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
let trailing_shards = (t: Tile.t): list(Piece.t) =>
  Tile.right_missing_shards(t)
  |> List.map(s => Piece.Tile(Tile.shard_of(t, Tile.r_shard(s))));

/* Create shard pieces from incomplete tiles.
 * Takes tiles in left-to-right order, returns shards inner-first (reversed). */
let shards_from_incomplete = (incomplete: list(Tile.t)): list(Piece.t) =>
  List.rev(incomplete) |> List.concat_map(trailing_shards);

/* Single-pass partitioning at blank lines (double linebreaks) after incomplete tiles.
 * Returns list of (subsegment, incomplete_tiles_in_subsegment).
 * Split only occurs at blank lines that follow at least one incomplete tile.
 * Incomplete tiles are collected during the scan - no separate pass needed. */
let partition_at_blank_lines =
    (seg: Segment.t): list((Segment.t, list(Tile.t))) => {
  let rec go =
          (
            seg: Segment.t,
            acc: Segment.t,
            incomplete_acc: list(Tile.t),
            incomplete_before: bool,
          )
          : list((Segment.t, list(Tile.t))) => {
    switch (seg) {
    | [] =>
      /* End of segment - return accumulated subsegment with its incomplete tiles */
      [(List.rev(acc), List.rev(incomplete_acc))]
    | [Secondary(w1), Secondary(w2), ...rest]
        when Secondary.is_linebreak(w1) && Secondary.is_linebreak(w2) =>
      if (incomplete_before) {
        /* Split here: finish current subsegment, start new one */
        let current = List.rev([Piece.Secondary(w1), ...acc]);
        let current_incomplete = List.rev(incomplete_acc);
        let remaining = go(rest, [Secondary(w2)], [], false);
        [(current, current_incomplete), ...remaining];
      } else {
        /* No split - continue accumulating */
        go(
          rest,
          [Secondary(w2), Secondary(w1), ...acc],
          incomplete_acc,
          false,
        );
      }
    | [Piece.Tile(t) as p, ...rest] when !Tile.is_complete(t) =>
      /* Incomplete tile - add to both accumulators */
      go(rest, [p, ...acc], [t, ...incomplete_acc], true)
    | [p, ...rest] =>
      go(rest, [p, ...acc], incomplete_acc, incomplete_before)
    };
  };
  go(seg, [], [], false);
};

let complete_segment = (sort: Sort.t, seg: Segment.t): completion_result => {
  /* Single pass: partition at blank lines AND collect incomplete tiles */
  let partitioned = partition_at_blank_lines(seg);

  /* Extract all incomplete tiles for shard_records */
  let all_incomplete = List.concat_map(snd, partitioned);
  let shard_records =
    List.map(
      (t: Tile.t) =>
        {
          tile_id: t.id,
          original_shards: t.shards,
        },
      all_incomplete,
    );

  if (List.length(all_incomplete) == 0) {
    {
      /* No changes needed */
      completed_seg: seg,
      shard_records,
    };
  } else {
    /* Phase 1: Insert shards at end of each subsegment using pre-collected tiles */
    let seg_with_shards =
      partitioned
      |> List.concat_map(((subseg, incomplete)) =>
           subseg @ shards_from_incomplete(incomplete)
         );

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

/* CanonicalCompletion: Complete incomplete syntax to enable term creation
 *
 * Algorithm:
 * 1. Use blank-line heuristic to find insertion point (same as Indentation.re)
 * 2. Collect trailing shards from all incomplete tiles (inner first, outer last)
 * 3. Insert shards at the insertion point
 * 4. Regrout the whole segment
 * 5. Reassemble to combine same-ID shards into complete tiles
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

/* Find the shortest prefix of the segment containing all incomplete tiles
 * followed by two consecutive linebreaks (aka a blank line)  */
let incomplete_subseg_before_blank_line =
    (seg: Segment.t): option((Segment.t, Segment.t)) => {
  let rec find_split_point =
          (seg: Segment.t, acc: Segment.t, incomplete_before: bool)
          : option((Segment.t, Segment.t)) => {
    switch (seg) {
    | [] => None
    | [Secondary(w1) as p, Secondary(w2), ...rest]
        when Secondary.is_linebreak(w1) && Secondary.is_linebreak(w2) =>
      let incomplete_before = incomplete_before || !Piece.is_complete(p);
      if (incomplete_before) {
        /* Note: Leaves one linebreak in and one out (empty line) */
        Some((
          List.rev([Piece.Secondary(w1), ...acc]),
          [Secondary(w2), ...rest],
        ));
      } else {
        find_split_point(
          rest,
          [Secondary(w2), Secondary(w1), ...acc],
          incomplete_before,
        );
      };
    | [p, ...rest] =>
      find_split_point(
        rest,
        [p, ...acc],
        incomplete_before || !Piece.is_complete(p),
      )
    };
  };
  find_split_point(seg, [], false);
};

/* Recursively insert shards at blank-line split points.
 * This only handles shard insertion - regrout/reassemble happen once after. */
let rec insert_shards_at_splits = (seg: Segment.t): Segment.t => {
  switch (incomplete_subseg_before_blank_line(seg)) {
  | None =>
    /* No blank line split point - insert shards at end */
    let shards = collect_trailing_shards(seg);
    seg @ shards;
  | Some((before, after)) =>
    /* Insert shards for 'before', then recursively handle 'after' */
    let shards = collect_trailing_shards(before);
    let after_with_shards = insert_shards_at_splits(after);
    before @ shards @ after_with_shards;
  };
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

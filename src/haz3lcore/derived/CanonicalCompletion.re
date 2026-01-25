/* CanonicalCompletion: Complete incomplete syntax to enable term creation
 *
 * Partition heuristics (to determine where to insert missing delimiters):
 * 1. BLANK LINE: Two consecutive linebreaks always partition
 * 2. RELATIVE INDENT: Content at same-or-lesser indent than incomplete tile partitions
 *
 * Algorithm:
 * 1. Partition segment based on heuristics above
 * 2. Collect trailing shards from all incomplete tiles (inner first, outer last)
 * 3. Insert shards at end of each partition
 * 4. Regrout the whole segment to fix shape inconsistencies
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

/* Check if a piece is a space (not linebreak, just horizontal whitespace) */
let is_space_piece = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_space(s)
  | _ => false
  };

/* Count leading space pieces in a segment */
let count_leading_spaces = (seg: Segment.t): int => {
  let rec count = (seg, n) =>
    switch (seg) {
    | [Piece.Secondary(s), ...rest] when Secondary.is_space(s) =>
      count(rest, n + 1)
    | _ => n
    };
  count(seg, 0);
};

/* Single-pass partitioning based on indentation heuristics.
 * Returns list of (subsegment, incomplete_tiles_in_subsegment).
 *
 * Partition heuristics (when incomplete_before is true):
 * 1. BLANK LINE: Two consecutive linebreaks (always enabled)
 * 2. RELATIVE INDENT: After a linebreak, if the content's indentation is
 *    less than or equal to the incomplete tile's indentation, partition.
 *    (only when ~use_indent_heuristic=true)
 *
 * The relative indent heuristic interprets same-or-lesser indented content
 * after incomplete syntax as user intent to start something new.
 * This subsumes the old "zero indent" heuristic (incomplete at col 0,
 * content at col 0 means 0 <= 0 -> partition).
 *
 * This should be disabled for indentation calculation to avoid circular
 * dependency (indentation uses completion, completion uses indentation). */
let partition_segment =
    (~use_indent_heuristic=true, seg: Segment.t)
    : list((Segment.t, list(Tile.t))) => {
  let rec go =
          (
            seg: Segment.t,
            acc: Segment.t,
            incomplete_acc: list(Tile.t),
            incomplete_before: bool,
            line_indent: int, /* spaces since last linebreak */
            past_indent: bool, /* have we seen non-space on this line? */
            incomplete_indent: option(int),
          ) /* indent of first incomplete tile */
          : list((Segment.t, list(Tile.t))) => {
    switch (seg) {
    | [] =>
      /* End of segment - return accumulated subsegment with its incomplete tiles */
      [(List.rev(acc), List.rev(incomplete_acc))]

    /* Heuristic 1: Blank line (two consecutive linebreaks) */
    | [Secondary(w1), Secondary(w2), ...rest]
        when Secondary.is_linebreak(w1) && Secondary.is_linebreak(w2) =>
      if (incomplete_before) {
        /* Split here: finish current subsegment, start new one */
        let current = List.rev([Piece.Secondary(w1), ...acc]);
        let current_incomplete = List.rev(incomplete_acc);
        let remaining =
          go(rest, [Secondary(w2)], [], false, 0, false, None);
        [(current, current_incomplete), ...remaining];
      } else {
        /* No split - continue accumulating */
        go(
          rest,
          [Secondary(w2), Secondary(w1), ...acc],
          incomplete_acc,
          false,
          0,
          false,
          incomplete_indent,
        );
      }

    /* Heuristic 2: Relative indent comparison */
    | [Secondary(w), ...rest]
        when use_indent_heuristic && Secondary.is_linebreak(w) =>
      let spaces_after = count_leading_spaces(rest);
      switch (incomplete_indent) {
      | Some(inc_ind) when incomplete_before && spaces_after <= inc_ind =>
        /* Partition: content at same/lesser indent than incomplete tile */
        let current = List.rev(acc);
        let current_incomplete = List.rev(incomplete_acc);
        let remaining = go(rest, [Secondary(w)], [], false, 0, false, None);
        [(current, current_incomplete), ...remaining];
      | _ =>
        /* No partition - continue accumulating */
        go(
          rest,
          [Secondary(w), ...acc],
          incomplete_acc,
          incomplete_before,
          0,
          false,
          incomplete_indent,
        )
      };

    /* Space at start of line - increment indent */
    | [Secondary(s) as p, ...rest] when Secondary.is_space(s) && !past_indent =>
      go(
        rest,
        [p, ...acc],
        incomplete_acc,
        incomplete_before,
        line_indent + 1,
        false,
        incomplete_indent,
      )

    /* Space after content - doesn't affect indent tracking */
    | [Secondary(_) as p, ...rest] =>
      go(
        rest,
        [p, ...acc],
        incomplete_acc,
        incomplete_before,
        line_indent,
        past_indent,
        incomplete_indent,
      )

    /* Incomplete tile - record its indent level */
    | [Piece.Tile(t) as p, ...rest] when !Tile.is_complete(t) =>
      let new_incomplete_indent =
        switch (incomplete_indent) {
        | None => Some(line_indent)
        | some => some
        };
      go(
        rest,
        [p, ...acc],
        [t, ...incomplete_acc],
        true,
        line_indent,
        true,
        new_incomplete_indent,
      );

    /* Other pieces (complete tiles, grout, projectors) */
    | [p, ...rest] =>
      go(
        rest,
        [p, ...acc],
        incomplete_acc,
        incomplete_before,
        line_indent,
        true,
        incomplete_indent,
      )
    };
  };
  go(seg, [], [], false, 0, false, None);
};

let complete_segment =
    (~use_indent_heuristic=true, sort: Sort.t, seg: Segment.t)
    : completion_result => {
  /* Single pass: partition AND collect incomplete tiles */
  let partitioned = partition_segment(~use_indent_heuristic, seg);

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
    /* Phase 1: Insert shards at end of each subsegment */
    let seg_with_shards =
      partitioned
      |> List.concat_map(((subseg, incomplete)) =>
           subseg @ shards_from_incomplete(incomplete)
         );

    /* Phase 2: Regrout to fix shape inconsistencies */
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
let rec complete_segment_deep =
        (~use_indent_heuristic=true, ~sort, seg: Segment.t): completion_result => {
  /* First, complete children of all tiles using their expected sorts */
  let seg_with_completed_children =
    List.map(
      fun
      | Piece.Tile(t) => {
          /* Get each child paired with its expected sort from the mold */
          let completed_children =
            Tile.sorted_children(t)
            |> List.map(((child_sort, child)) => {
                 let result =
                   complete_segment_deep(
                     ~use_indent_heuristic,
                     ~sort=child_sort,
                     child,
                   );
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
  complete_segment(~use_indent_heuristic, sort, seg_with_completed_children);
};

/* === Integration Points === */

let for_make_term = (seg: Segment.t): (Segment.t, list(shard_record)) => {
  let result = complete_segment_deep(~sort=Sort.Exp, seg);
  (result.completed_seg, result.shard_records);
};

let for_editor = (seg: Segment.t): completion_result => {
  complete_segment_deep(~sort=Sort.Exp, seg);
};

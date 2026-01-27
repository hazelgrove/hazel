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

/* A single delimiter to be inserted, with hole info */
[@deriving (show({with_path: false}), sexp, yojson)]
type delimiter_info = {
  text: string, /* The delimiter token (e.g., "in", "->", ")") */
  needs_hole: bool, /* Whether a "?" follows this delimiter */
};

/* Information about a single insertion point for visualization.
 * Positions are looked up later using the adjacent piece ID. */
[@deriving (show({with_path: false}), sexp, yojson)]
type insertion = {
  adjacent_id: Id.t, /* ID of piece adjacent to insertion point */
  side: Direction.t, /* Which side of the adjacent piece (Left or Right) */
  delimiters: list(delimiter_info), /* The delimiter tokens with hole info */
};

/* Result of completing a segment */
[@deriving (show({with_path: false}), sexp, yojson)]
type completion_result = {
  completed_seg: Segment.t,
  shard_records: list(shard_record),
  insertions: list(insertion), /* For visualization: where and what to insert */
};

/* Get trailing missing shard indices for a tile */
let trailing_shards = (t: Tile.t): list(Piece.t) =>
  Tile.right_missing_shards(t)
  |> List.map(s => Piece.Tile(Tile.shard_of(t, Tile.r_shard(s))));

/* Create shard pieces from incomplete tiles.
 * Takes tiles in left-to-right order, returns shards inner-first (reversed). */
let shards_from_incomplete = (incomplete: list(Tile.t)): list(Piece.t) =>
  List.rev(incomplete) |> List.concat_map(trailing_shards);

/* Check if a shard needs a hole after it (has concave right side).
 *
 * Delimiters with concave right expect something after them:
 *   - `in`   : concave right (expects body expression)
 *   - `->`   : concave right (expects function body)
 *   - `then` : concave right (expects consequent)
 *   - `else` : concave right (expects alternative)
 *
 * Delimiters with convex right are self-terminating:
 *   - `)`    : convex right
 *   - `]`    : convex right
 *   - `end`  : convex right
 *
 * Note: When multiple delimiters are inserted at the same position,
 * later delimiters cannot fill holes from earlier ones. This is because
 * all trailing/closing delimiters have CONCAVE LEFT (they receive what
 * came before them in the tile structure):
 *   - `in`   : concave left (accepts the definition)
 *   - `->`   : concave left (accepts the pattern)
 *   - `)`    : concave left (accepts inner expression)
 *   - `else` : concave left (accepts the "then" branch)
 *   - `end`  : concave left (accepts case arms)
 *
 * So for `let f = fun x` → `-> ? in ?`, the `in` cannot fill the hole
 * after `->` because `in` has concave left, not convex left. */
let shard_needs_hole = (t: Tile.t, shard_idx: int): bool => {
  let (_, right_nib) = Mold.nibs(~index=shard_idx, t.mold);
  switch (right_nib.shape) {
  | Concave(_) => true
  | Convex => false
  };
};

/* Get delimiter info for missing shards of incomplete tiles.
 * For visualization: shows what text will be inserted and whether holes needed.
 * Takes tiles inner-first (reversed from left-to-right order). */
let delimiters_from_incomplete = (incomplete: list(Tile.t)): list(delimiter_info) =>
  List.rev(incomplete)
  |> List.concat_map((t: Tile.t) => {
       let label = t.label;
       Tile.right_missing_shards(t)
       |> List.map((s: Tile.t) => {
            let shard_idx = List.hd(s.shards);
            {
              text: List.nth(label, shard_idx),
              needs_hole: shard_needs_hole(t, shard_idx),
            };
          });
     });

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

/* Find the last piece in a segment for insertion position.
 * For blank-line partitions, this will be the trailing linebreak.
 * For column-0 partitions, this will be the last content piece. */
let last_piece_for_insertion = (seg: Segment.t): option(Piece.t) =>
  ListUtil.last_opt(seg);

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
      insertions: [],
    };
  } else {
    /* Compute insertions: for each partition with incomplete tiles,
     * record the adjacent piece ID for later position lookup */
    let insertions =
      partitioned
      |> List.filter_map(((subseg, incomplete)) =>
           if (List.length(incomplete) == 0) {
             None;
           } else {
             /* Find the last piece in the subsegment.
              * Insertion happens on the RIGHT side of this piece.
              * For blank-line partitions, this is the trailing linebreak.
              * For column-0 partitions, this is the last content piece. */
             switch (last_piece_for_insertion(subseg)) {
             | None => None
             | Some(last_p) =>
               let delimiters = delimiters_from_incomplete(incomplete);
               Some({
                 adjacent_id: Piece.id(last_p),
                 side: Right,
                 delimiters,
               });
             };
           }
         );

    /* Phase 1: Insert shards at end of each subsegment */
    let seg_with_shards =
      partitioned
      |> List.concat_map(((subseg, incomplete)) =>
           subseg @ shards_from_incomplete(incomplete)
         );

    /* Phase 2: Regrout to make segment well-formed for reassemble */
    let regrouted =
      seg_with_shards
      |> Segment.regrout((Nib.Shape.concave(), Nib.Shape.concave()), _);

    /* Phase 3: Reassemble to combine same-ID shards; remold to get correct molds */
    let reassembled =
      Segment.reassemble(regrouted) |> Segment.remold(_, sort);

    /* Phase 4: Regrout again based on NEW molds (remold may have changed shapes) */
    let completed_seg =
      Segment.regrout(
        (Nib.Shape.concave(), Nib.Shape.concave()),
        reassembled,
      );

    {
      completed_seg,
      shard_records,
      insertions,
    };
  };
};

/* Complete a segment recursively (descends into tile children).
 * Collects insertions from all levels for visualization. */
let rec complete_segment_deep =
        (~use_indent_heuristic=true, ~sort, seg: Segment.t): completion_result => {
  /* Helper: complete all children of a tile, collecting insertions */
  let complete_tile_children = (t: Tile.t): (list(Segment.t), list(insertion)) => {
    Tile.sorted_children(t)
    |> List.fold_left(
         ((segs_acc, ins_acc), (child_sort, child)) => {
           let result =
             complete_segment_deep(
               ~use_indent_heuristic,
               ~sort=child_sort,
               child,
             );
           (segs_acc @ [result.completed_seg], ins_acc @ result.insertions);
         },
         ([], []),
       );
  };

  /* Complete children of all tiles, collecting insertions */
  let (seg_with_completed_children, child_insertions) =
    List.fold_left(
      ((seg_acc, ins_acc), piece) =>
        switch (piece) {
        | Piece.Tile(t) =>
          let (completed_children, tile_insertions) = complete_tile_children(t);
          let new_tile = Piece.Tile({...t, children: completed_children});
          (seg_acc @ [new_tile], ins_acc @ tile_insertions);
        | p => (seg_acc @ [p], ins_acc)
        },
      ([], []),
      seg,
    );

  /* Complete the segment at this level */
  let top_result =
    complete_segment(~use_indent_heuristic, sort, seg_with_completed_children);

  /* Merge child insertions with top-level insertions */
  {
    ...top_result,
    insertions: child_insertions @ top_result.insertions,
  };
};

/* === Integration Points === */

let for_make_term = (seg: Segment.t): (Segment.t, list(shard_record)) => {
  let result = complete_segment_deep(~sort=Sort.Exp, seg);
  (result.completed_seg, result.shard_records);
};

let for_editor = (seg: Segment.t): completion_result => {
  complete_segment_deep(~sort=Sort.Exp, seg);
};

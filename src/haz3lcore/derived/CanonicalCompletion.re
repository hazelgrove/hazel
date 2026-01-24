/* CanonicalCompletion: Complete incomplete syntax to enable term creation
 *
 * This module provides functions to complete segments containing incomplete
 * tiles (tiles with missing shards/delimiters) into syntactically complete
 * segments that can be converted to terms.
 *
 * The key insight is that during editing, users create incomplete forms like:
 *   - `let x = 1` (missing `in`)
 *   - `fun x` (missing `->`)
 *   - `(1 + 2` (missing `)`)
 *
 * For semantic analysis and round-tripping, we need to canonically complete
 * these forms while recording which shards were originally present.
 *
 * Design Goals:
 * 1. Cursor-independent: Unlike Dump.re, works on segments directly
 * 2. Deterministic: Same input always produces same output
 * 3. Reversible: Record enough info to reconstruct incomplete state
 * 4. Integrated: Can be called during MakeTerm traversal
 *
 * See plans/canonical-completion.md for full design rationale.
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

/* === Heuristic Configuration === */

/* Whether to stop completion at linebreaks (matching Dump.re behavior) */
let stop_at_linebreak = true;

/* Whether to stop at blank lines (two consecutive linebreaks, like Indentation.re) */
let stop_at_blank_line = true;

/* Create a single space Secondary piece */
let space = (): Piece.t => Secondary(Secondary.mk_space(Id.mk()));

/* === Helper Functions === */

/* Check if a piece is a linebreak */
let is_linebreak = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_linebreak(s)
  | _ => false
  };

/* Check if segment starts with a linebreak */
let starts_with_linebreak = (seg: Segment.t): bool =>
  switch (seg) {
  | [p, ..._] => is_linebreak(p)
  | [] => false
  };

/* Check if segment starts with two consecutive linebreaks (blank line) */
let starts_with_blank_line = (seg: Segment.t): bool =>
  switch (seg) {
  | [p1, p2, ..._] => is_linebreak(p1) && is_linebreak(p2)
  | _ => false
  };

/* Find the position to drop trailing shards.
 *
 * Heuristic (matching Dump.re):
 * - Go as far right as possible
 * - Stop at linebreak if stop_at_linebreak is true
 * - Stop at blank line if stop_at_blank_line is true
 *
 * Returns: (segment_before_drop, segment_after_drop)
 */
let find_drop_position = (seg: Segment.t): (Segment.t, Segment.t) => {
  let rec go = (before: Segment.t, after: Segment.t) =>
    switch (after) {
    | [] => (before, [])
    | [p, ...rest] =>
      /* Check stopping conditions */
      if (stop_at_blank_line && starts_with_blank_line(after)) {
        (before, after);
      } else if (stop_at_linebreak && is_linebreak(p)) {
        /* Include the linebreak in 'before', stop after it */
        (before @ [p], rest);
      } else {
        go(before @ [p], rest);
      }
    };
  go([], seg);
};

/* Create a completed version of a tile by filling in all shards.
 *
 * For a tile with label ["let", "=", "in"] and shards [0, 1],
 * this produces a tile with shards [0, 1, 2] and appropriate children.
 */
let complete_tile = (t: Tile.t, trailing_content: Segment.t): Tile.t => {
  let all_shard_indices = List.init(List.length(t.label), i => i);
  let missing_count = List.length(t.label) - List.length(t.shards);

  /* The existing children plus new empty children for missing shards */
  let new_children =
    if (missing_count > 0) {
      /* For trailing shards, the last child is the trailing content,
         and any intermediate missing shards get empty segments */
      let intermediate_empties = List.init(missing_count - 1, _ => []);
      t.children @ intermediate_empties @ [trailing_content];
    } else {
      t.children;
    };

  {
    ...t,
    shards: all_shard_indices,
    children: new_children,
  };
};

/* === Main Completion Functions === */

/* Complete a single segment (non-recursive, doesn't descend into tile children).
 *
 * This is the core workhorse function. It:
 * 1. Scans for incomplete tiles
 * 2. For each incomplete tile, determines where to drop its trailing shards
 * 3. Completes the tile and regrouts if necessary
 * 4. Records which shards were originally present
 *
 * Parameters:
 * - insert_separators: If true, add spaces where tokens would jam together
 */
let complete_segment =
    (~insert_separators: bool=false, seg: Segment.t): completion_result => {
  let rec go =
          (acc: Segment.t, shard_records: list(shard_record), remaining: Segment.t)
          : completion_result =>
    switch (remaining) {
    | [] => {completed_seg: acc, shard_records}
    | [Piece.Tile(t), ...rest] when !Tile.is_complete(t) =>
      /* Found an incomplete tile - need to complete it */
      let record = {tile_id: t.id, original_shards: t.shards};

      /* Find where to drop the trailing shards */
      let (content_before_drop, content_after_drop) = find_drop_position(rest);

      /* Complete the tile with the content before the drop point as its last child */
      let completed_tile = complete_tile(t, content_before_drop);

      /* TODO: May need to regrout here if shapes don't fit.
         For now, assume shapes work out. */

      /* Optionally add separator space before the completed tile's trailing content */
      let piece_to_add =
        if (insert_separators) {
          /* TODO: Be smarter about when separators are needed.
             For now, we're not actually inserting them in the tile itself. */
          Piece.Tile(completed_tile);
        } else {
          Piece.Tile(completed_tile);
        };

      /* Continue processing the rest */
      go(
        acc @ [piece_to_add],
        shard_records @ [record],
        content_after_drop,
      );
    | [p, ...rest] =>
      /* Not an incomplete tile, just pass through */
      go(acc @ [p], shard_records, rest)
    };

  go([], [], seg);
};

/* Complete a segment recursively (descends into tile children).
 *
 * This applies complete_segment at each level of the segment tree.
 *
 * Parameters:
 * - insert_separators: If true, add spaces where tokens would jam together
 */
let rec complete_segment_deep =
        (~insert_separators: bool=false, seg: Segment.t): completion_result => {
  /* First, complete children of all tiles */
  let seg_with_completed_children =
    seg
    |> List.map(p =>
         switch (p) {
         | Piece.Tile(t) =>
           let completed_children =
             t.children
             |> List.map(child => {
                  let result = complete_segment_deep(~insert_separators, child);
                  result.completed_seg;
                });
           Piece.Tile({...t, children: completed_children});
         | _ => p
         }
       );

  /* Then complete this level */
  /* Note: We're discarding child shard_records here for simplicity.
     A full implementation would aggregate them. */
  complete_segment(~insert_separators, seg_with_completed_children);
};

/* === Integration Points === */

/* For use during MakeTerm: complete a segment before parsing.
 *
 * Returns the completed segment and shard records to store in term annotations.
 * Uses insert_separators=false since MakeTerm doesn't need readable output.
 */
let for_make_term = (seg: Segment.t): (Segment.t, list(shard_record)) => {
  let result = complete_segment_deep(~insert_separators=false, seg);
  (result.completed_seg, result.shard_records);
};

/* For use in editor affordances (e.g., click-to-complete).
 *
 * Returns completed segment with separator spaces for readability.
 */
let for_editor = (seg: Segment.t): completion_result => {
  complete_segment_deep(~insert_separators=true, seg);
};

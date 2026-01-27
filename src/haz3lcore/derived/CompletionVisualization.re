/* CompletionVisualization: Generate text mockups showing canonical completion.
 *
 * Format:
 *   - Middle dot (·) marks insertion points inline
 *   - `// text` at end of line (4 spaces after content) shows what's inserted
 *
 * Example:
 *   let x = 1·    // in ?
 */

open Util;

/* Middle dot character for marking insertion points */
let dot = {js|·|js};

/* An insertion with its resolved position */
type positioned_insertion = {
  row: int,
  col: int,
  delimiters: list(CanonicalCompletion.delimiter_info),
};

/* Resolve an insertion's position by looking up adjacent_id in Measured */
let resolve_position =
    (measured: Measured.t, ins: CanonicalCompletion.insertion)
    : option(positioned_insertion) =>
  switch (Measured.find_by_id(ins.adjacent_id, measured)) {
  | None => None
  | Some(m) =>
    let (row, col) =
      switch (ins.side) {
      | Right => (m.last.row, m.last.col)
      | Left => (m.origin.row, m.origin.col)
      };
    Some({row, col, delimiters: ins.delimiters});
  };

/* Compute display text for delimiters with their holes.
 * skip_last_hole: if true, the last delimiter's hole is filled by following content. */
let format_delimiters =
    (~skip_last_hole=false, delimiters: list(CanonicalCompletion.delimiter_info))
    : string => {
  let n = List.length(delimiters);
  delimiters
  |> List.mapi((i, d: CanonicalCompletion.delimiter_info) => {
       let is_last = i == n - 1;
       let show_hole = d.needs_hole && !(is_last && skip_last_hole);
       if (show_hole) {
         d.text ++ " ?";
       } else {
         d.text;
       };
     })
  |> String.concat(" ");
};

/* Group positioned insertions by row */
let group_by_row =
    (insertions: list(positioned_insertion))
    : IntMap.t(list(positioned_insertion)) =>
  List.fold_left(
    (acc, ins) =>
      IntMap.update(
        ins.row,
        fun
        | None => Some([ins])
        | Some(existing) => Some([ins, ...existing]),
        acc,
      ),
    IntMap.empty,
    insertions,
  );

/* Generate the mockup string from a segment.
 * Shows dots at insertion points and offside comments with what's inserted. */
let mockup = (seg: Segment.t): string => {
  /* Get the original text representation.
   * Hide concave grout for cleaner visualization. */
  let original_text =
    Printer.of_segment(
      ~holes="?",
      ~concave_holes="",
      ~refractors=Id.Map.empty,
      seg,
    );

  /* Run completion to get insertion info */
  let result = CanonicalCompletion.for_editor(seg);
  let insertions = result.insertions;

  if (List.length(insertions) == 0) {
    /* No completions needed - return original */
    original_text;
  } else {
    /* Measure the original segment to look up positions from IDs */
    let measured =
      Measured.of_segment(seg, ProjectorCore.Shape.Map.empty, Id.Map.empty);

    /* Resolve positions for all insertions */
    let positioned = List.filter_map(resolve_position(measured), insertions);

    /* Compute has_following_content for each insertion position.
     * An insertion has following content if there's more content in the
     * segment after its position. We use partition info for this. */
    let partitioned = CanonicalCompletion.partition_segment(seg);
    let n_partitions = List.length(partitioned);

    /* Check if a segment has non-trivial content (not just whitespace) */
    let has_content = (seg: Segment.t): bool =>
      List.exists(
        fun
        | Piece.Secondary(_) => false
        | _ => true,
        seg,
      );

    /* Build a map from (row, col) to has_following_content */
    let following_content_map: IntMap.t(IntMap.t(bool)) =
      partitioned
      |> List.mapi((i, x) => (i, x))
      |> List.fold_left(
           (acc, (idx, (subseg, incomplete))) =>
             if (List.length(incomplete) == 0) {
               acc;
             } else {
               let following_partitions =
                 ListUtil.sublist((idx + 1, n_partitions), partitioned);
               let has_following =
                 List.exists(((seg, _)) => has_content(seg), following_partitions);

               switch (CanonicalCompletion.last_piece_for_insertion(subseg)) {
               | None => acc
               | Some(last_p) =>
                 switch (Measured.find_by_id(Piece.id(last_p), measured)) {
                 | None => acc
                 | Some(m) =>
                   IntMap.update(
                     m.last.row,
                     fun
                     | None => Some(IntMap.singleton(m.last.col, has_following))
                     | Some(col_map) =>
                       Some(IntMap.add(m.last.col, has_following, col_map)),
                     acc,
                   )
                 }
               };
             },
           IntMap.empty,
         );

    let by_row = group_by_row(positioned);

    /* Process each line */
    let lines = String.split_on_char('\n', original_text);
    let result_lines =
      List.mapi(
        (row_idx, line) => {
          switch (IntMap.find_opt(row_idx, by_row)) {
          | None => line
          | Some(row_insertions) =>
            /* Sort insertions by column (right to left for insertion) */
            let sorted =
              List.sort(
                (a, b) => Int.compare(b.col, a.col),
                row_insertions,
              );

            /* Insert dots at each position (right to left to preserve indices) */
            let line_with_dots =
              List.fold_left(
                (current_line, ins) => {
                  let grapheme_idx =
                    Token.column_to_grapheme_index(current_line, ins.col);
                  Token.insert_nth(grapheme_idx, dot, current_line);
                },
                line,
                sorted,
              );

            /* Get display texts for each insertion */
            let col_map =
              IntMap.find_opt(row_idx, following_content_map)
              |> Option.value(~default=IntMap.empty);
            let all_texts =
              sorted
              |> List.rev /* restore left-to-right order */
              |> List.map(ins => {
                   let has_following =
                     IntMap.find_opt(ins.col, col_map)
                     |> Option.value(~default=false);
                   format_delimiters(~skip_last_hole=has_following, ins.delimiters);
                 })
              |> String.concat(" ");

            /* Add offside comment: 4 spaces after content */
            line_with_dots ++ "    // " ++ all_texts;
          }
        },
        lines,
      );

    String.concat("\n", result_lines);
  };
};

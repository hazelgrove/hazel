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
    Some({
      row,
      col,
      delimiters: ins.delimiters,
    });
  };

/* Compute display text for delimiters with their holes */
let format_delimiters =
    (delimiters: list(CanonicalCompletion.delimiter_info)): string =>
  delimiters
  |> List.map((d: CanonicalCompletion.delimiter_info) => {
       let suffix = d.needs_hole ? " ?" : "";
       d.text ++ suffix;
     })
  |> String.concat(" ");

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
    Printer.of_segment(~holes="?", ~concave_holes="", ~refractors=[], seg);

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
            let all_texts =
              sorted
              |> List.rev  /* restore left-to-right order */
              |> List.map(ins => format_delimiters(ins.delimiters))
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

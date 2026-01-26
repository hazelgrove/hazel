/* DocSlideMigration.re
 *
 * Migration script for adding indentation to old doc slides.
 *
 * The old doc slides have segments with linebreaks but no indentation
 * spaces after them. This module migrates them to the new format
 * with proper indentation.
 */

/* Migrate a PersistentSegment.t to have proper indentation.
 *
 * Strategy:
 * 1. Try to restore the segment from the sexp (preferred - preserves IDs)
 * 2. Apply Format to fix indentation
 * 3. Re-persist with new segment sexp and backup_text
 */
let migrate = (persisted: PersistentSegment.t): PersistentSegment.t => {
  /* Step 1: Restore the segment from sexp */
  let original_seg =
    try(persisted.segment |> Sexplib.Sexp.of_string |> Segment.t_of_sexp) {
    | _ =>
      /* Fallback: parse from backup_text */
      switch (Parser.to_segment(persisted.backup_text)) {
      | Some(seg) => seg
      | None => Segment.empty
      }
    };

  /* Step 2: Apply Format to fix indentation */
  let formatted_seg = AutoFormat.segment(original_seg);

  /* Step 3: Create a zipper to persist (needed for refractors) */
  let zipper =
    formatted_seg
    |> Zipper.unzip(~direction=Left)
    |> Zipper.update_refractors(
         _,
         PersistentSegment.restore_refractors(persisted.refractors),
       );

  /* Step 4: Re-persist */
  PersistentSegment.persist(zipper);
};

/* Escape a string for use in an OCaml string literal.
 * Uses OCaml's line continuation syntax for multiline strings.
 *
 * OCaml's line continuation (backslash-newline) strips leading whitespace
 * from the continuation line. To preserve Hazel indentation, we must put
 * the content's indentation BEFORE the continuation character:
 *
 *   "line1\n  \
 *    line2"  ->  "line1\n  line2"  (spaces preserved!)
 *
 * vs the wrong way:
 *
 *   "line1\n\
 *      line2"  ->  "line1\nline2"  (spaces stripped!)
 */
let escape_for_ml = (ml_indent: string, s: string): string => {
  /* Escape special characters within a line (not newlines) */
  let escape_chars = line => {
    let escape_char = c =>
      switch (c) {
      | '\\' => "\\\\"
      | '"' => "\\\""
      | '\t' => "\\t"
      | '\r' => "\\r"
      | c => String.make(1, c)
      };
    String.to_seq(line)
    |> Seq.map(escape_char)
    |> List.of_seq
    |> String.concat("");
  };

  /* Split into lines */
  let lines = String.split_on_char('\n', s);

  /* Helper to count and strip leading spaces */
  let strip_leading_spaces = (s: string): (int, string) => {
    let rec count = (i, s) =>
      if (i >= String.length(s)) {
        i;
      } else if (s.[i] == ' ') {
        count(i + 1, s);
      } else {
        i;
      };
    let n = count(0, s);
    (n, String.sub(s, n, String.length(s) - n));
  };

  /* For each line, we need to output:
   * - The line content (escaped, without leading spaces which were handled earlier)
   * - If not the last line: \n followed by next line's leading spaces
   *   followed by continuation \<newline><ml_indent>
   *
   * The key insight: OCaml continuation strips whitespace AFTER the newline,
   * so we put Hazel's indentation BEFORE the continuation character.
   */
  let rec process_lines = (lines: list(string), is_first: bool): string =>
    switch (lines) {
    | [] => ""
    | [last] =>
      let (_, content) = is_first ? (0, last) : strip_leading_spaces(last);
      escape_chars(content);
    | [current, ...rest] =>
      let next = List.hd(rest);
      let (_, current_content) =
        is_first ? (0, current) : strip_leading_spaces(current);
      let (next_indent, _) = strip_leading_spaces(next);
      let hazel_indent = String.make(next_indent, ' ');
      /* Format: escaped_content + \n + hazel_indent + \<newline> + ml_indent */
      escape_chars(current_content)
      ++ "\\n"
      ++ hazel_indent
      ++ "\\\n"
      ++ ml_indent
      ++ process_lines(rest, false);
    };

  process_lines(lines, true);
};

/* Generate an ML file content for a migrated slide */
let generate_ml_content =
    (title: string, persisted: PersistentSegment.t): string => {
  /* Use different indentation for each field */
  let segment_indent = "         ";
  let backup_indent = "         ";
  let title_escaped = escape_for_ml("", title);
  let segment_escaped = escape_for_ml(segment_indent, persisted.segment);
  let backup_escaped = escape_for_ml(backup_indent, persisted.backup_text);
  let refractors_escaped = escape_for_ml("", persisted.refractors);

  Printf.sprintf(
    {|let out : string * Haz3lcore.PersistentSegment.t =
  ( "%s",
    {
      segment =
        "%s";
      backup_text =
        "%s";
      refractors = "%s";
    } )
|},
    title_escaped,
    segment_escaped,
    backup_escaped,
    refractors_escaped,
  );
};

/* Print diagnostic info about a migration */
let print_migration_diff =
    (
      title: string,
      original: PersistentSegment.t,
      migrated: PersistentSegment.t,
    )
    : unit => {
  print_endline("=== Migration: " ++ title ++ " ===");
  print_endline("");
  print_endline("--- Original backup_text (first 500 chars) ---");
  print_endline(
    String.sub(
      original.backup_text,
      0,
      min(500, String.length(original.backup_text)),
    ),
  );
  print_endline("");
  print_endline("--- Migrated backup_text (first 500 chars) ---");
  print_endline(
    String.sub(
      migrated.backup_text,
      0,
      min(500, String.length(migrated.backup_text)),
    ),
  );
  print_endline("");
};

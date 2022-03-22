/* Perform a structural diff on two `UHExp.t` (or `UHExp.line`,
   or `UHExp.opseq`, or `UHExp.operand`) and retrieve the root
   of the diff. In particular, we want to determine whether the root of
   the diff was a hole or lies in a (non-empty) hole in the earlier
   expression. If the root of a diff does not lie in a hole, the first
   parent hole will be returned instead, if such a parent exists.

   Note that when diffing lines, only diffs across a single line are
   considered. This should handle all cases except for the case in which
   an `ExpLine` that only has a hole is filled with multiple lines.

   See `FillAndResume.is_fill_viable` for the usage of this function.

   TODO: implement `diff_operand` with `case` expressions.

   TODO: implement the case when an `ExpLine` that only has a hole is
   filled with multiple lines.
   */

/* Diff result for when diffing two `UHExp.opseq` or `UHExp.operand`.

   `ONoDiff`: Opseqs are equal.
   `ODiff`: There is a difference. The first argument is the filled opseq
   (from the second opseq). The hole number is from then first opseq,
   if the replaced expression is a hole.
   */
[@deriving sexp]
type opseq_diff_result =
  | ONoDiff
  | ODiff(UHExp.opseq, option(MetaVar.t));

/* Diff result for when diffing two `UHExp.block` or `UHExp.line`.

   `BNoDiff`: Line(s) are equal
   `BNonFillDiff`:
   - Different number of lines
   - Comparison between `LetLine` and `ExpLine` (different types of lines)
   - Multiple lines have differences
   `BFillDiff`: Only a single line (`LetLine` or `ExpLine`) has a difference,
   and all lines are of the same type.
   */
[@deriving sexp]
type block_diff_result =
  | BNoDiff
  | BNonFillDiff
  | BFillDiff(UHExp.opseq, option(MetaVar.t));

/* Note: `diff_operand` doesn't simply take two `UHExp.operand`,
   because an operand may be `Skel.Placeholder`, which is not
   self-contained (requires information about the `Seq.t`).

   `diff_operand(placeholder1_index, seq1, placeholder2_index, seq2)`
   */
let diff_operand:
  (
    int,
    Seq.t(UHExp.operand, UHExp.operator),
    int,
    Seq.t(UHExp.operand, UHExp.operator)
  ) =>
  opseq_diff_result;

let diff_opseq: (UHExp.opseq, UHExp.opseq) => opseq_diff_result;

let diff_line: (UHExp.line, UHExp.line) => block_diff_result;

/* Check if a `UHExp.t` (or `UHExp.block`) has a valid diff.

   This means that there is exactly one line (`ExpLine` or `LetLine`)
   which has a difference (`ODiff`).

   If there are no lines that are different, then there is no diff (`BNoDiff`).

   If there are multiple lines that are different, if there are different line
   types being compared (ignoring empty or comment lines), or if there are a
   differing number of lines (also ignoring empty or comment lines), then there
   is no valid diff for a fill operation (`BNonFillDiff`).

   Note that empty and comment lines are ignored.

   TODO: handle the case when an `ExpLine` with only a single hole
   gets filled with multiple lines.
   */
let diff_block: (UHExp.t, UHExp.t) => block_diff_result;

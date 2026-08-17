open Util;

/* Placement of holes at shape-conflict boundaries (policy ported from
 * the virtual-grout branch, PR #2165 — architecture-independent
 * knowledge: the same rule there drove decorations; here it drives
 * where artifact grout sits in a secondary run, see GroutPlace).
 *
 * A conflict boundary between two tiles whose nib shapes disagree owns
 * the run of secondaries (spaces, linebreaks, comments) separating
 * them. `decide` picks where in that run the hole goes and in what
 * style, so the placement policy lives in one place and can be tested
 * textually (Test_GroutPlace).
 *
 * Policy: the hole sits one space from its anchor token — the token on
 * its left, or on its right for leading holes. Free room at a line end
 * is used only to avoid compressing that gap, never to drift further:
 * `1 + ` puts the hole one space after `+`, and `1 +    ` puts it in
 * the same place, with the rest of the run continuing past it. An
 * empty gap pinches the hole directly between the tokens. The hole
 * never passes a comment.
 *
 * (Ported from virtual-grout's decoration placement; re-expressed for
 * material insertion — the hole is a piece occupying its own cell, so
 * the replace-a-space-cell degradations are gone: the index always
 * means "insert before run[index]".)
 *
 * Across linebreaks: a gap that merely wraps onto the content's line
 * (single linebreak) keeps the hole at the end of its owner's line.
 * A gap containing a whole blank line — a prepared slot — puts the
 * hole on the first such line, at the end of its indentation. At the
 * top-level trailing edge every line after the first linebreak is
 * blank, so the same rule applies from the first linebreak on (`in ⏎`
 * and `in⏎` both put the hole on the next line, and extra linebreaks
 * below never drag it further down).
 */

[@deriving (show({with_path: false}), sexp)]
type style =
  | Thick /* box occupying the whitespace cell it is drawn before */
  | ThickHalf /* box shifted left half a cell, straddling a cell
               * boundary; not emitted by the current policy, kept for
               * caret-following placement experiments */
  | Thin; /* zero-width X pinched directly between tokens */

[@deriving (show({with_path: false}), sexp)]
type t = {
  index: int, /* draw before run[index]; may equal the run length */
  style,
};

let decide = (~at_boundary: bool, ~leading=false, run: list(Secondary.t)): t => {
  /* only the prefix before the first comment is placeable territory;
   * a comment counts as content on the right */
  let rec first_comment = (i, xs) =>
    switch (xs) {
    | [] => i
    | [x, ..._] when Secondary.is_comment(x) => i
    | [_, ...xs] => first_comment(i + 1, xs)
    };
  let k = first_comment(0, run);
  let (eff, _) = ListUtil.split_n(k, run);
  let has_comment = k < List.length(run);
  let has_lb = List.exists(Secondary.is_linebreak, eff);
  /* spaces on the run's first line (eff is spaces + linebreaks only) */
  let rec leading_spaces = (i, xs) =>
    switch (xs) {
    | [x, ...xs] when Secondary.is_space(x) => leading_spaces(i + 1, xs)
    | _ => i
    };
  let n = leading_spaces(0, eff);
  let lb_count = List.length(List.filter(Secondary.is_linebreak, eff));
  /* The hole goes on the first EMPTY line of the gap (a line with no
   * token on it), at the end of its indentation, and never further
   * down. Mid-segment, the left token occupies line one, so the first
   * empty line takes two linebreaks (the line after a single one
   * belongs to the content below) — except at the trailing edge,
   * where every line after the first linebreak is blank. At the
   * leading edge there is no left token, so line one itself is the
   * first empty line. */
  let has_slot =
    has_lb && (lb_count >= 2 || at_boundary && !has_comment || leading);
  if (has_slot) {
    if (leading) {
      {
        index: n,
        style: Thick,
      };
    } else {
      let rec first_lb = (i, xs) =>
        switch (xs) {
        | [x, ..._] when Secondary.is_linebreak(x) => i
        | [_, ...xs] => first_lb(i + 1, xs)
        | [] => i
        };
      let i1 = first_lb(0, eff);
      let (_, rest) = ListUtil.split_n(i1 + 1, eff);
      {
        index: i1 + 1 + leading_spaces(0, rest),
        style: Thick,
      };
    };
  } else if (has_lb) {
    {
      /* the gap merely wraps onto the content's line: hole stays on
       * the first line, one space after the left token */
      index: n >= 1 ? 1 : 0,
      style: Thick,
    };
  } else if (at_boundary && !has_comment) {
    if (n == 0) {
      {
        /* free space directly right of the last token */
        index: 0,
        style: Thick,
      };
    } else {
      {
        /* one space after the last token; the free edge means even a
         * single typed space survives next to the hole */
        index: 1,
        style: Thick,
      };
    };
  } else {
    /* single-line gap with content on both sides: EXACTLY ONE space
     * before the hole where the gap has one, the rest after (andrew
     * 2026-07-26 rule, gap-1 ordering corrected 2026-07-27). Rendered:
     * 1 space -> `let?=` (the hole paints into that space's cell, no
     * width added), 2 -> `let ?=`, 3 -> `let ? =`, 5 -> `let ?   =`.
     * An empty gap pinches the hole directly between the tokens.
     *
     * WHY, over centering (P10 FILL-POSITION AFFINITY + trajectory
     * weighting — see plans/obligation-display-design.md): typing a
     * space right after `let` must leave the hole AFTER the caret,
     * where the operand you are about to type will land, so the
     * operand replaces the hole rather than the hole vanishing from
     * behind the caret. GAP-1 ORDER (2026-07-27): the hole goes AFTER
     * the single space in PIECE ORDER too — `min(1, n - 1)` put it
     * before, which was this rule failing its own motivating example:
     * at `let ␣¦` the hole (and the oracle pad at its far junction)
     * landed BEFORE the caret, so the drawn caret jumped two columns
     * on one keypress (`let¦` -> `let ?¦`). The render is identical
     * either way at gap 1 (one cell); the order decides which side of
     * the hole the caret's boundary maps to. This also makes the hole
     * position fully stable: it never moves on any gap transition. */
    switch (n) {
    | 0 => {
        index: 0,
        style: Thin,
      }
    | n => {
        index: leading ? max(n - 2, 0) : min(1, n),
        style: Thick,
      }
    };
  };
};

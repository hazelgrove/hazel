/* Caret-shift scroll compensation.
 *
 * When something causes the editor's layout above the caret to grow
 * or shrink — currently: a refractor's drawer-mode `Tab(n)` height
 * changing because of a model toggle, or because new samples arrived
 * and the pretty-printed height recomputed — the caret's screen
 * position would otherwise jump as content reflows beneath it.
 *
 * `CachedSyntax.calculate` computes the exact row delta when its
 * `mk` runs (cheap: diff old vs new shape_map / refractor_shape_map
 * for entries above the caret), and stashes it here. `Main.re`'s
 * `after_display` consumes the delta and scrolls `#main` by
 * `rows * font_metrics.row_height` so the caret returns to its
 * previous screen Y.
 *
 * This module is intentionally narrow: it fires only when the shape
 * map actually changes, and only contains rows that strictly precede
 * the caret. No per-frame DOM measurements. */
let pending_rows: ref(int) = ref(0);

let add = (rows: int): unit => pending_rows := pending_rows^ + rows;

let consume = (): int => {
  let r = pending_rows^;
  pending_rows := 0;
  r;
};

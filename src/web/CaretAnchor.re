/* Reactive caret-anchor scroll compensation.
 *
 * Invariant: the text caret's screen-y should be stable when the
 * caret's *logical* position in the document is unchanged. Things
 * that can shift it without a logical move:
 *
 *   - Refractor drawer heights changing above the caret (new samples
 *     from the worker, sample-focus changes shifting Single-mode
 *     displayed samples, drawer-mode toggles elsewhere, etc.)
 *   - Probes being added/removed at positions other than the caret.
 *   - Any other layout-affecting change above the caret that the
 *     editor doesn't classify as a user-initiated caret move.
 *
 * Detection: structural signature of the caret = (left-neighbor piece
 * id, right-neighbor piece id, caret state). Piece ids are stable
 * across reflows, so the signature is invariant under drawer-height
 * changes but flips immediately when the user moves/edits the caret.
 *
 * Algorithm (W3C scroll-anchoring shaped, but for `#caret` as anchor):
 *
 *   1. After each render, read #caret's `getBoundingClientRect().top`
 *      and the new signature; stash both.
 *   2. On the next render: compute new signature, read new top.
 *      If signature unchanged and top differs, scroll #main by the
 *      delta to restore the caret's previous screen-y.
 *   3. If signature changed (logical caret moved), don't compensate;
 *      just refresh the baseline.
 *
 * Interaction with SampleAnchor (Left/Right sample focus): SampleAnchor
 * uses a different invariant (indicated sample's screen-y stable across
 * its action). When both could fire on the same frame, caret-anchor
 * runs first; SampleAnchor consumes after; then `refresh` resets the
 * caret baseline to the post-SampleAnchor-scroll position so the next
 * frame doesn't try to undo SampleAnchor's scroll.
 *
 * Float scrollTop arithmetic everywhere — int truncation drifts under
 * repeated rapid changes (autorepeat). */

open Js_of_ocaml;
open Haz3lcore;

type signature = (option(Id.t), option(Id.t), ZipperBase.caret);

let signature_of = (z: Zipper.t): signature => {
  let (l, r) = Siblings.neighbors(z.relatives.siblings);
  (Option.map(Piece.id, l), Option.map(Piece.id, r), z.caret);
};

let equal_sig = ((al, ar, ac): signature, (bl, br, bc): signature): bool =>
  al == bl && ar == br && ZipperBase.equal_caret(ac, bc);

let read_caret_top = (): option(float) => {
  let doc = Dom_html.document;
  Js.Opt.case(
    doc##getElementById(Js.string("caret")),
    () => None,
    el => Some(el##getBoundingClientRect##.top),
  );
};

let scroll_main_by = (dy: float): unit => {
  let doc = Dom_html.document;
  Js.Opt.iter(
    doc##getElementById(Js.string("main")),
    main => {
      let st: float = Js.Unsafe.get(main, Js.string("scrollTop"));
      Js.Unsafe.set(main, Js.string("scrollTop"), st +. dy);
    },
  );
};

/* Per-frame snapshot. `measured` is captured by physical identity:
 * CachedSyntax preserves the same `measured` reference when no rebuild
 * happens (the no-op branch of `calculate` returns `{...old, ...}`,
 * which leaves field refs intact). Comparing physical identity is
 * therefore a precise "did this frame's layout change" gate, and
 * suppresses compensation on idle re-renders, arrow-key scroll-into-
 * view, animation frames, sub-pixel layout drift from other code, etc. */
type state = {
  sig_: signature,
  y: float,
  measured: Measured.t,
};

let prev: ref(option(state)) = ref(None);

/* Compare, possibly scroll, update baseline. Three gates must all hold
 * for compensation to fire:
 *
 *   1. `measured` reference changed since last frame — layout actually
 *      rebuilt this frame (edit / refractor change / dynamics arrival).
 *   2. Caret signature unchanged — the caret didn't logically move; if
 *      it did, the user wanted it to.
 *   3. Caret screen-y differs — the rebuild actually moved the caret.
 *
 * Call BEFORE other scroll-affecting compensations so caret-anchor sees
 * its own delta cleanly. */
let update = (~measured: Measured.t, z: Zipper.t): unit => {
  let new_sig = signature_of(z);
  switch (prev^, read_caret_top()) {
  | (Some({sig_: old_sig, y: old_y, measured: old_measured}), Some(new_y))
      when
        old_measured !== measured
        && equal_sig(old_sig, new_sig)
        && new_y != old_y =>
    /* Caret moved by (new_y - old_y) on screen; scrolling #main by the
     * same amount shifts content (including the caret) back, restoring
     * its previous screen position. */
    scroll_main_by(new_y -. old_y);
    /* Re-read post-scroll for next frame's baseline. */
    prev :=
      read_caret_top()
      |> Option.map(y =>
           {
             sig_: new_sig,
             y,
             measured,
           }
         );
  | (_, Some(new_y)) =>
    prev :=
      Some({
        sig_: new_sig,
        y: new_y,
        measured,
      })
  | (_, None) => prev := None
  };
};

/* Update baseline only — no comparison, no scroll. Call AFTER other
 * compensations (e.g. SampleAnchor) on frames where those scrolled,
 * so the next `update` doesn't try to undo them. */
let refresh = (~measured: Measured.t, z: Zipper.t): unit => {
  let new_sig = signature_of(z);
  switch (read_caret_top()) {
  | Some(y) =>
    prev :=
      Some({
        sig_: new_sig,
        y,
        measured,
      })
  | None => prev := None
  };
};

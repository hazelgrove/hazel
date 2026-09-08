open Util;
open Base;

/* CHILD-SLOT BOUNDARY CONTEXT — the one home for "what material
 * flanks this position, crossing out of a child slot into the
 * enclosing tile's shards".
 *
 * A segment nested in a tile's child slot has no siblings past its
 * own edges, but it is NOT at a segment edge: the enclosing shards
 * are its real neighbours. Every consumer that walks a segment or a
 * caret and asks "what is on my left/right" must cross that boundary,
 * or it mistakes a slot edge for a document edge.
 *
 * That mistake has now been made three times independently:
 *   - the ghost-region detector (fixed 2026-07-24: a hole alone in a
 *     child slot saw no siblings, so a span lost its pads),
 *   - the caret facing rule (fixed 2026-07-26 via `bounds_at_caret`
 *     below: `let ?¦=` faced left because the enclosing `=` shard
 *     read as a segment edge),
 *   - avoided only in the pad walk, which threads the enclosing
 *     shards itself as `bound(k)`.
 * Hence P11 (see plans/obligation-display-design.md): a position's
 * neighbours are its flanking MATERIAL, never its flanking SIBLINGS.
 *
 * Two entry points, one concept: `slot_bounds` for segment walkers
 * that descend into children (they know the tile and child index),
 * `bounds_at_caret` for zipper consumers (they know the ancestors).
 * A None result means a genuine document edge, where a concave
 * boundary shape is the right reading. */

/* Shapes facing INTO child slot `k` of a tile: the right nib of the
 * shard bounding it on the left, the left nib of the shard bounding
 * it on the right. None on a side means that side has no shard (a
 * malformed/partial tile), not that it is a document edge. */
let slot_bounds =
    (~mold: Mold.t, ~shards: list(int), ~child: int)
    : (option(Nib.Shape.t), option(Nib.Shape.t)) => {
  let nib_of = (i: int, side: Direction.t) =>
    switch (Mold.nibs(~index=i, mold)) {
    | (l, r) => Some(side == Direction.Left ? l.shape : r.shape)
    | exception _ => None
    };
  let l =
    switch (List.nth_opt(shards, child)) {
    | Some(i) => nib_of(i, Direction.Right)
    | None => None
    };
  let r =
    switch (List.nth_opt(shards, child + 1)) {
    | Some(i) => nib_of(i, Direction.Left)
    | None => None
    };
  (l, r);
};

/* Shapes flanking the caret's own segment, crossing into the
 * innermost ancestor's shards. None on a side = genuine document
 * edge (no enclosing tile on that side). */
let ancestor_bounds =
    (ancestors: list((Ancestor.t, (segment, segment))))
    : (option(Nib.Shape.t), option(Nib.Shape.t)) =>
  switch (ancestors) {
  | [] => (None, None)
  | [(a, _), ..._] =>
    let (pre, suf) = a.shards;
    let l =
      switch (ListUtil.last_opt(pre)) {
      | Some(i) =>
        let (_, r) = Mold.nibs(~index=i, a.mold);
        Some(r.shape);
      | None => None
      };
    let r =
      switch (ListUtil.hd_opt(suf)) {
      | Some(i) =>
        let (l, _) = Mold.nibs(~index=i, a.mold);
        Some(l.shape);
      | None => None
      };
    (l, r);
  };

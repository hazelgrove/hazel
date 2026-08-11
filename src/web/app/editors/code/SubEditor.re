open Util;
open Haz3lcore;

/* A sub-editor is a windowed view onto one term of a host ("main")
 * editor: the splice. The splice's pieces ARE the main editor's pieces —
 * there is no sub-editor model. The zipper, statics, undo history and
 * persistence all belong to the main editor, so a sub-editor cannot
 * fall out of sync with the syntax it displays: edits made in either
 * view are edits to the same segment.
 *
 * Used by the proof-stepper views (e.g. `InductionStep`'s scrutinee and
 * `InductionCase`'s pattern) to display and edit slices of the
 * surrounding theorem syntax in place. See `CodeEditable.View.view`'s
 * `~sub_editor` parameter for the rendering / confinement half. */

/* Structural locator for a contiguous editable region inside a host
 * piece. Identified STRUCTURALLY — host piece id plus a path of child /
 * selector steps — rather than by the region's own term rep id: term
 * ids churn as the user types (a grout hole becomes a var, a var
 * becomes the child of a cons, ...) while the render-time view model
 * lags the (debounced) statics, so a term-id key goes stale mid-burst
 * and drops keystrokes. The host PIECE id is stable under any edit
 * inside its slots.
 *
 * Two flavors of anchor:
 *
 *   - a splice wrapper, whose content IS the region, so no path is
 *     needed (`of_splice`). This is the projector/language notion of an
 *     editable region;
 *   - an arbitrary host tile, where the call site supplies the grammar
 *     knowledge needed to carve a region out of its children:
 *
 *       let scrut =
 *         Target.child(~anchor=inductionId, 0)
 *         |> Target.until(Before(Target.nthTile(["|", "=>"], 0)));
 *
 *       let pattern = i =>
 *         Target.child(~anchor=inductionId, 0)
 *         |> Target.descend(Target.nthTile(["|", "=>"], i), ~child=0);
 */
module Target = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type selector =
    | /* The n-th (0-based) direct tile piece whose label matches. Nested
       * tiles inside children are ignored. */ NthTile(
        Label.t,
        int,
      );

  [@deriving (show({with_path: false}), sexp, yojson)]
  type bound =
    | Start
    | End
    | Before(selector)
    | /* Like Before, but the segment end when no piece matches — for
       * ranges bounded by a tile that may legitimately be absent (e.g.
       * an induction scrutinee when there are no case rules yet). */ BeforeOrEnd(
        selector,
      )
    | After(selector);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type step =
    | /* Enter the i-th mold child of the current tile. */ IntoChild(int) /* Among the current segment's direct pieces, select a tile and
     * enter its i-th mold child. */
    | IntoSelected(selector, int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    anchor: Id.t,
    steps: list(step),
    from: bound,
    until: bound,
  };

  let nthTile = (label: Label.t, n: int): selector => NthTile(label, n);

  /* The whole content of a splice wrapper: a splice already delimits an
   * editable region, so there is no path to walk and no host syntax
   * beside the region to protect. */
  let of_splice = (id: Id.t): t => {
    anchor: id,
    steps: [],
    from: Start,
    until: End,
  };

  /* Start at the host tile's i-th child, spanning the whole child. */
  let child = (~anchor: Id.t, i: int): t => {
    anchor,
    steps: [IntoChild(i)],
    from: Start,
    until: End,
  };

  /* Narrow the end of the contiguous range in the current segment. */
  let until = (until: bound, t: t): t => {
    ...t,
    until,
  };

  /* Narrow the start of the contiguous range in the current segment. */
  let from = (from: bound, t: t): t => {
    ...t,
    from,
  };

  /* Find a tile in the current segment via `selector`, then continue
   * locating from that tile's `~child`-th mold child (full span). */
  let descend = (selector: selector, ~child: int, t: t): t => {
    anchor: t.anchor,
    steps: t.steps @ [IntoSelected(selector, child)],
    from: Start,
    until: End,
  };

  let matches_selector = (sel: selector, p: Piece.t): bool =>
    switch (sel, p) {
    | (NthTile(label, _), Tile(tile)) => tile.label == label
    | (NthTile(_, _), _) => false
    };

  /* Index of the n-th direct piece matching `sel`, or None. */
  let find_selector_index = (sel: selector, seg: Segment.t): option(int) => {
    let NthTile(_, n) = sel;
    let rec go = (i, seen, rest) =>
      switch (rest) {
      | [] => None
      | [p, ...ps] =>
        if (matches_selector(sel, p)) {
          seen == n ? Some(i) : go(i + 1, seen + 1, ps);
        } else {
          go(i + 1, seen, ps);
        }
      };
    go(0, 0, seg);
  };

  let find_selected_tile = (sel: selector, seg: Segment.t): option(Base.tile) =>
    switch (find_selector_index(sel, seg)) {
    | Some(i) =>
      switch (List.nth_opt(seg, i)) {
      | Some(Piece.Tile(tile)) => Some(tile)
      | _ => None
      }
    | None => None
    };

  /* Inclusive start / exclusive end index for a range bound. */
  let bound_index = (bound: bound, seg: Segment.t): option(int) =>
    switch (bound) {
    | Start => Some(0)
    | End => Some(List.length(seg))
    | Before(sel) => find_selector_index(sel, seg)
    | BeforeOrEnd(sel) =>
      Some(
        find_selector_index(sel, seg)
        |> Option.value(~default=List.length(seg)),
      )
    | After(sel) =>
      switch (find_selector_index(sel, seg)) {
      | Some(i) => Some(i + 1)
      | None => None
      }
    };

  let slice =
      (seg: Segment.t, ~from: bound, ~until: bound): option(Segment.t) => {
    open OptUtil.Syntax;
    let* start = bound_index(from, seg);
    let* stop = bound_index(until, seg);
    start <= stop && start >= 0 && stop <= List.length(seg)
      ? Some(ListUtil.sublist((start, stop), seg)) : None;
  };

  /* Some(id) when the target denotes a whole wrapper's content, i.e. it
   * was built by `of_splice`. Lets `mk` reuse the frame the syntax cache
   * already computed for that splice instead of re-resolving. */
  let whole_content_id = (t: t): option(Id.t) =>
    switch (t.steps, t.from, t.until) {
    | ([], Start, End) => Some(t.anchor)
    | _ => None
    };

  type cursor =
    | AtTile(Base.tile)
    | AtSegment(Segment.t);

  /* Find the anchor piece anywhere in the segment tree, recursing
   * through tile children, splice contents and projector syntax. A tile
   * anchor starts the path AT the tile (so a child step is required to
   * reach a segment); a splice anchor starts it INSIDE the wrapper's
   * content, which is what makes a bare `of_splice` target resolve to
   * the whole splice. */
  let rec find_anchor = (id: Id.t, seg: Segment.t): option(cursor) =>
    List.fold_left(
      (found, p: Piece.t) =>
        switch (found) {
        | Some(_) => found
        | None =>
          switch (p) {
          | Tile(tile) =>
            tile.id == id
              ? Some(AtTile(tile))
              : List.fold_left(
                  (found, child) =>
                    found == None ? find_anchor(id, child) : found,
                  None,
                  tile.children,
                )
          | Splice(s) =>
            s.id == id
              ? Some(AtSegment(s.content)) : find_anchor(id, s.content)
          | Projector(pr) => find_anchor(id, pr.syntax)
          | Grout(_)
          | Secondary(_) => None
          }
        },
      None,
      seg,
    );

  let apply_step = (cur: cursor, step: step): option(cursor) => {
    OptUtil.Syntax.(
      switch (cur, step) {
      | (AtTile(tile), IntoChild(i)) =>
        let+ child = List.nth_opt(tile.children, i);
        AtSegment(child);
      | (AtSegment(seg), IntoSelected(sel, i)) =>
        let* tile = find_selected_tile(sel, seg);
        let+ child = List.nth_opt(tile.children, i);
        AtSegment(child);
      | (AtSegment(_), IntoChild(_))
      | (AtTile(_), IntoSelected(_)) => None
      }
    );
  };

  /* Resolve `target` against a live segment tree. None when the anchor,
   * any step, or the final range cannot be found — callers treat this
   * as a transient mid-rewrite state and degrade to read-only. */
  let resolve = (target: t, root: Segment.t): option(Segment.t) => {
    open OptUtil.Syntax;
    let* anchor = find_anchor(target.anchor, root);
    let* cur =
      List.fold_left(
        (cur, step) => {
          let* cur = cur;
          apply_step(cur, step);
        },
        Some(anchor),
        target.steps,
      );
    switch (cur) {
    | AtSegment(seg) => slice(seg, ~from=target.from, ~until=target.until)
    | AtTile(_) => None
    };
  };
};

type t = {
  /* Locator used to rebuild this splice from the live piece tree.
   * Retained so view-emitted actions can be confined without the
   * caller re-supplying the target. */
  target: Target.t,
  /* The sub-segment of the main editor being displayed, re-measured
   * from a local (0,0) origin for standalone display. The measured map
   * contains exactly the splice's (recursive) pieces, which makes it
   * double as a membership map for caret confinement. */
  splice: CachedSyntax.splice,
  /* Absolute position of the splice's first piece in the main editor */
  origin: Point.t,
  /* Absolute position just after the splice's last piece */
  last: Point.t,
};

let is_space_piece = (p: Piece.t) =>
  switch (p) {
  | Secondary(w) => !Secondary.is_linebreak(w) && !Secondary.is_comment(w)
  | _ => false
  };

let is_linebreak_piece = (p: Piece.t) =>
  switch (p) {
  | Secondary(w) => Secondary.is_linebreak(w)
  | _ => false
  };

/* Strip ONE boundary separator from each end of the slot: the single
 * space the host formatting puts around the term — or, when the host
 * breaks the line there, the linebreak together with the indentation
 * whitespace beside it. Displaying the separator would show a stray
 * space (or a line break) in the chip and let the user delete
 * whitespace the host syntax needs. Only that much is stripped: any
 * FURTHER whitespace is the user's own mid-typing input (`5 ` on the
 * way to `5 + …`) and must stay visible in the chip — and inside the
 * splice, or the caret would strand outside it after the space. The
 * caret positions adjacent to the stripped separator are still
 * legitimate positions inside the splice; `caret_in_splice` and
 * `caret_point` below tolerate them explicitly. */
let trim_separators = (seg: Segment.t): Segment.t => {
  let rec drop_spaces =
    fun
    | [p, ...ps] when is_space_piece(p) => drop_spaces(ps)
    | ps => ps;
  let leading =
    fun
    | [lb, ...rest] when is_linebreak_piece(lb) =>
      /* linebreak, then the next line's indentation */
      drop_spaces(rest)
    | [ws, ...rest] when is_space_piece(ws) => rest
    | seg => seg;
  /* Trailing, on the reversed slot: the separator reads
   * [indentation…, linebreak] there (document order: linebreak, then
   * the indentation preceding the host's closing delimiter). */
  let trailing_rev = rev =>
    switch (drop_spaces(rev)) {
    | [lb, ...rest] when is_linebreak_piece(lb) => rest
    | _ =>
      switch (rev) {
      | [ws, ...rest] when is_space_piece(ws) => rest
      | rev => rev
      }
    };
  seg |> leading |> List.rev |> trailing_rev |> List.rev;
};

/* Extent of a splice frame in its own coordinate space. */
let extent = (splice: CachedSyntax.splice): option((Point.t, Point.t)) => {
  open OptUtil.Syntax;
  let* first = ListUtil.hd_opt(splice.segment);
  let* last_piece = ListUtil.last_opt(splice.segment);
  let* m_first = Measured.find_by_id(Piece.id(first), splice.measured);
  let+ m_last = Measured.find_by_id(Piece.id(last_piece), splice.measured);
  (m_first.origin, m_last.last);
};

/* Build the splice for `target` in the main editor. None when the
 * target can't be resolved, the region is empty, or its pieces aren't
 * measured — e.g. mid-rewrite states. Callers should degrade to a
 * read-only rendering in that case; it resolves on the next pass. */
let mk = (editor: Editor.t, ~target: Target.t): option(t) => {
  open OptUtil.Syntax;
  let syntax: CachedSyntax.t = editor.syntax;
  let cached =
    Target.whole_content_id(target)
    |> OptUtil.and_then(id => CachedSyntax.splice_opt(id, syntax));
  switch (cached) {
  | Some(splice) =>
    /* Splice wrapper: the syntax cache already measured this content in
     * the splice's own frame, and there is no host separator beside the
     * region to trim — the wrapper delimits it exactly. */
    let+ (origin, last) = extent(splice);
    {
      target,
      splice,
      origin,
      last,
    };
  | None =>
    let* segment = Target.resolve(target, CachedSyntax.segment(syntax));
    let segment = trim_separators(segment);
    let* first = ListUtil.hd_opt(segment);
    let* last_piece = ListUtil.last_opt(segment);
    let* m_first =
      Measured.find_by_id(Piece.id(first), CachedSyntax.measured(syntax));
    let* m_last =
      Measured.find_by_id(
        Piece.id(last_piece),
        CachedSyntax.measured(syntax),
      );
    let measured =
      Measured.of_segment(segment, syntax.shape_map, Id.Map.empty);
    Some({
      target,
      splice: {
        segment,
        measured,
        projector_list: CachedSyntax.splice_projector_list(segment),
      },
      origin: m_first.origin,
      last: m_last.last,
    });
  };
};

/* Translate a pointer goal from splice-local coordinates to the main
 * editor's coordinate space, clamping to the splice's extent so mouse
 * gestures can never land the main caret outside the splice. Exact for
 * single-line splices (the only kind the stepper currently renders);
 * multi-line splices translate row-wise without clamping. */
let translate_goal = (sub: t, p: Point.t): Point.t =>
  if (sub.origin.row == sub.last.row) {
    Point.{
      row: sub.origin.row,
      col: min(sub.origin.col + max(p.col, 0), sub.last.col),
    };
  } else {
    Point.{
      row: sub.origin.row + p.row,
      col: p.row == 0 ? sub.origin.col + p.col : p.col,
    };
  };

/* Whether the main caret is currently inside the splice, judged by
 * EITHER of the caret's neighboring pieces being one of the splice's
 * (recursive) pieces. Both neighbors matter: at the splice's very
 * first position the piece to the caret's left is the whitespace just
 * OUTSIDE the splice, while the piece to its right is the splice's
 * first piece — that position is inside. Used to gate caret /
 * selection decorations — the splice-local measured map only knows the
 * splice's pieces, so drawing with the caret elsewhere would raise —
 * and to gate editing, so actions routed to a focused sub-editor can't
 * invisibly edit some other part of the main buffer. */
let caret_in_splice = (sub: t, z: Zipper.t): bool => {
  let mem = (p: Piece.t) =>
    Measured.find_by_id(Piece.id(p), sub.splice.measured) |> Option.is_some;
  switch (Siblings.neighbors(ZipperBase.sibs_with_sel(z))) {
  | (Some(l), _) when mem(l) => true
  | (_, Some(r)) when mem(r) => true
  | _ => false
  };
};

/* Whether this sub-editor displays a splice wrapper, as opposed to a
 * region carved out of a host tile. Splices are their own coordinate
 * frame with their own caret routing (splice context entered/exited by
 * Move, pointer goals carried as SplicePoint), so the two flavors differ
 * in who owns the caret and who handles keys. */
let is_splice_frame = (sub: t): bool =>
  Target.whole_content_id(sub.target) != None;

/* Reframe an action emitted by this sub-editor's view. Pointer goals
 * from a sub-editor are container-relative, hence already in the
 * displayed region's coordinates; for a splice — which is its own
 * coordinate frame — that means Point goals must travel as SplicePoint
 * so Perform resolves them against the splice's own measured map instead
 * of the main editor's. Region sub-editors share the host's frame, so
 * their goals are translated by `translate_goal` instead. */
let reframe_action = (sub: t, action: Action.t): Action.t =>
  switch (Target.whole_content_id(sub.target), action) {
  | (Some(id), Move(Point(goal, _))) => Move(SplicePoint(id, goal))
  | (Some(id), Select(Resize(Point(goal, _)))) =>
    Select(Resize(SplicePoint(id, goal)))
  | (_, action) => action
  };

/* Whether this sub-editor's frame owns the caret, i.e. whether it is the
 * surface that should draw it. For a splice that is the zipper's splice
 * context — a caret inside a splice is not in the enclosing frame even
 * though the splice's pieces are. A carved-out region introduces no
 * frame boundary, so plain membership decides. */
let owns_caret = (sub: t, z: Zipper.t): bool =>
  switch (Target.whole_content_id(sub.target)) {
  | Some(id) => Zipper.splice_context(z) == Some(id)
  | None => caret_in_splice(sub, z)
  };

/* Caret point in splice-local coordinates. `Zipper.Caret.point` reads
 * the caret's representative piece (left-preferred), which anywhere in
 * the splice's FIRST piece is the whitespace outside the splice — fall
 * back to the in-splice right neighbor's origin there, keeping the
 * caret's inner offset (`Inner(k)` is right-neighbor-relative, so a
 * caret mid-way through the splice's first token lands here too and
 * must not be drawn at the token's start). */
let caret_point = (sub: t, z: Zipper.t): option(Point.t) =>
  switch (Zipper.Caret.point(sub.splice.measured, z)) {
  | p => Some(p)
  | exception _ =>
    switch (Siblings.neighbors(ZipperBase.sibs_with_sel(z))) {
    | (_, Some(r)) =>
      Measured.find_by_id(Piece.id(r), sub.splice.measured)
      |> Option.map((m: Measured.measurement) =>
           Point.{
             row: m.origin.row,
             col: m.origin.col + Zipper.Caret.offset(z),
           }
         )
    | (Some(l), _) =>
      Measured.find_by_id(Piece.id(l), sub.splice.measured)
      |> Option.map((m: Measured.measurement) => m.last)
    | _ => None
    }
  };

/* Caret sits at the left/right edge of the splice (caret confinement:
 * deletion must not eat the surrounding syntax; unmodified arrows
 * escape focus instead of leaving the splice). */
let caret_at_left = (sub: t, z: Zipper.t): bool =>
  z.caret == Outer
  && {
    open OptUtil.Syntax;
    let* leftmost = ListUtil.hd_opt(sub.splice.segment);
    let* (right_of_caret, _) = Relatives.pop(Right, z.relatives);
    Piece.id(leftmost) == Piece.id(right_of_caret) ? Some() : None;
  }
  |> Option.is_some;

let caret_at_right = (sub: t, z: Zipper.t): bool =>
  z.caret == Outer
  && {
    open OptUtil.Syntax;
    let* rightmost = ListUtil.last_opt(sub.splice.segment);
    let* (left_of_caret, _) = Relatives.pop(Left, z.relatives);
    Piece.id(rightmost) == Piece.id(left_of_caret) ? Some() : None;
  }
  |> Option.is_some;

/* Update-time confinement for actions routed from a sub-editor view
 * (see CodeEditable.Update.PerformConfined). The view-level key guards
 * are only UX sugar: a Key.handler closes over the render-time zipper,
 * so during a keystroke burst every event is judged against the same
 * stale state and the caret can walk out of the splice before the next
 * render refreshes the guard. These checks run in `update` against the
 * live model, so they cannot race.
 *
 * `confine_pre` vets an action before it is performed:
 *   - None                 → reject outright;
 *   - Some(sub)            → proceed; if the action is a pure
 *     move/select, additionally `confine_post` the resulting zipper
 *     (piece ids are unchanged by moves, so the pre-action splice is
 *     still an exact membership map) and drop the action if the caret
 *     ended up outside the splice. */
let confine_pre =
    (~target: Target.t, ~action: Action.t, editor: Editor.t): option(t) => {
  open OptUtil.Syntax;
  let* sub = mk(editor, ~target);
  let z = editor.state.zipper;
  if (Action.is_edit(action)) {
    /* Edits must start from inside the splice (a caret parked outside
     * would invisibly edit some other part of the main buffer), and
     * deletion at the splice's edges must not eat the host syntax. */
    let edge_delete =
      switch (action) {
      | Destruct(Left) =>
        caret_at_left(sub, z) && Selection.is_empty(z.selection)
      | Destruct(Right) =>
        caret_at_right(sub, z) && Selection.is_empty(z.selection)
      | _ => false
      };
    caret_in_splice(sub, z) && !edge_delete ? Some(sub) : None;
  } else {
    /* Moves and selects are always dispatched — clicking into the
     * splice from outside is itself a Move — and judged by where they
     * land instead. */
    Some(
      sub,
    );
  };
};

let confine_post = (sub: t, z: Zipper.t): bool => caret_in_splice(sub, z);

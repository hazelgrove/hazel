open Util;
open Haz3lcore;

/* A sub-editor is a window onto one term of the main editor, the
 * splice. Its pieces are the main editor's pieces, and the zipper,
 * statics and undo history are the main editor's too, so it can't fall
 * out of sync with the syntax it shows: an edit in either view edits the
 * same segment. The stepper uses them to edit slices of a theorem's
 * proof in place (an induction's scrutinee, a case's pattern, a form's
 * argument). CodeEditable.View renders them and confines their edits. */

/* Where a splice is: a host tile and a path into it. A term's own id
 * won't do, because term ids change as the user types (a hole becomes a
 * var, a var the child of a cons) while the view lags the statics, so a
 * term-id key goes stale mid-burst and drops keystrokes. The host tile's
 * id survives any edit inside it. The caller knows the grammar:
 *
 *   let scrut =
 *     Target.child(~anchor=induction_id, 0)
 *     |> Target.until(Before(NthTile(["|", "=>"], 0)));
 *
 *   let pattern = i =>
 *     Target.child(~anchor=induction_id, 0)
 *     |> Target.descend(NthTile(["|", "=>"], i), ~child=0);
 */
module Target = {
  /* `NthTile(label, n)`: the n-th (0-based) direct tile piece whose label
   * matches. Tiles nested inside children are ignored. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type selector =
    | NthTile(Label.t, int);

  /* `BeforeOrEnd` is like `Before`, but the segment end when no piece
   * matches — for ranges bounded by a tile that may legitimately be absent
   * (e.g. an induction scrutinee when there are no case rules yet). */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type bound =
    | Start
    | End
    | Before(selector)
    | BeforeOrEnd(selector)
    | After(selector);

  /* `IntoChild(i)` enters the i-th mold child of the current tile;
   * `IntoSelected(sel, i)` selects a tile among the current segment's
   * direct pieces and enters its i-th mold child. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type step =
    | IntoChild(int)
    | IntoSelected(selector, int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    anchor: Id.t,
    steps: list(step),
    from: bound,
    until: bound,
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
    | (NthTile(label, _), Tile(tile)) => Tile.label(tile) == label
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

  type cursor =
    | AtTile(Base.tile)
    | AtSegment(Segment.t);

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

  /* None when the anchor, a step or the range can't be found, which
   * callers take as a passing state mid-rewrite. */
  let resolve = (target: t, root: Segment.t): option(Segment.t) => {
    open OptUtil.Syntax;
    let rec find_tile = (seg: Segment.t): option(Base.tile) =>
      List.find_map(
        (p: Piece.t) =>
          switch (p) {
          | Tile(tile) when tile.id == target.anchor => Some(tile)
          | Tile(tile) => List.find_map(find_tile, tile.children)
          | Grout(_)
          | Secondary(_)
          | Projector(_) => None
          },
        seg,
      );
    let* tile = find_tile(root);
    let* cur =
      List.fold_left(
        (cur, step) => {
          let* cur = cur;
          apply_step(cur, step);
        },
        Some(AtTile(tile)),
        target.steps,
      );
    switch (cur) {
    | AtSegment(seg) => slice(seg, ~from=target.from, ~until=target.until)
    | AtTile(_) => None
    };
  };
};

type t = {
  /* Kept so that the view's actions can be confined to the splice. */
  target: Target.t,
  /* Measured from a local origin. It measures exactly the splice's
   * pieces, so it also answers whether a piece is in the splice. */
  splice: CachedSyntax.splice,
  /* Where the splice starts and ends in the main editor. */
  origin: Point.t,
  last: Point.t,
};

/* Strips one separator from each end of the slot: the space the host's
 * layout puts around the term, or a line break with the indentation
 * beside it. Shown, it would be a stray space in the chip that the user
 * could delete from the host's syntax. Any more whitespace is the user's
 * own, mid-typing (`5 ` on the way to `5 + …`), and stays, or the caret
 * would be stranded outside the splice after it. The caret positions
 * beside a stripped separator still count as in the splice (see
 * `caret_in_splice` and `caret_point`). */
let trim_separators = (seg: Segment.t): Segment.t => {
  let is_space_piece = (p: Piece.t) =>
    switch (p) {
    | Secondary(w) => !Secondary.is_linebreak(w) && !Secondary.is_comment(w)
    | Tile(_)
    | Grout(_)
    | Projector(_) => false
    };
  let is_linebreak_piece = (p: Piece.t) =>
    switch (p) {
    | Secondary(w) => Secondary.is_linebreak(w)
    | Tile(_)
    | Grout(_)
    | Projector(_) => false
    };
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

/* None when the target can't be resolved, the region is empty or its
 * pieces aren't measured, as happens mid-rewrite; callers render
 * read-only until the next pass. */
let mk = (editor: Editor.t, ~target: Target.t): option(t) => {
  open OptUtil.Syntax;
  let syntax: CachedSyntax.t = editor.syntax;
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
  let measured = Measured.of_segment(segment, syntax.shape_map, Id.Map.empty);
  Some({
    target,
    splice: {
      segment,
      measured,
      projector_list: [],
    },
    origin: m_first.origin,
    last: m_last.last,
  });
};

/* A pointer position in the splice, in the main editor's coordinates,
 * clamped to the splice so the mouse can't put the caret outside it.
 * A splice over several lines is translated row by row, unclamped. */
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

/* Whether either of the caret's neighbours is in the splice. Both count:
 * at the splice's first position the left neighbour is the whitespace
 * just outside it. */
let caret_in_splice = (sub: t, z: Zipper.t): bool => {
  let mem = (p: Piece.t) =>
    Measured.find_by_id(Piece.id(p), sub.splice.measured) |> Option.is_some;
  switch (Siblings.neighbors(ZipperBase.sibs_with_sel(z))) {
  | (Some(l), _) when mem(l) => true
  | (_, Some(r)) when mem(r) => true
  | _ => false
  };
};

/* The caret in splice-local coordinates. `Zipper.Caret.point` reads the
 * piece left of the caret, which in the splice's first piece is the
 * whitespace outside the splice; there the right neighbour's origin is
 * used instead, plus the caret's offset into it. */
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

/* At an edge of the splice, deleting outward would eat the host's syntax,
 * and an arrow key escapes the sub-editor rather than leave the splice. */
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

/* Whether a sub-editor's action may run (CodeEditable's PerformConfined),
 * judged in `update` against the live zipper: the view's key guards see
 * the zipper of the last render, which is stale during a burst of keys.
 * None rejects the action. Some(sub) lets it run; a move or select is
 * then dropped if the caret lands outside the splice, which `sub` still
 * describes exactly because a move doesn't change piece ids. */
let confine_pre =
    (~target: Target.t, ~action: Action.t, editor: Editor.t): option(t) => {
  open OptUtil.Syntax;
  let* sub = mk(editor, ~target);
  let z = editor.state.zipper;
  if (Action.is_edit(action)) {
    /* A caret outside the splice would edit some other part of the
     * buffer, out of sight. */
    let edge_delete =
      switch (action) {
      | Destruct(Local(Left, _)) =>
        caret_at_left(sub, z) && Selection.is_empty(z.selection)
      | Destruct(Local(Right, _)) =>
        caret_at_right(sub, z) && Selection.is_empty(z.selection)
      | _ => false
      };
    caret_in_splice(sub, z) && !edge_delete ? Some(sub) : None;
  } else {
    /* Clicking into the splice from outside is itself a move, so moves
     * are judged by where they land. */
    Some(
      sub,
    );
  };
};

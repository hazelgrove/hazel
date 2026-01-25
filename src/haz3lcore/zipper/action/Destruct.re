open Zipper;
open Util;
open OptUtil.Syntax;

/* Captures the UUID of a single grout or tile about to be deleted
 * so as to transfer that id to its replacement if possible. See
 * also Insert.preserve_grout_id */
let capture = (z): t => {
  let junk_id =
    switch (z.selection.content) {
    | [Tile(t)] when List.length(t.label) == 1 => Some(t.id)
    | [Tile(t)]
        when
          List.length(Tile.effective_label(t)) == 1
          && !
               List.exists(
                 (tt: Tile.t) => tt.id == t.id,
                 Relatives.local_missing_shards(z.relatives),
               ) =>
      /* Don't want to capture the UUID if there are other shards
       * that will persist with that id. This is a subtle condition,
       * reliant on the selection being length 1 */
      Some(t.id)
    | [Grout(g)] => Some(g.id)
    | _ => None
    };
  Grout.cache_id(junk_id);
  z;
};

/* Check if a piece is a space (not linebreak, not comment) */
let is_space_piece = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_space(s)
  | _ => false
  };

/* Check if a piece is a linebreak */
let is_linebreak_piece = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_linebreak(s)
  | _ => false
  };

/* Check if a piece is whitespace (space or linebreak, not comment) */
let is_whitespace_piece = (p: Piece.t): bool =>
  is_space_piece(p) || is_linebreak_piece(p);

/* Check if cursor is in "leading whitespace" position:
   - Cursor is Outer
   - All pieces to the left (back to linebreak) are spaces
   Returns the count of spaces if true, None otherwise */
let leading_whitespace_context = (z: t): option(int) =>
  if (z.caret != Outer) {
    None;
  } else {
    let (left_sibs, _) = z.relatives.siblings;
    /* Count spaces from right end of left_sibs until we hit linebreak or non-space */
    let rec count_spaces = (sibs, n) =>
      switch (sibs) {
      | [] => Some(n) /* Start of segment = start of line */
      | [p, ...rest] when is_space_piece(p) => count_spaces(rest, n + 1)
      | [p, ..._] when is_linebreak_piece(p) => Some(n) /* Found linebreak */
      | _ => None /* Found non-whitespace content */
      };
    count_spaces(List.rev(left_sibs), 0);
  };

/* Check if the left neighbor is whitespace (space or linebreak) */
let left_neighbor_is_whitespace = (z: t): bool =>
  switch (Zipper.generalized_neighbor(Left, z)) {
  | Some(p) => is_whitespace_piece(p)
  | None => false
  };

let delete = (d: Direction.t, z: t): option(t) => {
  let+ z = select(d, z);
  let z = capture(z);
  destroy_selection(z);
};

let outer = (d: Direction.t, z: t): option(t) =>
  switch (Zipper.neighbor_token(d, z)) {
  | Some(t) when Token.length(t) > 1 && !Token.is_string_or_comment(t) =>
    Insert.replace_shard(d, Token.rm_edge(d, t), z)
  | _ => delete(d, z)
  };

let rm_nth_right = (idx, t, z) =>
  Insert.replace_shard(Right, Token.rm_nth(t, idx), z);

let inner_left = (idx: int, z: t): option(t) =>
  switch (Zipper.neighbor_token(Right, z)) {
  | Some(t) when Token.is_string_or_comment(t) && idx == 0 =>
    z |> Caret.set(Outer) |> delete(Right)
  | Some(t) =>
    let z = Caret.set(idx == 0 ? Outer : Inner(idx - 1), z);
    let+ z_init = rm_nth_right(idx, t, z);
    let z_final = Zipper.remold_regrout(Left, z_init);
    Insert.adjust_caret_pos(~z_final, ~z_init);
  | None => z |> Caret.set(Outer) |> delete(Right)
  };

let is_last_inner_pos = (t, idx) => Token.length(t) - 2 == idx;

let inner_right = (idx: int, z: t): option(t) =>
  switch (Zipper.neighbor_token(Right, z)) {
  | Some(t) when Token.is_string_or_comment(t) && is_last_inner_pos(t, idx) =>
    z |> Caret.set(Outer) |> delete(Right)
  | Some(t) =>
    let* z = rm_nth_right(idx + 1, t, z);
    is_last_inner_pos(t, idx)
      ? z |> Caret.set(Outer) |> Zipper.move(Right) : Some(z);
  | None => z |> Caret.set(Outer) |> delete(Left)
  };

let destruct = (d: Direction.t, z: t): option(t) =>
  switch (z.caret) {
  | _ when z.selection.content != [] =>
    Some(z |> capture |> destroy_selection)
  | Outer => outer(d, z)
  | Inner(idx) =>
    switch (d) {
    | Left => inner_left(idx, z)
    | Right => inner_right(idx, z)
    }
  };

/* Delete multiple spaces (for indent-level backspace) */
let rec delete_spaces = (n: int, z: t): option(t) =>
  if (n <= 0) {
    Some(z);
  } else {
    switch (delete(Left, z)) {
    | None => Some(z)
    | Some(z) => delete_spaces(n - 1, z)
    };
  };

/* Hungry delete: delete all contiguous whitespace to the left,
   including at most one linebreak. Stops at non-whitespace or
   after consuming one linebreak. */
let rec hungry_delete = (z: t, seen_linebreak: bool): option(t) =>
  switch (Zipper.generalized_neighbor(Left, z)) {
  | Some(p) when is_space_piece(p) =>
    /* Delete space and continue */
    let* z = delete(Left, z);
    hungry_delete(z, seen_linebreak);
  | Some(p) when is_linebreak_piece(p) && !seen_linebreak =>
    /* Delete linebreak (first one only) and continue */
    let* z = delete(Left, z);
    hungry_delete(z, true);
  | _ =>
    /* Stop: hit non-whitespace, second linebreak, or start of segment */
    Some(z)
  };

/* Delete by token: delete the entire neighboring token/piece.
   For multi-char tokens like "foo", this deletes the whole token.
   For single-char pieces, this acts like normal delete. */
let delete_token = (d: Direction.t, z: t): option(t) => {
  /* If caret is inside a token, first escape to outer */
  let z =
    switch (z.caret) {
    | Inner(_) => Caret.set(Outer, z)
    | Outer => z
    };
  /* Now delete the adjacent piece */
  delete(d, z);
};

/* Standard destruct with post-processing */
let destruct_with_cleanup = (d: Direction.t, z: t): option(t) => {
  let+ z = destruct(d, z);
  z |> Insert.merge_or_noop |> remold_regrout(d) |> Insert.merge_or_noop;
};

let go = (d: Direction.t, chunk: Action.chunkiness, z: t): option(t) =>
  switch (Triggers.destruct(z)) {
  | Some(z) => Some(z)
  | None =>
    switch (chunk) {
    | Action.ByChar =>
      /* Check for indent-level backspace: if in leading whitespace, delete 2 spaces */
      switch (d, leading_whitespace_context(z)) {
      | (Left, Some(n)) when n > 0 =>
        let to_delete = min(2, n);
        let+ z = delete_spaces(to_delete, z);
        z |> Insert.merge_or_noop |> remold_regrout(d) |> Insert.merge_or_noop;
      | _ => destruct_with_cleanup(d, z)
      }
    | Action.ByToken =>
      /* Check if we're in a whitespace run */
      if (d == Left && left_neighbor_is_whitespace(z)) {
        /* Hungry delete: delete all whitespace including one linebreak */
        let+ z = hungry_delete(z, false);
        z |> Insert.merge_or_noop |> remold_regrout(d) |> Insert.merge_or_noop;
      } else {
        /* Delete by token */
        let+ z = delete_token(d, z);
        z |> Insert.merge_or_noop |> remold_regrout(d) |> Insert.merge_or_noop;
      }
    }
  };

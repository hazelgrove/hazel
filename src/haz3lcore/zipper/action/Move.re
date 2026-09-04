open Util_web;
open OptUtil.Syntax;
open Zipper;

let by_char_left = (z: t): option(t) =>
  switch (z.caret, Caret.nhbr_max_idx(Left, z)) {
  | (Outer, None) => move(Left, z)
  | (Outer, Some(max_idx)) => z |> Caret.set(Inner(max_idx)) |> move(Left)
  | (Inner(char), None | Some(_)) when char == 0 =>
    z |> Caret.set(Outer) |> Option.some
  | (Inner(char), None | Some(_)) =>
    z |> Caret.set(Inner(char - 1)) |> Option.some
  };

let by_char_right = (z: t): option(t) =>
  switch (z.caret, Caret.nhbr_max_idx(Right, z)) {
  | (Outer, None) => move(Right, z)
  | (Outer, Some(_)) => z |> Caret.set(Inner(0)) |> Option.some
  | (Inner(char), Some(max_idx)) when char >= max_idx =>
    z |> Caret.set(Outer) |> move(Right)
  | (Inner(_), None) =>
    /* Inner references the right-side piece. If the right neighbor
     * isn't a multi-char token (grout, projector, single-char token,
     * or empty), `Inner(char)` has no valid internal position to step
     * to — collapse to Outer and pop. Without this, `do_towards_point`
     * loops when Inner was set by a prior char-level selection and
     * the post-unselect right neighbor can't accept it: col advances
     * by 1 per step but row never changes, so `(Under, _)` keeps
     * recursing. */
    z |> Caret.set(Outer) |> move(Right)
  | (Inner(char), Some(_)) =>
    z |> Caret.set(Inner(char + 1)) |> Option.some
  };

let by_char = (d: Direction.t, z: t): option(t) =>
  switch (d) {
  | Left => by_char_left(z)
  | Right => by_char_right(z)
  };

let by_token = (d: Direction.t, z: t): option(t) =>
  switch (z.caret) {
  | Outer => move(d, z)
  | Inner(_) =>
    let z = Caret.set(Outer, z);
    switch (d) {
    | Left => Some(z)
    | Right => move(Right, z)
    };
  };

let local = (chunkiness: Action.chunkiness, d: Direction.t, z: t): option(t) => {
  let z = unselect(z);
  switch (chunkiness) {
  | ByToken => by_token(d, z)
  /* BySmart is a selection-only granularity; for caret movement it
   * degrades to ByChar. Never emitted for Move actions in practice. */
  | ByChar
  | BySmart => by_char(d, z)
  };
};

/* Do move_action until the indicated piece is such that piece_p is true,
   restarting from the beginning/end if not found in forward direction.
   If no such piece is found, don't move. */
let move_until_wrap = (p, d, z) =>
  switch (do_until(local(ByToken, d), p, z)) {
  | None =>
    let z = do_to_extreme(local(ByToken, Direction.toggle(d)), z);
    do_until(local(ByToken, d), p, z);
  | Some(z) => Some(z)
  };

/* This moves the caret to the directionmost edge of
 * the piece with the target id. Note that this may not
 * mean that the piece at that id will be considered
 * indicated from the point of view of the code deco
 * and cursor info display. This is true even when the
 * direction is set to the Left, though in relatively
 * few cases including for example `true && !|flag`,
 * where the caret (|) is at the leftmost edge of
 * `flag`, but the not operator ("!") is indicated */
let jump_to_side_of_id = (d: Direction.t, z, id): option(t) => {
  let at_piece =
    fun
    | (_, Some(piece)) when d == Left => Piece.id(piece) == id
    | (Some(piece), _) when d == Right => Piece.id(piece) == id
    | _ => false;
  let z = do_to_extreme(local(ByToken, d), z);
  at_piece(Zipper.generalized_neighbors(z))
    ? Some(z) : do_until(local(ByToken, Direction.toggle(d)), at_piece, z);
};

/* Caret-position invariant for the char-level selection model:
 *   `Inner(n)` is right-neighbor-relative — the right-generalized
 *   neighbor must be a Piece P with `piece_max_idx(P) >= n`.
 * `by_char` / `by_token` movements maintain this via `set + move`.
 * Selection operations that lift content + reassemble can break it
 * (reassemble may absorb the named piece into an ancestor or shuffle
 * adjacent Secondaries across the caret). Such transitions must
 * re-canonicalize the structural state before setting an Inner caret;
 * the helpers below do that via `jump_to_shard`. */

/* (tile_id, shard_idx) precisely identifies a single shard of a tile.
 * Shards of a multi-shard tile share `tile_id` so id alone is
 * ambiguous; `shard_idx` is the shard's index within the tile's label. */
let shard_locator = (p: option(Piece.t)): option((Id.t, int)) =>
  switch (p) {
  | Some(Tile(t)) =>
    switch (t.shards) {
    | [idx] => Some((t.id, idx))
    | _ => None
    }
  | _ => None
  };

/* Navigate so the right-generalized neighbor is the shard identified
 * by (tile_id, shard_idx). by_token-based, so the resulting structural
 * state is canonical. */
let jump_to_shard = (z: t, tile_id: Id.t, shard_idx: int): option(t) => {
  let at_target = ((_, r): Zipper.neighbors) =>
    switch (r) {
    | Some(Piece.Tile(t)) =>
      Id.equal(t.id, tile_id)
      && (
        switch (t.shards) {
        | [idx] => idx == shard_idx
        | _ => false
        }
      )
    | _ => false
    };
  let z = do_to_extreme(local(ByToken, Left), z);
  at_target(Zipper.generalized_neighbors(z))
    ? Some(z) : do_until(local(ByToken, Right), at_target, z);
};

/* Post-unselect canonicalization for collapsing a char-level selection
 * to an Inner caret target. Pre-unselect, capture the boundary piece
 * via `shard_locator`; pass `target_caret` and `locator` here after
 * `Zipper.directional_unselect`. No-op for Outer targets or when the
 * boundary piece isn't a single-shard tile. */
let canonicalize_inner_unselect =
    (~locator: option((Id.t, int)), ~target_caret: CaretBase.t, z: t): t =>
  switch (locator, target_caret) {
  | (Some((tile_id, shard_idx)), Inner(_)) =>
    switch (jump_to_shard(z, tile_id, shard_idx)) {
    | Some(z') => Zipper.Caret.set(target_caret, z')
    | None => Zipper.Caret.set(target_caret, z)
    }
  | _ => z
  };

/* Moves to the left side of the token with the given id,
 * then checks if it's indicated. If not, move one token
 * to the right. I believe but have not proved this
 * always results in the token being indicated  */
let jump_to_id_indicated = (z: t, id: Id.t): option(t) => {
  let* z_l = jump_to_side_of_id(Left, z, id);
  let* indicated_id = Indicated.index(z_l);
  if (id == indicated_id) {
    Some(z_l);
  } else {
    let* z_r = jump_to_side_of_id(Right, z, id);
    let* indicated_id = Indicated.index(z_r);
    id == indicated_id ? Some(z_r) : None;
  };
};

let to_next_grout: (Direction.t, t) => option(t) =
  move_until_wrap(neighbors =>
    switch (neighbors) {
    | (Some(Grout(_)), _) => true
    | _ => false
    }
  );

let vertical =
    (~col_target: int, ~measured: Measured.t, d: Action.vertical, z: t)
    : option(t) => {
  let goal =
    Point.{
      col: col_target,
      row: Zipper.Caret.point(measured, z).row + (d == Down ? 1 : (-1)),
    };
  do_towards_point(~force_progress=true, ~measured, local(ByChar), goal, z);
};

let to_point = (~measured: Measured.t, ~goal: Point.t, z: t): option(t) =>
  switch (do_towards_point(~measured, local(ByChar), goal, z)) {
  | None => Some(z)
  | Some(z) => Some(z)
  };

let to_start: t => t = do_to_extreme(local(ByToken, Left));

let to_end: t => t = do_to_extreme(local(ByToken, Right));

let to_linebreak = (d: Direction.t, z: t): option(t) =>
  do_until_linebreak(local(ByToken, d), d, z);

let to_next_problem =
    (~measured: Measured.t, ~problem_ids: Seq.t(Id.t), d: Direction.t, z: t)
    : option(t) => {
  let cursor_pos = Zipper.Caret.point(measured, z);
  /* Sort problem IDs by measured position */
  let sorted =
    problem_ids
    |> Seq.filter_map(id =>
         switch (Measured.find_by_id(id, measured)) {
         | Some({origin, _}) => Some((id, origin))
         | None => None
         }
       )
    |> List.of_seq
    |> List.sort(((_, p1), (_, p2)) => Point.compare(p1, p2));
  switch (sorted) {
  | [] => None
  | _ =>
    let target =
      switch (d) {
      | Right =>
        switch (
          List.find_opt(
            ((_, pos)) => Point.compare(pos, cursor_pos) > 0,
            sorted,
          )
        ) {
        | Some((id, _)) => Some(id)
        | None => Some(fst(List.hd(sorted))) /* wrap to first */
        }
      | Left =>
        let rev = List.rev(sorted);
        switch (
          List.find_opt(
            ((_, pos)) => Point.compare(pos, cursor_pos) < 0,
            rev,
          )
        ) {
        | Some((id, _)) => Some(id)
        | None => Some(fst(List.hd(rev))) /* wrap to last */
        };
      };
    switch (target) {
    | Some(id) => jump_to_id_indicated(z, id)
    | None => None
    };
  };
};

let move_dispatch =
    (
      ~statics: Language.Statics.Map.t,
      ~problem_ids: Seq.t(Id.t),
      ~col_target: int,
      ~measured: Measured.t,
      d: Action.move,
      z: t,
    )
    : option(t) =>
  switch (d) {
  | Local(d, chunk) => local(chunk, d, z)
  | Start => Some(to_start(z))
  | End => Some(to_end(z))
  | Line(d) => to_linebreak(d, z)
  | Vertical(d, _) => vertical(~measured, ~col_target, d, z)
  | Point(goal, _) => to_point(~measured, ~goal, z)
  | Goal(Hole(d)) => to_next_grout(d, z)
  | Goal(NextProblem(d)) => to_next_problem(~measured, ~problem_ids, d, z)
  | Goal(TileId(id)) => jump_to_id_indicated(z, id)
  | Goal(BindingSiteOfIndicatedVar) =>
    let* ci = Indicated.ci_of(z, statics);
    let* binding_id = Language.Info.get_binding_site(ci);
    jump_to_id_indicated(z, binding_id);
  };

let pre_unselect = (a: Action.move, z: t): t => {
  let d =
    switch (a) {
    | Local(d, _) => d
    | Vertical(Up, _) => Left
    | Vertical(Down, _) => Right
    | Start
    | End
    | Line(_)
    | Point(_)
    | Goal(_) => z.selection.focus
    };
  let landing_at_anchor = d != z.selection.focus;
  let target_caret: CaretBase.t =
    landing_at_anchor ? z.selection.anchor_caret : z.caret;
  let locator =
    shard_locator(
      landing_at_anchor
        ? Selection.anchor_piece(z.selection)
        : Selection.focus_piece(z.selection),
    );
  let z = Zipper.directional_unselect(d, z);
  canonicalize_inner_unselect(~locator, ~target_caret, z);
};
let go =
    (
      ~statics: Language.Statics.Map.t,
      ~problem_ids: Seq.t(Id.t),
      ~col_target: int,
      ~measured: Measured.t,
      a: Action.move,
      z: t,
    )
    : option(t) =>
  if (Selection.is_empty(z.selection)) {
    move_dispatch(~statics, ~problem_ids, ~col_target, ~measured, a, z);
  } else {
    let z = pre_unselect(a, z);
    switch (a) {
    // By char or smart just unselects (movement within selection)
    | Local(Left, ByChar | BySmart)
    | Local(Right, ByChar | BySmart) => Some(z)
    | _ =>
      switch (
        move_dispatch(~statics, ~problem_ids, ~col_target, ~measured, a, z)
      ) {
      | Some(z) => Some(z)
      /* Always empty selection on move action,
       * even if we don't actually move */
      | None => Some(z)
      }
    };
  };

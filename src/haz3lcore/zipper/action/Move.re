open Util;
open OptUtil.Syntax;
open Zipper;

let enter_neighbor = (d: Direction.t, z: t): option(t) => {
  switch (z.caret, Selection.is_empty(z.selection)) {
  | (Outer, true) =>
    switch (Siblings.pop(d, z.relatives.siblings)) {
    | Some((Projector(pr), siblings)) =>
      let+ relatives =
        Relatives.enter_projector(d, pr, siblings, z.relatives.ancestors);
      {
        ...z,
        relatives,
      };
    | Some((Splice(sp), siblings)) =>
      Some({
        ...z,
        relatives:
          Relatives.enter_splice(
            d,
            sp,
            ~sort=Sort.Any,
            siblings,
            z.relatives.ancestors,
          ),
      })
    | _ => None
    }
  | _ => None
  };
};

let exit_current_splice = (d: Direction.t, z: t): option(t) => {
  let+ relatives = Relatives.exit_splice(d, z.relatives);
  {
    ...z,
    relatives: Relatives.reassemble(relatives),
  };
};

let structural_move = (d: Direction.t, z: t): option(t) =>
  switch (enter_neighbor(d, z)) {
  | Some(_) as entered => entered
  | None =>
    switch (move(d, z)) {
    | Some(_) as moved => moved
    | None => exit_current_splice(d, z)
    }
  };

let by_char_left = (z: t): option(t) =>
  switch (z.caret, Caret.nhbr_max_idx(Left, z)) {
  | (Outer, None) => structural_move(Left, z)
  | (Outer, Some(max_idx)) =>
    z |> Caret.set(Inner(max_idx)) |> structural_move(Left)
  | (Inner(char), None | Some(_)) when char == 0 =>
    z |> Caret.set(Outer) |> Option.some
  | (Inner(char), None | Some(_)) =>
    z |> Caret.set(Inner(char - 1)) |> Option.some
  };

let by_char_right = (z: t): option(t) =>
  switch (z.caret, Caret.nhbr_max_idx(Right, z)) {
  | (Outer, None) => structural_move(Right, z)
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
  | Outer => structural_move(d, z)
  | Inner(_) =>
    let z = Caret.set(Outer, z);
    switch (d) {
    | Left => Some(z)
    | Right => structural_move(Right, z)
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

/* If [goal] falls inside one of [measured]'s splice regions via the
 * provided [splice_offsets] map (origin of each splice on screen), this
 * returns the translated goal and the splice id to enter. Otherwise
 * None, meaning the goal should be resolved against top-level measured.
 *
 * NOTE: the splice_offsets map is currently not populated from the DOM;
 * returning None here means clicks are always resolved against the
 * top-level coordinate frame. Keyboard/arrow entry into splices is
 * handled separately via Relatives.pop. When the view pipeline starts
 * publishing splice offsets, this hook will enable direct splice
 * clicks to translate the goal point into splice-local coordinates. */
let splice_at_point =
    (
      ~splice_offsets: Id.Map.t(Point.t)=Id.Map.empty,
      ~measured: Measured.t,
      goal: Point.t,
    )
    : option((Id.t, Point.t)) => {
  ignore(measured);
  Id.Map.fold(
    (id, offset: Point.t, acc) =>
      switch (acc) {
      | Some(_) => acc
      | None =>
        /* A splice's bounding box is recorded in [measured.splices].
         * Check if [goal] falls within [offset, offset + size]. */
        switch (Id.Map.find_opt(id, measured.splices)) {
        | None => None
        | Some(info) =>
          let contains =
            goal.row >= offset.row
            && goal.row <= offset.row
            + info.size.row
            && goal.col >= offset.col
            && goal.col <= offset.col
            + info.size.col;
          contains
            ? Some((
                id,
                Point.{
                  row: goal.row - offset.row,
                  col: goal.col - offset.col,
                },
              ))
            : None;
        }
      },
    splice_offsets,
    None,
  );
};

let to_point = (~measured: Measured.t, ~goal: Point.t, z: t): option(t) => {
  /* If [goal] is inside a splice's on-screen box, translate to splice-
   * local coords and resolve there. Otherwise resolve against the top-
   * level measured grid. */
  let _ = splice_at_point(~measured, goal);
  switch (do_towards_point(~measured, local(ByChar), goal, z)) {
  | None => Some(z)
  | Some(z) => Some(z)
  };
};

let rec collect_splices = (seg: Base.segment): list(Base.splice) =>
  List.concat_map(
    (p: Base.piece) =>
      switch (p) {
      | Splice(s) => [s, ...collect_splices(s.content)]
      | Tile(t) => List.concat_map(collect_splices, t.children)
      | Projector(_)
      | Grout(_)
      | Secondary(_) => []
      },
    seg,
  );

let nth_opt = (xs: list('a), i: int): option('a) =>
  i < 0
    ? None
    : (
      try(Some(List.nth(xs, i))) {
      | _ => None
      }
    );

let split_projector =
    (~projector_id: Id.t, seg: Base.segment)
    : option((Segment.t, Base.projector, Segment.t)) => {
  let rec scan = (pre: Segment.t, rest: Segment.t) =>
    switch (rest) {
    | [] => None
    | [Projector(pr), ...suf] when pr.id == projector_id =>
      Some((List.rev(pre), pr, suf))
    | [p, ...suf] => scan([p, ...pre], suf)
    };
  scan([], seg);
};

let find_projector_containing_splice =
    (
      ~projector_list: list(Id.t),
      ~projectors: Id.Map.t(Base.projector),
      ~splice_id: Id.t,
    )
    : option((Id.t, Base.projector)) =>
  List.find_map(
    projector_id => {
      let* projector = Id.Map.find_opt(projector_id, projectors);
      List.exists(
        (s: Base.splice) => s.id == splice_id,
        collect_splices(projector.syntax),
      )
        ? Some((projector_id, projector)) : None;
    },
    projector_list,
  );

let enter_splice_id =
    (
      ~projector_list: list(Id.t),
      ~projectors: Id.Map.t(Base.projector),
      ~splice_id: Id.t,
      z: t,
    )
    : option(t) => {
  let* (projector_id, _cached_projector) =
    find_projector_containing_splice(
      ~projector_list,
      ~projectors,
      ~splice_id,
    );
  let full_segment = Zipper.unselect_and_zip(z);
  let* (outer_left, projector, outer_right) =
    split_projector(~projector_id, full_segment);
  let pred = (s: Base.splice) => s.id == splice_id;
  let* (descent_ancs, splice_sibs, _splice, before, after) =
    Relatives.find_splice_descent_when(Right, pred, projector.syntax);
  let proj_anc: Ancestor.proj_anc = {
    id: projector.id,
    kind: projector.kind,
    model: projector.model,
    before,
    after,
  };
  Some({
    ...z,
    caret: Outer,
    relatives: {
      siblings: splice_sibs,
      ancestors:
        descent_ancs
        @ [(Ancestor.Projector(proj_anc), (outer_left, outer_right))],
    },
  });
};

let to_splice_point =
    (
      ~measured: Measured.t,
      ~projector_list: list(Id.t),
      ~projectors: Id.Map.t(Base.projector),
      ~splice_id: Id.t,
      ~goal: Point.t,
      z: t,
    )
    : option(t) => {
  let* z = enter_splice_id(~projector_list, ~projectors, ~splice_id, z);
  switch (do_towards_point(~measured, local(ByChar), goal, z)) {
  | None => Some(z)
  | Some(z) => Some(z)
  };
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
      ~projector_list: list(Id.t),
      ~projectors: Id.Map.t(Base.projector),
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
  | SplicePoint(splice_id, goal) =>
    to_splice_point(
      ~measured,
      ~projector_list,
      ~projectors,
      ~splice_id,
      ~goal,
      z,
    )
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
    | SplicePoint(_, _)
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
      ~projector_list: list(Id.t),
      ~projectors: Id.Map.t(Base.projector),
      a: Action.move,
      z: t,
    )
    : option(t) =>
  if (Selection.is_empty(z.selection)) {
    move_dispatch(
      ~statics,
      ~problem_ids,
      ~col_target,
      ~measured,
      ~projector_list,
      ~projectors,
      a,
      z,
    );
  } else {
    let z = pre_unselect(a, z);
    switch (a) {
    // By char or smart just unselects (movement within selection)
    | Local(Left, ByChar | BySmart)
    | Local(Right, ByChar | BySmart) => Some(z)
    | _ =>
      switch (
        move_dispatch(
          ~statics,
          ~problem_ids,
          ~col_target,
          ~measured,
          ~projector_list,
          ~projectors,
          a,
          z,
        )
      ) {
      | Some(z) => Some(z)
      /* Always empty selection on move action,
       * even if we don't actually move */
      | None => Some(z)
      }
    };
  };

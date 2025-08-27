open Zipper;
open Util;
open OptUtil.Syntax;

let move_by_char_left = (z: t): option(t) =>
  switch (z.caret, Caret.nhbr_max_idx(Left, z)) {
  | (Outer, None) => move(Left, z)
  | (Outer, Some(max_idx)) => z |> Caret.set(Inner(max_idx)) |> move(Left)
  | (Inner(char), None | Some(_)) when char == 0 =>
    z |> Caret.set(Outer) |> Option.some
  | (Inner(char), None | Some(_)) =>
    z |> Caret.set(Inner(char - 1)) |> Option.some
  };

let move_by_char_right = (z: t): option(t) =>
  switch (z.caret, Caret.nhbr_max_idx(Right, z)) {
  | (Outer, None) => move(Right, z)
  | (Outer, Some(_)) => z |> Caret.set(Inner(0)) |> Option.some
  | (Inner(char), Some(max_idx)) when char == max_idx =>
    z |> Caret.set(Outer) |> move(Right)
  | (Inner(char), None | Some(_)) =>
    z |> Caret.set(Inner(char + 1)) |> Option.some
  };

let move_by_char = (d: Direction.t, z: t): option(t) =>
  switch (d) {
  | Left => move_by_char_left(z)
  | Right => move_by_char_right(z)
  };

let move_by_token = (d: Direction.t, z: t): option(t) =>
  switch (z.caret) {
  | Outer => move(d, z)
  | Inner(_) =>
    let z = Caret.set(Outer, z);
    switch (d) {
    | Left => Some(z)
    | Right => move(Right, z)
    };
  };

let primary =
    (chunkiness: Action.chunkiness, d: Direction.t, z: t): option(t) => {
  let z = unselect(z);
  switch (chunkiness) {
  | ByToken => move_by_token(d, z)
  | ByChar => move_by_char(d, z)
  };
};

/* Do move_action until the predicate on the generalized neigbors of the
   caret becomes true. A generalized neighbor is the neighboring piece, unless
   the neighbor is a polytile, in which case it's the relevant shard, or
   we are at the edge of a segment, in which case it's the relevant shard
   of the parent. The None case strictly means the beginning/end of the program.
   If no such piece is found, don't move. Does not check predicate before
   moving; caller should handle that case if necessary */
let rec do_until =
        (
          move_action: t => option(t),
          piece_p: ((option(Piece.t), option(Piece.t))) => bool,
          z: t,
        )
        : option(t) => {
  let* z = move_action(z);
  if (piece_p(Zipper.generalized_neighbors(z))) {
    Some(z);
  } else {
    do_until(move_action, piece_p, z);
  };
};

let do_to_extreme = (f, z: t): t =>
  do_until(
    f,
    neighbors =>
      switch (neighbors) {
      | (None, _) => true
      | (_, None) => true
      | _ => false
      },
    z,
  )
  |> Option.value(~default=z);

/* Do move_action until the indicated piece is such that piece_p is true,
   restarting from the beginning/end if not found in forward direction.
   If no such piece is found, don't move. */
let move_until_wrap = (p, d, z) =>
  switch (do_until(primary(ByToken, d), p, z)) {
  | None =>
    let z = do_to_extreme(primary(ByToken, Direction.toggle(d)), z);
    do_until(primary(ByToken, d), p, z);
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
  let z = do_to_extreme(primary(ByToken, d), z);
  at_piece(Zipper.generalized_neighbors(z))
    ? Some(z)
    : do_until(primary(ByToken, Direction.toggle(d)), at_piece, z);
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

let linebreak_on =
    (d: Direction.t, neighbors: (option(Piece.t), option(Piece.t))) =>
  switch (neighbors) {
  | (_, Some(Secondary(s))) when d == Right && Secondary.is_linebreak(s) =>
    true
  | (_, None) when d == Right => true
  | (Some(Secondary(s)), _) when d == Left && Secondary.is_linebreak(s) =>
    true
  | (None, _) when d == Left => true
  | _ => false
  };

let do_until_linebreak = (f, d, z) =>
  linebreak_on(d, Zipper.generalized_neighbors(z))
    ? Some(z) : do_until(f, linebreak_on(d), z);

let do_towards_goal =
    (
      ~anchor: option(Measured.Point.t)=?,
      ~measured: Measured.t,
      ~force_progress: bool=false,
      f: (Direction.t, t) => option(t),
      goal: Measured.Point.t,
      z: t,
    )
    : option(t) => {
  let caret_point = Zipper.Caret.point(measured);

  let is_at_side_of_row = (d: Direction.t, z: Zipper.t) => {
    let Point.{row, col} = caret_point(z);
    switch (Zipper.move(d, z)) {
    | None => true
    | Some(z) =>
      let Point.{row: rowp, col: colp} = caret_point(z);
      row != rowp || col == colp;
    };
  };

  let direction_to_from = (p1: Point.t, p2: Point.t): Direction.t => {
    let before_row = p1.row < p2.row;
    let at_row = p1.row == p2.row;
    let before_col = p1.col < p2.col;
    before_row || at_row && before_col ? Left : Right;
  };

  let closer_to_prev = (curr, prev, goal: Point.t) =>
    /* Default to true if equal */
    abs(caret_point(prev).col - goal.col)
    < abs(caret_point(curr).col - goal.col);

  let init = caret_point(z);
  let d_to_goal = direction_to_from(goal, init);
  let rec go = (prev: t, curr: t) => {
    let curr_p = caret_point(curr);
    let x_progress = Point.dcomp(d_to_goal, curr_p.col, goal.col);
    let y_progress = Point.dcomp(d_to_goal, curr_p.row, goal.row);
    switch (y_progress, x_progress) {
    /* If we're not there yet, keep going */
    | (Under, Over | Exact | Under)
    | (Exact, Under) =>
      switch (f(d_to_goal, curr)) {
      | Some(next) => go(curr, next)
      | None => curr /* Should only occur at start/end of program */
      }
    /* If we're there, stop */
    | (Exact, Exact) => curr
    /* If we've overshot, meaning the exact goal is inaccessible,
     * we choose between current and previous (undershot) positions */
    | (Over, Over | Exact | Under) =>
      switch (force_progress) {
      /* Ideally we would use the same logic as from the below
       * anchor case here; however that results in strange
       * behavior when accidentally starting a drag at the end
       * of a line, which triggers the (invisible) selection of
       * a linebreak, making it appear that the caret has jumped
       * to the next line. The downside of leaving this as-is is
       * that multiline tokens (projectors) do not become part of
       * the selection when dragging until you're all the way
       * over them, which is slightly visually jarring */
      | false => prev
      /* Up/down kb movement works by setting a goal one row
       * below the current. When adjacent to a multiline token,
       * the nearest next caret position may be multiple lines down.
       * We must allow this overshoot in order to make progress. */
      | true => caret_point(prev) == init ? curr : prev
      }
    | (Exact, Over) =>
      switch (anchor) {
      | None =>
        /* If you're trying to (eg) move down at the end of a row
         * but the first position of the next row is further right
         * than the currentrow's end, we want to make progress
         * regardless of whether the new position would be closer
         * or further from the goal.  Otherwise, we try to just
         * get as close as we can  */
        is_at_side_of_row(Direction.toggle(d_to_goal), curr)
          ? curr : closer_to_prev(curr, prev, goal) ? prev : curr
      | Some(anchor) =>
        /* If we're dragging to make a selection, decide whether or
         * not to force progress based on the relative position of the
         * anchor (the position where the drag was started) */
        direction_to_from(goal, anchor) == d_to_goal ? curr : prev
      }
    };
  };
  let res = go(z, z);
  Measured.Point.equals(caret_point(res), caret_point(z))
    ? None : Some(res);
};

let vertical =
    (~col_target: int, ~measured: Measured.t, d: Action.vertical, z: t)
    : option(t) => {
  /* Here f should be a function which results in strict d-wards
     movement of the caret. Iterate f until we get to the closet
     caret position to a target derived from the initial position */
  let caret_point = Zipper.Caret.point(measured);
  let goal =
    Point.{
      col: col_target,
      row: caret_point(z).row + (d == Down ? 1 : (-1)),
    };
  do_towards_goal(~force_progress=true, ~measured, primary(ByChar), goal, z);
};

let to_point = (~measured: Measured.t, ~goal: Point.t, z: t): option(t) =>
  switch (do_towards_goal(~measured, primary(ByChar), goal, z)) {
  | None => Some(z)
  | Some(z) => Some(z)
  };

let to_start: Zipper.t => Zipper.t = do_to_extreme(primary(ByToken, Left));

let to_end: Zipper.t => Zipper.t = do_to_extreme(primary(ByToken, Right));

let to_linebreak: (Direction.t, Zipper.t) => option(Zipper.t) =
  d => do_until_linebreak(primary(ByToken, d), d);

let move_dispatch =
    (
      ~info_map: Language.Statics.Map.t,
      ~col_target: int,
      ~measured: Measured.t,
      d: Action.move,
      z: Zipper.t,
    )
    : option(Zipper.t) =>
  switch (d) {
  | Local(d, chunk) => primary(chunk, d, z)
  | Start => Some(to_start(z))
  | End => Some(to_end(z))
  | Line(d) => to_linebreak(d, z)
  | Vertical(d) => vertical(~measured, ~col_target, d, z)
  | Point(goal) => to_point(~measured, ~goal, z)
  | Goal(Hole(d)) => to_next_grout(d, z)
  | Goal(TileId(id)) => jump_to_id_indicated(z, id)
  | Goal(BindingSiteOfIndicatedVar) =>
    let* ci = Indicated.ci_of(z, info_map);
    let* binding_id = Language.Info.get_binding_site(ci);
    jump_to_id_indicated(z, binding_id);
  };

let pre_unselect = (a: Action.move, z: Zipper.t): Zipper.t => {
  let d =
    switch (a) {
    | Local(d, _) => d
    | Vertical(Up) => Left
    | Vertical(Down) => Right
    | Start
    | End
    | Line(_)
    | Point(_)
    | Goal(_) => z.selection.focus
    };
  Zipper.directional_unselect(d, z);
};
let go =
    (
      ~info_map: Language.Statics.Map.t,
      ~col_target: int,
      ~measured: Measured.t,
      a: Action.move,
      z: Zipper.t,
    )
    : option(Zipper.t) =>
  if (Selection.is_empty(z.selection)) {
    move_dispatch(~info_map, ~col_target, ~measured, a, z);
  } else {
    let z = pre_unselect(a, z);
    switch (a) {
    // By char just unselects
    | Local(Left, ByChar)
    | Local(Right, ByChar) => Some(z)
    | _ =>
      switch (move_dispatch(~info_map, ~col_target, ~measured, a, z)) {
      | Some(z) => Some(z)
      /* Always empty selection on move action,
       * even if we don't actually move */
      | None => Some(z)
      }
    };
  };

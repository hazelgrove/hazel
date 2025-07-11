open Zipper;
open Util;
open OptUtil.Syntax;

/* A label and an index into that label, representing a delimiter */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type indexed = option((int, Label.t));

/* The index of a delimiter and its maximum internal caret position */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type token_info = option((int, int));

let piece_neighbor = (d: Direction.t, p: Piece.t): indexed =>
  switch (p) {
  | Tile(t) => Some((Tile.shard_on_side(Direction.toggle(d), t), t.label))
  | Secondary(w) => Some((0, [Secondary.get_string(w.content)]))
  | Grout(_) => None
  | Projector(_) => None
  };

let ancestor_neighbor = (d: Direction.t, ancestors: Ancestors.t): indexed => {
  let+ {shards: (l, r), label, _} = Ancestors.parent(ancestors);
  switch (d) {
  | Left => (ListUtil.last(l), label)
  | Right => (List.hd(r), label)
  };
};

let indexes = ((delim_idx: int, label: Label.t)): token_info => {
  assert(delim_idx < List.length(label));
  let char_max = Token.length(List.nth(label, delim_idx)) - 2;
  char_max < 0 ? None : Some((delim_idx, char_max));
};

let nhbr = (d: Direction.t, r: Relatives.t): indexed =>
  switch (Siblings.neighbor(d, r.siblings)) {
  | Some(p) => piece_neighbor(d, p)
  | None => ancestor_neighbor(d, r.ancestors)
  };

let move_by_char_left = (z: t): option(t) =>
  switch (z.caret, Option.bind(nhbr(Left, z.relatives), indexes)) {
  | (Outer, None) => z |> move(Left)
  | (Outer, Some((delim_init, char_max))) =>
    z |> set_caret(Inner(delim_init, char_max)) |> move(Left)
  | (Inner(_, char), None | Some((_, _))) when char == 0 =>
    z |> set_caret(Outer) |> Option.some
  | (Inner(delim, char), None | Some(_)) =>
    z |> set_caret(Inner(delim, char - 1)) |> Option.some
  };

let move_by_char_right = (z: t): option(t) =>
  switch (z.caret, Option.bind(nhbr(Right, z.relatives), indexes)) {
  | (Outer, None) => z |> move(Right)
  | (Outer, Some((delim_init, _))) =>
    z |> set_caret(Inner(delim_init, 0)) |> Option.some
  | (Inner(_, char), Some((_, char_max))) when char == char_max =>
    z |> set_caret(Outer) |> move(Right)
  | (Inner(delim, char), None | Some(_)) =>
    z |> set_caret(Inner(delim, char + 1)) |> Option.some
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
    let z = set_caret(Outer, z);
    switch (d) {
    | Left => Some(z)
    | Right => move(Right, z)
    };
  };

let primary = (chunkiness: chunkiness, d: Direction.t, z: t): option(t) => {
  let z = unselect(z);
  switch (chunkiness) {
  | ByToken => move_by_token(d, z)
  | ByChar => move_by_char(d, z)
  };
};

module type S = {
  let measured: Measured.t;
  let term_ranges: TermRanges.t;
  let col_target: int;
};

module Make = (M: S) => {
  let caret_point = Zipper.caret_point(M.measured);
  let primary = primary;
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

  let do_towards =
      (
        ~anchor: option(Measured.Point.t)=?,
        ~force_progress: bool=false,
        f: (Direction.t, t) => option(t),
        goal: Measured.Point.t,
        z: t,
      )
      : option(t) => {
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
  let do_vertical =
      (f: (Direction.t, t) => option(t), d: Direction.t, z: t): option(t) => {
    /* Here f should be a function which results in strict d-wards
       movement of the caret. Iterate f until we get to the closet
       caret position to a target derived from the initial position */
    let cur_p = caret_point(z);
    let goal =
      Point.{
        col: M.col_target,
        row: cur_p.row + (d == Right ? 1 : (-1)),
      };
    do_towards(~force_progress=true, f, goal, z);
  };

  let do_extreme =
      (f: (Direction.t, t) => option(t), d: planar, z: t): option(t) => {
    let cur_p = caret_point(z);
    let goal: Point.t =
      switch (d) {
      | Right(_) => {
          col: Int.max_int,
          row: cur_p.row,
        }
      | Left(_) => {
          col: 0,
          row: cur_p.row,
        }
      | Up => {
          col: 0,
          row: 0,
        }
      | Down => {
          col: Int.max_int,
          row: Int.max_int,
        }
      };
    do_towards(f, goal, z);
  };

  let to_start = do_extreme(primary(ByToken), Up);
  let to_end = do_extreme(primary(ByToken), Down);

  let to_edge: (Direction.t, t) => option(t) =
    fun
    | Left => to_start
    | Right => to_end;

  /* Do move_action until the indicated piece is such that piece_p is true.
     If no such piece is found, don't move. */
  let rec do_until =
          (
            ~move_first=true,
            move_action: t => option(t),
            piece_p: Piece.t => bool,
            z: t,
          )
          : option(t) => {
    let* z = move_first ? move_action(z) : Some(z);
    let* (piece, _, _) = Indicated.piece'(~no_ws=false, ~ign=_ => false, z);
    if (piece_p(piece)) {
      Some(z);
    } else {
      let* z = move_first ? Some(z) : move_action(z);
      do_until(~move_first, move_action, piece_p, z);
    };
  };

  /* Do move_action until the indicated piece is such that piece_p is true,
     restarting from the beginning/end if not found in forward direction.
     If no such piece is found, don't move. */
  let do_until_wrap = (p, d, z) =>
    switch (do_until(primary(ByToken, d), p, z)) {
    | None =>
      let* z = to_edge(Direction.toggle(d), z);
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
    let jump_to_left_of_id = (z: t, id: Id.t): option(t) => {
      let* {origin, _} = Measured.find_by_id(id, M.measured);
      let z =
        switch (to_start(z)) {
        | None => z
        | Some(z) => z
        };
      switch (do_towards(primary(ByChar), origin, z)) {
      | None => Some(z)
      | Some(z) => Some(z)
      };
    };
    let+ z = jump_to_left_of_id(z, id);
    switch (d) {
    | Left => z
    | Right =>
      switch (primary(ByToken, Right, z)) {
      | Some(z) => z
      | None => z
      }
    };
  };

  /* Moves to the left side of the token with the given id,
   * then checks if it's indicated. If not, move one token
   * to the right. I believe but have not proved this
   * always results in the token being indicated  */
  let jump_to_id_indicated = (z: t, id: Id.t): option(t) => {
    let* {origin, _} = Measured.find_by_id(id, M.measured);
    let z =
      switch (to_start(z)) {
      | None => z
      | Some(z) => z
      };
    switch (do_towards(primary(ByChar), origin, z)) {
    | None => Some(z)
    | Some(z) =>
      switch (Indicated.index(z)) {
      | Some(indicated_id) when id == indicated_id => Some(z)
      | _ =>
        switch (primary(ByToken, Right, z)) {
        | Some(z) => Some(z)
        | None => Some(z)
        }
      }
    };
  };

  let vertical = (d: Direction.t, z: t): option(t) =>
    z.selection.content == []
      ? do_vertical(primary(ByChar), d, z)
      : Some(Zipper.directional_unselect(d, z));

  let targets_within_row = (z: t): list(t) => {
    let init = caret_point(z);
    let rec go = (d: Direction.t, z: t) => {
      switch (primary(ByChar, d, z)) {
      | None => []
      | Some(z) =>
        if (caret_point(z).row != init.row) {
          [];
        } else {
          switch (pop_backpack(z)) {
          | None => go(d, z)
          | Some(_) => [z, ...go(d, z)]
          };
        }
      };
    };
    let curr =
      switch (pop_backpack(z)) {
      | None => []
      | Some(_) => [z]
      };
    List.rev(go(Left, z)) @ curr @ go(Right, z);
  };

  // TODO(d): unify this logic with rest of movement logic
  let rec to_backpack_target = (d: planar, z: t): option(t) => {
    let done_or_try_again = (d, z) =>
      switch (pop_backpack(z)) {
      | None => to_backpack_target(d, z)
      | Some(_) => Some(z)
      };
    switch (d) {
    | Left(chunk) =>
      let* z = primary(chunk, Left, z);
      done_or_try_again(d, z);
    | Right(chunk) =>
      let* z = primary(chunk, Right, z);
      done_or_try_again(d, z);
    | Up =>
      let* z = vertical(Left, z);
      let zs =
        targets_within_row(z)
        |> List.sort((z1, z2) => {
             let dist1 = caret_point(z1).col - M.col_target;
             let dist2 = caret_point(z2).col - M.col_target;
             let c = Int.compare(abs(dist1), abs(dist2));
             // favor left
             c != 0 ? c : Int.compare(dist1, dist2);
           });
      switch (zs) {
      | [] => to_backpack_target(d, z)
      | [z, ..._] => Some(z)
      };
    | Down =>
      let* z = vertical(Right, z);
      let zs =
        targets_within_row(z)
        |> List.sort((z1, z2) => {
             let dist1 = caret_point(z1).col - M.col_target;
             let dist2 = caret_point(z2).col - M.col_target;
             let c = Int.compare(abs(dist1), abs(dist2));
             // favor right
             c != 0 ? c : - Int.compare(dist1, dist2);
           });
      switch (zs) {
      | [] => to_backpack_target(d, z)
      | [z, ..._] => Some(z)
      };
    };
  };

  let move_dispatch = (d: Action.move, z: Zipper.t): option(Zipper.t) =>
    switch (d) {
    | Goal(Piece(p, d)) => do_until_wrap(Action.of_piece_goal(p), d, z)
    | Goal(Point(goal)) =>
      switch (do_towards(primary(ByChar), goal, z)) {
      | None => Some(z)
      | Some(z) => Some(z)
      }
    | Extreme(d) => do_extreme(primary(ByToken), d, z)
    | Local(d) =>
      z
      |> (
        switch (d) {
        | Left(chunk) => primary(chunk, Left)
        | Right(chunk) => primary(chunk, Right)
        | Up => vertical(Left)
        | Down => vertical(Right)
        }
      )
    };

  let go = (d: Action.move, z: Zipper.t): option(Zipper.t) =>
    if (Selection.is_empty(z.selection)) {
      move_dispatch(d, z);
    } else {
      /* Always empty selection on move action,
       * even if we don't actually move */
      switch (d) {
      | Local(Left(ByChar)) => Some(Zipper.directional_unselect(Left, z))
      | Local(Right(ByChar)) => Some(Zipper.directional_unselect(Right, z))
      | Local(Left(ByToken)) =>
        let z = Zipper.directional_unselect(Left, z);
        switch (move_dispatch(d, z)) {
        | Some(z) => Some(z)
        | None => Some(z)
        };
      | Local(Right(ByToken)) =>
        let z = Zipper.directional_unselect(Right, z);
        switch (move_dispatch(d, z)) {
        | Some(z) => Some(z)
        | None => Some(z)
        };
      | _ =>
        let z = Zipper.directional_unselect(z.selection.focus, z);
        switch (move_dispatch(d, z)) {
        | Some(z) => Some(z)
        | None => Some(z)
        };
      };
    };

  let left_until_case_or_rule =
    do_until(go(Local(Left(ByToken))), Piece.is_case_or_rule);

  let left_until_not_comment_or_space = (~move_first) =>
    do_until(
      ~move_first,
      go(Local(Left(ByToken))),
      Piece.not_comment_or_space,
    );
};

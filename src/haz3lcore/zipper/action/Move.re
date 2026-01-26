open Util;
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
  | (Inner(char), Some(max_idx)) when char == max_idx =>
    z |> Caret.set(Outer) |> move(Right)
  | (Inner(char), None | Some(_)) =>
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
  | ByChar => by_char(d, z)
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

let move_dispatch =
    (
      ~statics: Language.Statics.Map.t,
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
  | Vertical(d) => vertical(~measured, ~col_target, d, z)
  | Point(goal) => to_point(~measured, ~goal, z)
  | Goal(Hole(d)) => to_next_grout(d, z)
  // | Goal(TileId(id)) => jump_to_id_indicated(z, id)
  | Goal(BindingSiteOfIndicatedVar) =>
    let* ci = Indicated.ci_of(z, statics);
    let* binding_id = Language.Info.get_binding_site(ci);
    jump_to_id_indicated(z, binding_id);
  };

let pre_unselect = (a: Action.move, z: t): t => {
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
      ~statics: Language.Statics.Map.t,
      ~col_target: int,
      ~measured: Measured.t,
      a: Action.move,
      z: t,
    )
    : option(t) =>
  if (Selection.is_empty(z.selection)) {
    move_dispatch(~statics, ~col_target, ~measured, a, z);
  } else {
    let z = pre_unselect(a, z);
    switch (a) {
    // By char just unselects
    | Local(Left, ByChar)
    | Local(Right, ByChar) => Some(z)
    | _ =>
      switch (move_dispatch(~statics, ~col_target, ~measured, a, z)) {
      | Some(z) => Some(z)
      /* Always empty selection on move action,
       * even if we don't actually move */
      | None => Some(z)
      }
    };
  };

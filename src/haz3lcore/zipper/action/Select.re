open Util;
open OptUtil.Syntax;

module Make = (M: Editor.Meta.S) => {
  module Move = Move.Make(M);

  let primary = (d: Direction.t, z: Zipper.t): option(Zipper.t) =>
    if (z.caret == Outer) {
      Zipper.select(d, z);
    } else if (d == Left) {
      z
      |> Zipper.set_caret(Outer)
      |> Zipper.move(Right)
      |> OptUtil.and_then(Zipper.select(d));
    } else {
      z |> Zipper.set_caret(Outer) |> Zipper.select(d);
    };

  let vertical = (d: Direction.t, ed: Zipper.t): option(Zipper.t) =>
    Move.do_vertical(primary, d, ed);

  let range = (l: Id.t, r: Id.t, z: Zipper.t): option(Zipper.t) => {
    let* z = Move.jump_to_id(z, l);
    let* Measured.{last, _} = Measured.find_by_id(r, M.measured);
    Move.do_towards(Zipper.select, last, z);
  };

  let term = (id: Id.t, z: Zipper.t): option(Zipper.t) => {
    //TODO: check if selection is already a term: no-op in this case
    let* (l, r) = TermRanges.find_opt(id, M.term_ranges);
    range(Piece.id(l), Piece.id(r), z);
  };

  let tile = (id: Id.t, z: Zipper.t): option(Zipper.t) => {
    let* z = Move.jump_to_id(z, id);
    let* Measured.{last, _} = Measured.find_by_id(id, M.measured);
    Move.do_towards(primary, last, z);
  };

  let current_term = z => {
    let* id = Indicated.index(z);
    term(id, z);
  };

  let current_tile = z => {
    let* id = Indicated.index(z);
    tile(id, z);
  };

  let go = (d: Action.move, z: Zipper.t) =>
    switch (d) {
    | Goal(Piece(_)) => failwith("Select.go not implemented for Piece Goal")
    | Goal(Point(goal)) =>
      let anchor = z |> Zipper.toggle_focus |> Zipper.caret_point(M.measured);
      Move.do_towards(~anchor, primary, goal, z);
    | Extreme(d) => Move.do_extreme(primary, d, z)
    | Local(d) =>
      /* Note: Don't update target on vertical selection */
      switch (d) {
      | Left(_) => primary(Left, z)
      | Right(_) => primary(Right, z)
      | Up => vertical(Left, z)
      | Down => vertical(Right, z)
      }
    };

  let grow_right_until_case_or_rule =
    Move.do_until(go(Local(Right(ByToken))), Piece.is_case_or_rule);

  let grow_right_until_not_comment_or_space =
    Move.do_until(go(Local(Right(ByToken))), Piece.not_comment_or_space);

  let shrink_left_until_not_case_or_rule_or_space =
    Move.do_until(
      go(Local(Left(ByToken))),
      Piece.is_not_case_or_rule_or_space,
    );

  let containing_rule = z => {
    let* z = current_tile(z);
    let* z = grow_right_until_case_or_rule(z);
    shrink_left_until_not_case_or_rule_or_space(z);
  };

  let containing_secondary_run = z => {
    let* z = Move.left_until_not_comment_or_space(z);
    let* z = grow_right_until_not_comment_or_space(z);
    go(Local(Left(ByToken)), z);
  };
};

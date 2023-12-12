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
    Move.do_towards(primary, last, z);
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

  let go = (d: Action.move, z: Zipper.t) =>
    switch (d) {
    | Goal(Piece(_)) => failwith("Select.go not implemented for Piece Goal")
    | Goal(Point(goal)) =>
      let anchor =
        {...z, selection: Selection.toggle_focus(z.selection)}
        |> Zipper.caret_point(M.measured);
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
};

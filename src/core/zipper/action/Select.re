open Util;

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

  let go = (d: Action.move, z: Zipper.t) =>
    switch (d) {
    | Goal(goal) =>
      let anchor =
        {...z, selection: Selection.toggle_focus(z.selection)}
        |> Zipper.caret_point(M.measured);
      Move.do_towards(~anchor, primary, goal, z);
    // |> Option.map(update_target);
    | Extreme(d) => Move.do_extreme(primary, d, z)
    // |> Option.map(update_target)
    | Local(d) =>
      /* Note: Don't update target on vertical selection */
      switch (d) {
      | Left(_) => primary(Left, z) // |> Option.map(update_target)
      | Right(_) => primary(Right, z) // |> Option.map(update_target)
      | Up => vertical(Left, z)
      | Down => vertical(Right, z)
      }
    };
};

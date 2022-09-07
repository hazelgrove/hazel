open Util;

module Make = (M: Editor.Meta.S) => {
  module Move = Move.Make(M);

  let primary = (d: Direction.t, z: Zipper.t, id_gen): option(Zipper.state) =>
    if (z.caret == Outer) {
      Zipper.select(d, z) |> Option.map(z => (z, id_gen));
    } else if (d == Left) {
      let z = z |> Zipper.set_caret(Outer);
      Zipper.move(Right, z, id_gen)
      |> OptUtil.and_then(((z, id_gen)) =>
           Zipper.select(d, z) |> Option.map(z => (z, id_gen))
         );
    } else {
      z
      |> Zipper.set_caret(Outer)
      |> Zipper.select(d)
      |> Option.map(z => (z, id_gen));
    };

  let vertical = (d: Direction.t, ed: Zipper.t, id_gen): option(Zipper.state) =>
    Move.do_vertical(primary, d, ed, id_gen);

  let go = (d: Action.move, z: Zipper.t, id_gen) =>
    switch (d) {
    | Goal(goal) =>
      let anchor =
        {...z, selection: Selection.toggle_focus(z.selection)}
        |> Zipper.caret_point(M.measured);
      Move.do_towards(~anchor, primary, goal, z, id_gen);
    | Extreme(d) => Move.do_extreme(primary, d, z, id_gen)
    | Local(d) =>
      let f =
        switch (d) {
        | Left(_) => primary(Left)
        | Right(_) => primary(Right)
        | Up => vertical(Left)
        | Down => vertical(Right)
        };
      f(z, id_gen);
    };
};

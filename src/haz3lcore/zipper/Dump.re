/* Try to complete the syntax to give better semantic feeback.
 * This is a best-effort approach focussed on adding new definitions
 * as opposed to restructuring; it does not complete the syntax in
 * all cases.
 *
 * NOTE: Setting the caret to outer was necessary to 'get it past'
 * string literals, i.e. offer live feeback when typing inside a
 * string; not sure if this is a hack or not, it may be compensating
 * for the put_down logic not working right with string lits. To test,
 * try to look at live evaluation while typing inside a string lit with
 * stuff left to drop in backpack with below set: Outer disabled. */
let to_zipper = (z: Zipper.t, ~root) =>
  if (!Selection.is_empty(z.selection)) {
    z;
  } else {
    let z = {
      ...z,
      caret: Outer,
    };
    let rec move_until_cant_put_down = (z_last, z: Zipper.t) =>
      if (Zipper.can_put_down(z) && !Zipper.is_linebreak_to_right_of_caret(z)) {
        switch (Zipper.move(Right, z)) {
        | None => z
        | Some(z_new) => move_until_cant_put_down(z, z_new)
        };
      } else if (Zipper.can_put_down(z)
                 && Zipper.is_linebreak_to_right_of_caret(z)) {
        switch (Zipper.move(Right, z)) {
        | None => z
        | Some(z_new) => z_new
        };
      } else {
        z_last;
      };
    let rec move_until_can_put_down = (z: Zipper.t) =>
      if (!Zipper.can_put_down(z)) {
        switch (Zipper.move(Right, z)) {
        | None => z
        | Some(z_new) => move_until_can_put_down(z_new)
        };
      } else {
        z;
      };
    let rec put_down_as_much_as_possible = (z: Zipper.t): Zipper.t => {
      switch (Zipper.put_down(z, ~root)) {
      | None => z
      | Some(z) => put_down_as_much_as_possible(z)
      };
    };
    let rec go = (z: Zipper.t): Zipper.t => {
      let z_can = Zipper.can_put_down(z) ? z : move_until_can_put_down(z);
      let z_cant = move_until_cant_put_down(z_can, z_can);
      let z = put_down_as_much_as_possible(z_cant);
      if (Zipper.local_backpack(z) == []) {
        z;
      } else {
        go(z);
      };
    };
    go(z);
  };

let to_segment_by_walking = (z: Zipper.t, ~root): Segment.t =>
  z
  |> Zipper.clear_unparsed_buffer
  |> to_zipper(~root)
  |> Zipper.unselect_and_zip(~erase_buffer=true);

/* Every nested segment counts: a tile's children, and a projector's syntax
 * and a splice's content, which Segment.global_missing_shards does not
 * enter (a `[` typed in a table cell is one). */
let rec has_incomplete_tile = (seg: Segment.t): bool =>
  List.exists(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        !Tile.is_complete(t) || List.exists(has_incomplete_tile, t.children)
      | Projector(pr) => has_incomplete_tile(pr.syntax)
      | Splice(s) => has_incomplete_tile(s.content)
      | Grout(_)
      | Secondary(_) => false
      },
    seg,
  );

/* The walk only ever puts down a missing shard, and can_put_down is false
 * wherever none is missing, so with no incomplete tile anywhere it moves
 * the caret to the end of the program and puts nothing down: the answer
 * is the program as it stands. That is the usual case, and the walk is a
 * Zipper.move per piece -- ~300 ms of every statics pass on the Color
 * slide -- so check for it first. */
let to_segment = (z: Zipper.t, ~root): Segment.t => {
  let zipped =
    z
    |> Zipper.clear_unparsed_buffer
    |> Zipper.unselect_and_zip(~erase_buffer=true);
  has_incomplete_tile(zipped) ? to_segment_by_walking(z, ~root) : zipped;
};

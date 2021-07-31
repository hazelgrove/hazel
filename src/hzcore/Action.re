open OptUtil.Syntax;

type construct_shape =
  | Newline
  | Text(string)
  | Shard(Shard.t);

type t =
  // TODO Mark
  | Move(Path.t)
  | Delete({
      dir: Direction.t,
      crossing_whitespace: bool,
    })
  | Construct(construct_shape);

module Error = {
  type t = unit;
};

module Success = {
  type t = (EditState.t, list(Effect.t));
};

let rec perform =
    (a: t, (cursor, term): EditState.t)
    : IdGenCmd.t(Result.t(Success.t, Error.t)) => {
  open IdGenCmd.Syntax;

  let move = path => {
    let edit_state = EditState.mk(Pointing(path), term);
    let effects = [Effect.Move(path)];
    IdGenCmd.return(Ok((edit_state, effects)));
  };

  switch (a) {
  | Move(path) => move(path)

  | Delete({dir, into_whitespace}) =>
    if (into_whitespace) {
      move(Path.next(dir, path, term));
    } else {
      let (subject, frame) = Path.to_zipper(cursor, term);
      let Pointing(path) = cursor;
      switch (subject) {
      | Pointing(ztile, (prefix, suffix)) =>
        let (child_step, caret_step, tile) = ztile;
        switch (Tile.is_text_lit(tile)) {
        | Some(s) =>
          failwith("todo")
        | None =>
          if (Path.escapes(dir, path, term)) {
            // unregistered move
            let next_path = Path.next(dir, path, term);
            let edit_state = EditState.mk(Pointing(next_path), term);
            perform(Delete({dir, into_whitespace: false}), edit_state);
          } else {
            let id = Tile.id(tile);
            let (children_pre, children_suf) = ZTile.same_sort_children(ztile);
            let* (prefix, suffix) =
              Parser.fix_holes(ltip, children_pre @ [prefix], children_suf @ [suffix], rtip);
            let (ztile, (prefix, suffix)) =
              switch (prefix, suffix) {
              | (_, [tile, ...suffix]) =>
                (ZTile.cursor_before(tile), (prefix, suffix))
              | ([tile, ...prefix], _) =>
                (ZTile.cursor_after(tile), (prefix, suffix))
              | ([], []) =>
                failwith("todo")
              }
          }
        }
      }
    };

  | Construct(shape) =>
    let (subject, frame) = Path.to_zipper(cursor, term);
    switch (subject) {
    | Pointing(ztile, (prefix, suffix)) =>
      // should be able to customize on a per shard(?) basis
      // how much to wrap, eg let line default should wrap up
      // to nearest line, while...
      // well I was gonna say parentheses should only default
      // wrap nearest operand but these are gonna enter
      // restructuring anyway... so the default only matters
      // for the

      // if cursor before ztile
      //   put any matching shards to the right on the other side of ztile
      // else if cursor after ztile
      //   put any matching shards to the left on the other side of ztile
      // else (cursor on the interior of ztile)
      //   silently move cursor and re-perform construct
    }

  };
};
open OptUtil.Syntax;

type t =
  // TODO Mark
  | Move(Path.t)
  | Delete({
      dir: Direction.t,
      into_whitespace: bool,
    })
  | Construct(unit); // TODO

let perform = (a: t, (cursor, term): EditState.t): option(EditState.t) =>
  switch (a) {
  | Move(path) =>
    let cursor =
      switch (cursor) {
      | Pointing(_) => Pointing(path)
      };
    Some(EditState.mk(cursor, term));
  | Delete({dir, into_whitespace}) =>
    if (into_whitespace) {
      // handle by moving or whatever
    } else {
      let (subject, frame) = Path.to_zipper(cursor, term);
      switch (subject) {
      | Pointing(ztile, (prefix, suffix)) =>
        let (child_step, caret_step, tile) = ztile;
        // check tile
        // if it's a text token, then do the regular text operation
        // otherwise, `let (pre, suf) = ZTile.get_same_sort_children(ztile)`

        // `let (prefix, suffix) = Parser.fix_holes(ltip, prefix, suffix, rtip)`
        //



        switch (tile) {
        | Exp(OpText(text)) =>
          let+ text = OpText_exp.delete(d, caret_step, text);
          // TODO handle empty result

        }

        let+ ((cursor, tile), (ts_pre, ts_suf)) = ZTile.(d, ztile);
        let ()

      }
    } else {
    };
  };
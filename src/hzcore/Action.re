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

  | Delete({dir, into_whitespace: true}) =>
    move(Path.next(dir, path, term))

  | Delete({dir, into_whitespace: false}) =>
    if (Path.escapes(dir, path, term)) {
      // unregistered move
      let next_path = Path.next(dir, path, term);
      let edit_state = EditState.mk(Pointing(next_path), term);
      perform(Delete({dir, into_whitespace: false}), edit_state);
    } else {
      let (subject, frame) = Path.to_zipper(cursor, term);
      let frame_sort = Frame.sort(frame);
      let Pointing((_, caret) as path) = cursor;
      let Pointing({
        current_shard,
        rest_of_tile,
        rest_of_subj: (prefix, suffix),
      }) = subject;
      switch (Shard.is_text_lit(current_shard)) {
      | Some(s) =>
        switch (StringUtil.delete(dir, caret, s)) {
        | None =>
          // caret is at end of text lit
          // perform unregistered move and try again
          let next = Path.next(dir, path, term);
          perform(a, (Pointing(next), term));
        | Some((caret, s)) =>
          // tips should remain unaltered, no need to fix
          let current_shard = Shard.mk_text(frame_sort, s);
          let subject = Subject.Pointing({current_shard, rest_of_tile, rest_of_subj});
          Path.of_zipper((subject, frame));
        }
      | None =>
        // remove current_shard and filter tiles in rest_of_tile
        let (same_sort_pre, same_sort_suf) =
          rest_of_tile
          |> AltListFrame.filter_map_a(
            _shard => None,
            term =>
              Term.sort(term) == frame_sort
              ? Some(term)
              : None,
          );
        let (same_sort_pre, same_sort_suf) =
          (
            List.map(term => List.rev(Term.flatten(term)), same_sort_pre),
            List.map(Term.flatten, same_sort_suf),
          );
        // fix holes between remaining segments and prefix/suffix
        let (prefix, suffix) =
          Parser.fix_tips(
            List.flatten(same_sort_pre) @ prefix,
            List.flatten(same_sort_suf) @ suffix,
          );
        let (current_shard, rest_of_tile, rest_of_subj) =
          switch (prefix, suffix) {
          | ([], []) => failwith("fix_tips returned empty frame")
          | (_, [tile, ...suffix]) =>
            let (shard, tile) = enter(Left, tile);
            (shard, tile, (prefix, suffix));
          | ([tile, ...prefix], _) =>
            let (shard, tile) = enter(Right, tile);
            (shard, tile, (prefix, suffix));
          };
        let subject = Subject.Pointing({current_shard, rest_of_tile, rest_of_subj});
        Path.of_zipper((subject, frame));
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
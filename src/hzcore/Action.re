open OptUtil.Syntax;

type t =
  // TODO Mark
  | Move(Path.t)
  | Delete(Direction.t)
  | Construct(unit); // TODO

let perform = (a: t, (cursor, term): EditState.t): option(EditState.t) =>
  switch (a) {
  | Move(path) =>
    let cursor =
      switch (cursor) {
      | Pointing(_) => Pointing(path)
      };
    Some((cursor, term));
  | Delete(d) =>
    let (subject, frame) = Path.to_zipper(cursor, term);
    switch (subject) {
    | Pointing(ztile, (prefix, suffix)) =>
      let+ remaining = ZTile.delete(d, ztile);

    }
  }
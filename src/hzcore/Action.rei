type t =
  // TODO Mark
  | Move(Path.t)
  | Delete({
      dir: Direction.t,
      into_layout_whitespace: bool,
    })
  | Construct(unit); // TODO

module Error = {
  type t = unit;
};

let perform:
  (t, EditState.t)
  => Result.t(
    list((EditState.t, HistoryEntry.t)),
    Error.t
  );

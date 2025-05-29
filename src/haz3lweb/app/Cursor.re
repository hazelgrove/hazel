type cursor('update) = {
  info: option(Haz3lcore.Info.t),
  // Used to display cursor info at the bottom
  // Used as input to explain this
  // *
  selection: option(Haz3lcore.Segment.t),
  // Used to determine what kinds of projectors are applicable
  // Used to copy the current selection
  // *
  indicated_piece: option(Haz3lcore.Piece.t),
  // Used to determine what kinds of projectors are applicable
  // *
  editor: option(Haz3lcore.Editor.Model.t),
  // Used to determing what kinds of projectors are applicable
  // *
  editor_read_only: bool,
  // Used to work out whether we can project at the current cursor position (can't project )
  // *
  editor_action: Haz3lcore.Action.t => option('update),
  // Used to project at a particular location (in projectorpanel)
  // Used by Ninjakeys to perform actions at the cursor position
  // *
  undo_action: option('update),
  // Only used for local undo, will be obselete with global undo
  // *
  redo_action: option('update),
  // Only used for local undo, will be obselete with global undo
  // *
};

let map = (f: 'a => 'b, cursor) => {
  ...cursor,
  editor_action: x => x |> cursor.editor_action |> Option.map(f),
  undo_action: cursor.undo_action |> Option.map(f),
  redo_action: cursor.redo_action |> Option.map(f),
};

let map_opt = (f: 'a => option('b), cursor) => {
  ...cursor,
  editor_action: x => x |> cursor.editor_action |> Option.bind(_, f),
  undo_action: cursor.undo_action |> Option.bind(_, f),
  redo_action: cursor.redo_action |> Option.bind(_, f),
};

let empty = {
  info: None,
  selection: None,
  indicated_piece: None,
  editor: None,
  editor_read_only: false,
  editor_action: _ => None,
  undo_action: None,
  redo_action: None,
};

let (let+) = (cursor, f) => map(f, cursor);

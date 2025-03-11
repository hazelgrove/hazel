type indicated_projector = {
  direction: Util.Direction.t,
  kind: Haz3lcore.ProjectorCore.Kind.t,
  term: Haz3lcore.Any.t,
};

type cursor('update) = {
  info: option(Haz3lcore.Info.t),
  selected_text: option(unit => string),
  selection: option(Haz3lcore.Segment.t),
  indicated_piece: option(Haz3lcore.Piece.t),
  editor: option(Haz3lcore.Editor.t),
  editor_read_only: bool,
  editor_action: Haz3lcore.Action.t => option('update),
  remove_projector: option('update),
  add_projector: Haz3lcore.ProjectorCore.Kind.t => option('update),
  undo_action: option('update),
  redo_action: option('update),
  indicated_projector: option(indicated_projector),
  of_projector: option(Haz3lcore.Id.t => Haz3lcore.Any.t),
};

let map = (f: 'a => 'b, cursor) => {
  ...cursor,
  editor_action: x => x |> cursor.editor_action |> Option.map(f),
  remove_projector: cursor.remove_projector |> Option.map(f),
  add_projector: x => x |> cursor.add_projector |> Option.map(f),
  undo_action: cursor.undo_action |> Option.map(f),
  redo_action: cursor.redo_action |> Option.map(f),
};

let map_opt = (f: 'a => option('b), cursor) => {
  ...cursor,
  editor_action: x => x |> cursor.editor_action |> Option.bind(_, f),
  remove_projector: cursor.remove_projector |> Option.bind(_, f),
  add_projector: x => x |> cursor.add_projector |> Option.bind(_, f),
  undo_action: cursor.undo_action |> Option.bind(_, f),
  redo_action: cursor.redo_action |> Option.bind(_, f),
};

let empty = {
  info: None,
  selected_text: None,
  selection: None,
  indicated_piece: None,
  editor: None,
  editor_read_only: false,
  editor_action: _ => None,
  remove_projector: None,
  add_projector: _ => None,
  undo_action: None,
  redo_action: None,
  indicated_projector: None,
  of_projector: None,
};

let (let+) = (cursor, f) => map(f, cursor);

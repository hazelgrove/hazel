type cursor('update) = {
  info: option(Haz3lcore.Info.t),
  selected_text: option(string),
  editor: option(Haz3lcore.Editor.t),
  editor_action: Haz3lcore.Action.t => option('update),
  undo_action: option('update),
  redo_action: option('update),
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
  selected_text: None,
  editor: None,
  editor_action: _ => None,
  undo_action: None,
  redo_action: None,
};

let (let+) = (cursor, f) => map(f, cursor);

type cursor('update) = {
  info: option(Language.Info.t),
  selected_text: option(unit => string),
  selection: option(Haz3lcore.Segment.t),
  indicated_piece: option(Haz3lcore.Piece.t),
  editor: option(Haz3lcore.Editor.t),
  editor_read_only: bool,
  editor_action: Haz3lcore.Action.t => option('update),
  undo_action: option('update),
  redo_action: option('update),
  /* Global statics summary for status indicator */
  error_ids: list(Util_web.Id.t),
  /* Context-sensitive actions for command palette and keyboard shortcuts */
  contextual_actions: list(ContextualAction.t),
};

let map = (f: 'a => 'b, cursor) => {
  ...cursor,
  editor_action: x => x |> cursor.editor_action |> Option.map(f),
  undo_action: cursor.undo_action |> Option.map(f),
  redo_action: cursor.redo_action |> Option.map(f),
  contextual_actions: cursor.contextual_actions,
};

let map_opt = (f: 'a => option('b), cursor) => {
  ...cursor,
  editor_action: x => x |> cursor.editor_action |> Option.bind(_, f),
  undo_action: cursor.undo_action |> Option.bind(_, f),
  redo_action: cursor.redo_action |> Option.bind(_, f),
  contextual_actions: cursor.contextual_actions,
};

let empty = {
  info: None,
  selected_text: None,
  selection: None,
  indicated_piece: None,
  editor: None,
  editor_read_only: false,
  editor_action: _ => None,
  undo_action: None,
  redo_action: None,
  error_ids: [],
  contextual_actions: [],
};

let with_actions = (actions: list(ContextualAction.t), cursor) => {
  ...cursor,
  contextual_actions: cursor.contextual_actions @ actions,
};

let (let+) = (cursor, f) => map(f, cursor);

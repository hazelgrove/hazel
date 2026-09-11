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
  error_ids: list(Util.Id.t),
  /* Context-sensitive actions for command palette and keyboard shortcuts.
   * The eager list carries item hotkeys, so it must be registered with
   * ninja-keys every render; the lazy list is computed only when the
   * palette opens (refactoring gating walks the syntax). */
  contextual_actions: list(ContextualAction.t),
  contextual_actions_lazy: unit => list(ContextualAction.t),
};

let map = (f: 'a => 'b, cursor) => {
  ...cursor,
  editor_action: x => x |> cursor.editor_action |> Option.map(f),
  undo_action: cursor.undo_action |> Option.map(f),
  redo_action: cursor.redo_action |> Option.map(f),
  contextual_actions: cursor.contextual_actions,
  contextual_actions_lazy: cursor.contextual_actions_lazy,
};

let map_opt = (f: 'a => option('b), cursor) => {
  ...cursor,
  editor_action: x => x |> cursor.editor_action |> Option.bind(_, f),
  undo_action: cursor.undo_action |> Option.bind(_, f),
  redo_action: cursor.redo_action |> Option.bind(_, f),
  contextual_actions: cursor.contextual_actions,
  contextual_actions_lazy: cursor.contextual_actions_lazy,
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
  contextual_actions_lazy: () => [],
};

let with_actions = (actions: list(ContextualAction.t), cursor) => {
  ...cursor,
  contextual_actions: cursor.contextual_actions @ actions,
};

let with_lazy_actions = (actions: unit => list(ContextualAction.t), cursor) => {
  ...cursor,
  contextual_actions_lazy: () => cursor.contextual_actions_lazy() @ actions(),
};

let (let+) = (cursor, f) => map(f, cursor);

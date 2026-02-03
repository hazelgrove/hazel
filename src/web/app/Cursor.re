open Haz3lcore;
open Language;
type cursor('update) = {
  info: option(Info.t),
  dynamics: option(list(Sample.t)),
  selected_text: option(unit => string),
  selection: option(Segment.t),
  indicated_piece: option(Piece.t),
  editor: option(Editor.t),
  editor_read_only: bool,
  editor_action: Action.t => option('update),
  undo_action: option('update),
  redo_action: option('update),
  /* Global statics summary for status indicator */
  error_ids: list(Util.Id.t),
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
  dynamics: None,
  selected_text: None,
  selection: None,
  indicated_piece: None,
  editor: None,
  editor_read_only: false,
  editor_action: _ => None,
  undo_action: None,
  redo_action: None,
  error_ids: [],
};

let (let+) = (cursor, f) => map(f, cursor);

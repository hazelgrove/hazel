open Haz3lcore;
open Util;

/* The TUI application model and update/render functions. Pure except for
   Save (writes the file); the node event loop in Tui.re drives it. The
   update cycle mirrors CodeWithStatics.Update.calculate minus the
   debounce/worker machinery: perform action -> recompute statics if the
   action edited -> Editor.Update.calculate (syntax/measured). */

type model = {
  editor: Editor.Model.t,
  statics: CachedStatics.t,
  dynamics: Language.Dynamics.Map.t,
  history: TuiHistory.t,
  file: option(string),
  dirty: bool,
  result: ResultView.t,
  show_result: bool,
  show_inspector: bool,
  row_off: int,
  col_off: int,
  status_msg: option(string),
  quit_armed: bool,
  /* wheel scrolling detaches the viewport from the caret until the
     next non-wheel action */
  free_scroll: bool,
  /* mouse drag/click-streak state (double/triple click cycle) */
  dragging: bool,
  last_click: (float, Util.Point.t),
  click_streak: int,
};

/* TyDi assist would draw completion buffers we don't fully render yet */
let settings: Language.CoreSettings.t = {
  ...Language.CoreSettings.on,
  assist: false,
};

let mk_statics = (editor: Editor.Model.t): CachedStatics.t =>
  CachedStatics.init(
    ~settings,
    ~is_dynamic_term=false,
    ~stitch=Fun.id,
    ~root=editor.root,
    editor.state.zipper,
  );

let calculate = (~is_edited: bool, model: model): model => {
  let statics = is_edited ? mk_statics(model.editor) : model.statics;
  let editor =
    Editor.Update.calculate(
      ~settings,
      ~autoprobe_mode=false,
      ~is_edited,
      statics,
      model.dynamics,
      model.editor,
    );
  let statics =
    CachedStatics.with_targets(~settings, editor.state.zipper, statics);
  {
    ...model,
    editor,
    statics,
  };
};

let init = (file: option(string)): model => {
  let zipper =
    switch (Option.map(FileIo.load, file)) {
    | Some(Some(z)) => z
    | Some(None)
    | None => Zipper.init()
    };
  let editor = Editor.Model.mk(zipper, ~root=Exp);
  calculate(
    ~is_edited=true,
    {
      editor,
      statics: CachedStatics.empty,
      dynamics: Language.Dynamics.Map.empty,
      history: TuiHistory.empty,
      file,
      dirty: false,
      result: Pending,
      show_result: true,
      show_inspector: false,
      row_off: 0,
      col_off: 0,
      status_msg: None,
      quit_armed: false,
      free_scroll: false,
      dragging: false,
      last_click: ((-1.0), Util.Point.zero),
      click_streak: 0,
    },
  );
};

let run_eval = (model: model): model => {
  ...model,
  result: ResultView.run(model.statics),
};

let perform = (a: Action.t, model: model): model =>
  switch (
    Editor.Update.update(
      ~settings,
      a,
      model.statics,
      model.dynamics,
      model.editor,
    )
  ) {
  | Ok(editor) =>
    let is_edited = Action.is_edit(a);
    let history =
      Action.is_historic(a)
        ? TuiHistory.push(model.editor, model.history) : model.history;
    calculate(
      ~is_edited,
      {
        ...model,
        editor,
        history,
        dirty: model.dirty || is_edited,
        result: is_edited ? Pending : model.result,
      },
    );
  | Error(_failure) => model /* e.g. Cant_move at an edge: ignore */
  | exception exn => {
      ...model,
      status_msg: Some("error: " ++ Printexc.to_string(exn)),
    }
  };

/* The TAB special-case from CodeEditable.Update (web) */
let tab_action = (z: Zipper.t): Action.t =>
  Selection.is_buffer(z.selection)
    ? Buffer(Accept)
    : Zipper.can_put_down(z) ? Put_down : Move(Goal(NextProblem(Right)));

let restore =
    (snapshot: option((Editor.Model.t, TuiHistory.t)), model: model) =>
  switch (snapshot) {
  | None => model
  | Some((editor, history)) =>
    calculate(
      ~is_edited=true,
      {
        ...model,
        editor,
        history,
        dirty: true,
        result: Pending,
      },
    )
  };

let save = (model: model): model =>
  switch (model.file) {
  | None => {
      ...model,
      status_msg: Some("no file to save to"),
    }
  | Some(path) =>
    switch (FileIo.save(path, model.editor.state.zipper)) {
    | () => {
        ...model,
        dirty: false,
        status_msg: Some("saved " ++ path),
      }
    | exception exn => {
        ...model,
        status_msg: Some("save failed: " ++ Printexc.to_string(exn)),
      }
    }
  };

let gutter_width = (n_rows: int): int =>
  String.length(string_of_int(max(1, n_rows))) + 1;

/* Screen coordinates -> buffer coordinates (gutter + viewport) */
let to_buffer = (model: model, screen: Util.Point.t): Util.Point.t => {
  let gutter = gutter_width(List.length(EditorView.rows(model.editor)));
  {
    row: screen.row + model.row_off,
    col: max(0, screen.col - gutter + model.col_off),
  };
};

/* Double/triple-click window, matching typical desktop timing */
let click_streak_window = 0.4;

/* Mouse handling mirrors the web's CodeEditable.move_or_select:
   click -> Move(Point), click streak cycles Move / Smart(2) / Smart(3),
   shift+click and drag -> Select(Resize(Point)). */
let on_mouse = (~now: float, ~page: int, model: model, m: AnsiInput.mouse) =>
  switch (m) {
  | Wheel(n) => {
      ...model,
      row_off: max(0, model.row_off + n),
      free_scroll: true,
    }
  | Release => {
      ...model,
      dragging: false,
    }
  | Drag(screen) when model.dragging =>
    perform(Select(Resize(Point(to_buffer(model, screen), None))), model)
  | Drag(_) => model
  | Press(screen, _) when screen.row >= page => model /* below editor */
  | Press(screen, true) => {
      ...
        perform(
          Select(Resize(Point(to_buffer(model, screen), None))),
          model,
        ),
      dragging: true,
    }
  | Press(screen, false) =>
    let (last_time, last_loc) = model.last_click;
    let streak =
      now
      -. last_time <= click_streak_window
      && Util.Point.equals(screen, last_loc)
        ? model.click_streak + 1 : 1;
    let pt = to_buffer(model, screen);
    let action: Action.t =
      switch (streak mod 3) {
      | 1 => Move(Point(pt, None))
      | 2 => Select(Smart(2))
      | _ => Select(Smart(3))
      };
    {
      ...perform(action, model),
      dragging: streak mod 3 == 1,
      last_click: (now, screen),
      click_streak: streak,
    };
  };

/* Apply one app action. [page] is the editor viewport height, used for
   PageUp/PageDown (implemented as repeated vertical moves so the caret
   stays on screen) and for hit-testing mouse events. [now] is the time
   in seconds, used for click-streak detection. Returns the new model
   and whether to quit. */
let apply =
    (~now: float=0.0, ~page: int, model: model, action: Keymap.t)
    : (model, bool) => {
  let model = {
    ...model,
    status_msg: None,
    free_scroll: false /* wheel re-sets it below */
  };
  let repeat = (n, a: Action.t, model) =>
    List.fold_left((m, _) => perform(a, m), model, List.init(n, Fun.id));
  switch (action) {
  | Mouse(m) => (on_mouse(~now, ~page, model, m), false)
  | Perform(a) => (perform(a, model), false)
  | Tab => (perform(tab_action(model.editor.state.zipper), model), false)
  | Save => (save(model), false)
  | Undo => (
      restore(TuiHistory.undo(model.editor, model.history), model),
      false,
    )
  | Redo => (
      restore(TuiHistory.redo(model.editor, model.history), model),
      false,
    )
  | PageUp => (
      repeat(max(1, page), Move(Vertical(Up, ByChar)), model),
      false,
    )
  | PageDown => (
      repeat(max(1, page), Move(Vertical(Down, ByChar)), model),
      false,
    )
  | ToggleResult => (
      {
        ...model,
        show_result: !model.show_result,
      },
      false,
    )
  | ToggleInspector => (
      {
        ...model,
        show_inspector: !model.show_inspector,
      },
      false,
    )
  | Quit =>
    model.dirty && !model.quit_armed
      ? (
        {
          ...model,
          quit_armed: true,
          status_msg: Some("unsaved changes! Ctrl+Q or Ctrl+C again to quit"),
        },
        false,
      )
      : (model, true)
  };
};

/* Reset the quit confirmation when any non-quit action intervenes */
let disarm = (model: model, action: Keymap.t): model =>
  switch (action) {
  | Quit => model
  | _ => {
      ...model,
      quit_armed: false,
    }
  };

/* === layout === */

let result_pane_height = (~rows: int, model: model): int =>
  model.show_result
    ? min(ResultView.wanted_height(model.result), rows / 3) : 0;

let editor_height = (~size: (int, int), model: model): int => {
  let (rows, _) = size;
  max(
    1,
    rows
    - 1
    - result_pane_height(~rows, model)
    - InspectorView.height(~size, model.show_inspector),
  );
};

/* Build the frame for the current model; also returns the model with
   viewport offsets clamped so the caret is visible. */
let render = (~size: (int, int), model: model): (Frame.t, model) => {
  let (rows, cols) = size;
  let editor_h = editor_height(~size, model);
  let buffer_rows = EditorView.rows(model.editor);
  let n_rows = List.length(buffer_rows);
  let gutter_w = gutter_width(n_rows);
  let text_w = max(1, cols - gutter_w);
  let caret = EditorView.caret_point(model.editor);

  /* clamp viewport to keep the caret visible — unless the user has
     wheel-scrolled away (free_scroll), in which case only keep the
     offsets within the buffer */
  let clamp = (off, lo, hi) => max(min(off, lo), hi);
  let (row_off, col_off) =
    model.free_scroll
      ? (min(model.row_off, n_rows - 1), model.col_off)
      : (
        clamp(model.row_off, caret.row, caret.row - editor_h + 1),
        clamp(model.col_off, caret.col, caret.col - text_w + 1),
      );
  let row_off = max(0, row_off);
  let col_off = max(0, col_off);
  let model = {
    ...model,
    row_off,
    col_off,
  };

  /* error/warning undercurls (selection reverse-video goes on top) */
  let buffer_rows =
    EditorView.apply_ranges(
      buffer_rows,
      EditorView.warning_ranges(model.statics, model.editor),
      Theme.warning_underline,
    );
  let buffer_rows =
    EditorView.apply_ranges(
      buffer_rows,
      EditorView.error_ranges(model.statics, model.editor),
      Theme.error_underline,
    );

  /* selection overlay in buffer coordinates */
  let buffer_rows =
    switch (EditorView.selection_range(model.editor)) {
    | Some((from, to_)) =>
      EditorView.apply_selection(buffer_rows, ~from, ~to_)
    | None => buffer_rows
    };

  /* viewport rows with line-number gutter */
  let visible =
    List.filteri(
      (i, _) => i >= row_off && i < row_off + editor_h,
      buffer_rows,
    )
    |> List.mapi((i, row) => {
         let line = string_of_int(row_off + i + 1);
         let num =
           String.make(gutter_w - 1 - String.length(line), ' ')
           ++ line
           ++ " ";
         [
           (Theme.line_number, num),
           ...EditorView.clip_row(row, ~col_off, ~width=text_w),
         ];
       });

  let result_rows =
    model.show_result
      ? ResultView.rows(
          ~width=cols,
          ~height=result_pane_height(~rows, model),
          model.result,
        )
      : [];

  let status =
    StatusView.row(
      ~width=cols,
      ~file=model.file,
      ~dirty=model.dirty,
      ~status_msg=model.status_msg,
      ~caret,
      ~statics=model.statics,
      model.editor.state.zipper,
    );

  let inspector_rows =
    InspectorView.rows(
      ~width=cols,
      ~height=InspectorView.height(~size, model.show_inspector),
      model.editor.state.zipper,
      model.statics,
    );

  /* pad the editor area so the panes + status sit at the bottom */
  let pad_rows = max(0, editor_h - List.length(visible));
  let frame_rows =
    visible
    @ List.init(pad_rows, _ => [])
    @ result_rows
    @ inspector_rows
    @ [status];

  let cursor =
    Point.{
      row: caret.row - row_off,
      col: caret.col - col_off + gutter_w,
    };
  let cursor =
    cursor.row >= 0 && cursor.row < editor_h && cursor.col >= gutter_w - 1
      ? Some(cursor) : None;

  (
    Frame.{
      rows: frame_rows,
      cursor,
    },
    model,
  );
};

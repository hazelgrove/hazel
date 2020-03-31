module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;
module KeyCombo = JSUtil.KeyCombo;
module MoveKey = JSUtil.MoveKey;
open ViewUtil;
open Sexplib.Std;

let kc_actions: Hashtbl.t(KeyCombo.t, CursorInfo.t => Action.t) =
  [
    (KeyCombo.Backspace, _ => Action.Backspace),
    (Delete, _ => Action.Delete),
    (ShiftTab, _ => Action.MoveToPrevHole),
    (Tab, _ => Action.MoveToNextHole),
    (
      KeyCombo.GT,
      fun
      | {CursorInfo.typed: OnType, _} => Action.Construct(SOp(SArrow))
      | _ => Action.Construct(SOp(SGreaterThan)),
    ),
    (Ampersand, _ => Action.Construct(SOp(SAnd))),
    (VBar, _ => Action.Construct(SOp(SOr))),
    (LeftParen, _ => Action.Construct(SParenthesized)),
    (Colon, _ => Action.Construct(SAsc)),
    (Equals, _ => Action.Construct(SOp(SEquals))),
    (Enter, _ => Action.Construct(SLine)),
    (Backslash, _ => Action.Construct(SLam)),
    (Plus, _ => Action.Construct(SOp(SPlus))),
    (Minus, _ => Action.Construct(SOp(SMinus))),
    (Asterisk, _ => Action.Construct(SOp(STimes))),
    (LT, _ => Action.Construct(SOp(SLessThan))),
    (Space, _ => Action.Construct(SOp(SSpace))),
    (Comma, _ => Action.Construct(SOp(SComma))),
    (
      LeftBracket,
      fun
      | {CursorInfo.typed: OnType, _} => Action.Construct(SList)
      | _ => Action.Construct(SListNil),
    ),
    (Semicolon, _ => Action.Construct(SOp(SCons))),
    (Alt_L, _ => Action.Construct(SInj(L))),
    (Alt_R, _ => Action.Construct(SInj(R))),
    (Alt_C, _ => Action.Construct(SCase)),
  ]
  |> List.to_seq
  |> Hashtbl.of_seq;

let on_click =
    (~inject, ~font_metrics: FontMetrics.t, ~cmap: CursorMap.t, evt) => {
  let container_rect =
    JSUtil.force_get_elem_by_id("code-container")##getBoundingClientRect;
  let (target_x, target_y) = (
    float_of_int(evt##.clientX),
    float_of_int(evt##.clientY),
  );
  if (container_rect##.left <= target_x
      && target_x <=
      container_rect##.right
      &&
      container_rect##.top <= target_y
      && target_y <=
      container_rect##.bottom) {
    let row_col = (
      Float.to_int(
        (target_y -. container_rect##.top) /. font_metrics.row_height,
      ),
      Float.to_int(
        Float.round(
          (target_x -. container_rect##.left) /. font_metrics.col_width,
        ),
      ),
    );
    let (_, rev_path) = cmap |> CursorMap.find_nearest_within_row(row_col);
    let path = CursorPath.rev(rev_path);
    inject(Update.Action.EditAction(MoveTo(path)));
  } else {
    Vdom.Event.Many([]);
  };
};

let focused_view = (~inject, ~font_metrics: FontMetrics.t, program) => {
  open Vdom;

  let (zmap, view) =
    program
    |> Program.get_doc
    |> UHCode.focused_view(
         ~inject,
         ~path=program |> Program.get_path,
         ~ci=program |> Program.get_cursor_info,
       );

  let prevent_stop_inject = a =>
    Event.Many([Event.Prevent_default, Event.Stop_propagation, inject(a)]);

  let position_cursor = ((row, col), caret_elem) => {
    caret_elem##.style##.top :=
      Js.string(
        string_of_float(float_of_int(row) *. font_metrics.row_height)
        ++ "0px",
      );
    caret_elem##.style##.left :=
      Js.string(
        string_of_float(float_of_int(col) *. font_metrics.col_width) ++ "0px",
      );
  };

  let restart_cursor_animation = caret_elem => {
    caret_elem##.classList##remove(Js.string("blink"));
    // necessary to trigger reflow
    let _ = caret_elem##getBoundingClientRect;
    caret_elem##.classList##add(Js.string("blink"));
  };

  let scroll_cursor_into_view_if_needed = caret_elem => {
    let page_rect =
      JSUtil.force_get_elem_by_id("page-area")##getBoundingClientRect;
    let caret_rect = caret_elem##getBoundingClientRect;
    if (caret_rect##.top < page_rect##.top) {
      caret_elem##scrollIntoView(Js._true);
    } else if (caret_rect##.bottom > page_rect##.bottom) {
      caret_elem##scrollIntoView(Js._false);
    };
  };

  let paint_cursor = row_col => {
    let caret_elem = JSUtil.force_get_elem_by_id("caret");
    position_cursor(row_col, caret_elem);
    restart_cursor_animation(caret_elem);
    scroll_cursor_into_view_if_needed(caret_elem);
    caret_elem##.style##.visibility := Js.string("visible");
  };

  let removed_term_indicator = ref(false);
  let move_cursor = move_key => {
    let (_, (_, rev_steps)) = zmap.z;
    let moved = zmap |> ZCursorMap.move(move_key);
    if (moved) {
      let (new_row_col, (_, new_rev_steps)) = zmap.z;
      if (! removed_term_indicator^) {
        if (new_rev_steps != rev_steps) {
          JSUtil.force_get_elem_by_cls("Cursor")##.classList##remove(
            Js.string("Cursor"),
          );
          removed_term_indicator := true;
        };
      };
      paint_cursor(new_row_col);
    } else {
      JSUtil.log("[CursorEscaped]");
    };
  };

  let key_handlers = [
    Attr.on_click(on_click(~inject, ~font_metrics, ~cmap=zmap.map)),
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keyup(evt => {
      switch (MoveKey.of_key(JSUtil.get_key(evt))) {
      | None => Event.Many([])
      | Some(_) =>
        let (_, rev_path) = zmap.z;
        let path = CursorPath.rev(rev_path);
        prevent_stop_inject(Update.Action.EditAction(MoveTo(path)));
      }
    }),
    Attr.on_keydown(evt => {
      switch (MoveKey.of_key(JSUtil.get_key(evt))) {
      | Some(move_key) =>
        move_cursor(move_key);
        Event.Prevent_default;
      | None =>
        switch (KeyCombo.of_evt(evt)) {
        | Some(Ctrl_Z) => prevent_stop_inject(Update.Action.Undo)
        | Some(Ctrl_Shift_Z) => prevent_stop_inject(Update.Action.Redo)
        | Some(kc) =>
          prevent_stop_inject(
            Update.Action.EditAction(
              Hashtbl.find(
                kc_actions,
                kc,
                program |> Program.get_cursor_info,
              ),
            ),
          )
        | None =>
          switch (JSUtil.is_single_key(evt)) {
          | None => Event.Ignore
          | Some(single_key) =>
            prevent_stop_inject(
              Update.Action.EditAction(
                Construct(SChar(JSUtil.single_key_string(single_key))),
              ),
            )
          }
        }
      }
    }),
  ];

  let on_display = (_, ~schedule_action as _) => {
    let (row_col, _) = zmap.z;
    paint_cursor(row_col);
  };

  (key_handlers, view, on_display);
};

let unfocused_view = (~inject, ~font_metrics, program) => {
  // TODO set up click handlers
  let (cmap, view) =
    UHCode.unfocused_view(~inject, program |> Program.get_doc);
  let on_display = (_, ~schedule_action as _) => ();
  (
    [Vdom.Attr.on_click(on_click(~inject, ~font_metrics, ~cmap))],
    view,
    on_display,
  );
};

let view = (~inject, model: Model.t) => {
  open Vdom;
  let font_metrics = model.font_metrics;
  let program = model |> Model.get_program;
  let (evt_handlers, code_view, on_display) =
    if (model.is_cell_focused) {
      focused_view(~inject, ~font_metrics, program);
    } else {
      unfocused_view(~inject, ~font_metrics, program);
    };
  let view =
    Node.div(
      [
        Attr.id(cell_id),
        // necessary to make cell focusable
        Attr.create("tabindex", "0"),
        Attr.on_focus(_ => inject(Update.Action.FocusCell)),
        Attr.on_blur(_ => inject(Update.Action.BlurCell)),
        ...evt_handlers,
      ],
      [
        Node.div([Attr.id("font-specimen")], [Node.text("X")]),
        Node.div([Attr.id("code-container")], [code_view]),
      ],
    );
  (on_display, view);
};

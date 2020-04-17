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
    (Ctrl_Alt_Up, _ => Action.SwapUp),
    (Ctrl_Alt_Down, _ => Action.SwapDown),
    (Ctrl_Alt_Left, _ => Action.SwapLeft),
    (Ctrl_Alt_Right, _ => Action.SwapRight),
  ]
  |> List.to_seq
  |> Hashtbl.of_seq;

let on_click = (~inject, ~font_metrics: FontMetrics.t, evt) => {
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
    inject(Update.Action.MoveViaClick(row_col));
  } else {
    Vdom.Event.Many([]);
  };
};

let view = (~inject, model: Model.t) => {
  open Vdom;
  let font_metrics = model.font_metrics;
  let program = model |> Model.get_program;
  let on_click_attr = Vdom.Attr.on_click(on_click(~inject, ~font_metrics));
  let prevent_stop_inject = a =>
    Event.Many([Event.Prevent_default, Event.Stop_propagation, inject(a)]);
  let (evt_handlers, code_view) =
    if (Model.is_cell_focused(model)) {
      let evt_handlers = [
        on_click_attr,
        Attr.on_keypress(_ => Event.Prevent_default),
        Attr.on_keydown(evt => {
          switch (MoveKey.of_key(JSUtil.get_key(evt))) {
          | Some(move_key) =>
            prevent_stop_inject(Update.Action.MoveViaKey(move_key))
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
      let view =
        program |> Program.get_decorated_layout |> UHCode.view(~inject);
      (evt_handlers, view);
    } else {
      let evt_handlers = [on_click_attr];
      let view = program |> Program.get_layout |> UHCode.view(~inject);
      (evt_handlers, view);
    };
  Node.div(
    [
      Attr.id(cell_id),
      // necessary to make cell focusable
      Attr.create("tabindex", "0"),
      Attr.on_blur(_ => inject(Update.Action.BlurCell)),
      ...evt_handlers,
    ],
    [
      Node.div([Attr.id("font-specimen")], [Node.text("X")]),
      Node.div([Attr.id("code-container")], [code_view]),
    ],
  );
};

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

let move_cursor = (~font_metrics: option(FontMetrics.t), move_key, zmap) => {
  let moved = zmap |> ZCursorMap.move(move_key);
  if (moved) {
    let ((row, col), _) = zmap |> ZCursorMap.get_cursor;
    let caret_elem = JSUtil.force_get_elem_by_id("caret");
    switch (font_metrics) {
    | None => caret_elem##.style##.visibility := Js.string("hidden")
    | Some({row_height, col_width}) =>
      JSUtil.force_get_elem_by_cls("Cursor")##.classList##remove(
        Js.string("Cursor"),
      );
      caret_elem##.style##.top :=
        Js.string(string_of_float(float_of_int(row) *. row_height) ++ "0px");
      caret_elem##.style##.left :=
        Js.string(string_of_float(float_of_int(col) *. col_width) ++ "0px");
      caret_elem##.style##.visibility := Js.string("visible");
      caret_elem##focus;
    };
  } else {
    JSUtil.log("[CursorEscaped]");
  };
};

let view = (~inject, model) => {
  open Vdom;
  let prevent_stop_inject = a =>
    Event.Many([Event.Prevent_default, Event.Stop_propagation, inject(a)]);

  let program = model |> Model.get_program;
  let (evt_handlers, code_view) =
    if (model.is_cell_focused) {
      let (zmap, view) =
        program
        |> Program.get_doc
        |> UHCode.focused_view(
             ~inject,
             ~path=program |> Program.get_path,
             ~ci=program |> Program.get_cursor_info,
           );
      let attrs = [
        Attr.on_keyup(evt => {
          switch (MoveKey.of_key(JSUtil.get_key(evt))) {
          | None => Event.Many([])
          | Some(_) =>
            let (_, rev_path) = zmap |> ZCursorMap.get_cursor;
            let path = CursorPath.rev(rev_path);
            prevent_stop_inject(Update.Action.EditAction(MoveTo(path)));
          }
        }),
        Attr.on_keydown(evt => {
          switch (MoveKey.of_key(JSUtil.get_key(evt))) {
          | Some(move_key) =>
            move_cursor(~font_metrics=model.font_metrics, move_key, zmap);
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
      (attrs, view);
    } else {
      // TODO set up click handlers
      let (_cmap, view) =
        UHCode.unfocused_view(~inject, program |> Program.get_doc);
      ([], view);
    };
  Node.div(
    [Attr.id(cell_id), ...evt_handlers],
    [
      Node.div([Attr.id("font-specimen")], [Node.text("x")]),
      Node.div([Attr.id("code-container")], [code_view]),
    ],
  );
};

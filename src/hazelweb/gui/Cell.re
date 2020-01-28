module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;
module KeyCombo = JSUtil.KeyCombo;
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
    (Shift_Enter, _ => Action.Construct(SSCommentLine)),
    // Create "Enter + Shift" => Action.Construct(SSCommentLine)
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

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  Vdom.(
    Node.div(
      [
        Attr.id(cell_id),
        Attr.on_keypress(evt =>
          JSUtil.is_movement_key(evt)
            ? Event.Many([]) : Event.Prevent_default
        ),
        Attr.on_keydown(evt => {
          let prevent_stop_inject = a =>
            Vdom.Event.Many([
              Vdom.Event.Prevent_default,
              Vdom.Event.Stop_propagation,
              inject(a),
            ]);
          if (JSUtil.is_movement_key(evt)) {
            Event.Many([]);
          } else {
            switch (KeyCombo.of_evt(evt)) {
            | Some(Ctrl_Z) => prevent_stop_inject(Update.Action.Undo)
            | Some(Ctrl_Shift_Z) => prevent_stop_inject(Update.Action.Redo)
            | Some(kc) =>
              prevent_stop_inject(
                Update.Action.EditAction(
                  Hashtbl.find(kc_actions, kc, model.cursor_info),
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
            };
          };
        }),
      ],
      {
        let (contenteditable, presentation) =
          model.is_cell_focused
            ? Code.editor_view_of_exp(
                ~inject,
                ~path=model |> Model.path,
                ~ci=model.cursor_info,
                model |> Model.exp,
              )
            : Code.editor_view_of_exp(~inject, model |> Model.exp);
        [
          Node.div(
            [Attr.id("code-container")],
            [contenteditable, presentation],
          ),
        ];
      },
    )
  );
};

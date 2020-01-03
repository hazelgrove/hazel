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
    (KeyCombo.Delete, _ => Action.Delete),
    (KeyCombo.ShiftTab, _ => Action.MoveToPrevHole),
    (KeyCombo.Tab, _ => Action.MoveToNextHole),
    (
      KeyCombo.GT,
      fun
      | {CursorInfo.typed: OnType, _} => Action.Construct(SOp(SArrow))
      | _ => Action.Construct(SOp(SGreaterThan)),
    ),
    (KeyCombo.Ampersand, _ => Action.Construct(SOp(SAnd))),
    (KeyCombo.VBar, _ => Action.Construct(SOp(SOr))),
    (KeyCombo.LeftParen, _ => Action.Construct(SParenthesized)),
    (KeyCombo.Colon, _ => Action.Construct(SAsc)),
    (KeyCombo.Equals, _ => Action.Construct(SOp(SEquals))),
    (KeyCombo.Enter, _ => Action.Construct(SLine)),
    (KeyCombo.Backslash, _ => Action.Construct(SLam)),
    (KeyCombo.Plus, _ => Action.Construct(SOp(SPlus))),
    (KeyCombo.Minus, _ => Action.Construct(SOp(SMinus))),
    (KeyCombo.Asterisk, _ => Action.Construct(SOp(STimes))),
    (KeyCombo.LT, _ => Action.Construct(SOp(SLessThan))),
    (KeyCombo.Space, _ => Action.Construct(SOp(SSpace))),
    (KeyCombo.Comma, _ => Action.Construct(SOp(SComma))),
    (KeyCombo.LeftBracket, _ => Action.Construct(SListNil)),
    (KeyCombo.Semicolon, _ => Action.Construct(SOp(SCons))),
    (KeyCombo.Alt_L, _ => Action.Construct(SInj(L))),
    (KeyCombo.Alt_R, _ => Action.Construct(SInj(R))),
    (KeyCombo.Alt_C, _ => Action.Construct(SCase)),
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

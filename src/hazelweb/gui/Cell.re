module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;
module KeyCombo = JSUtil.KeyCombo;
open ViewUtil;
open Sexplib.Std;

type kc_info = {
  action_fn: CursorInfo.t => Action.t,
  description: string,
};

let kc_actions: Hashtbl.t(KeyCombo.t, kc_info) =
  [
    (
      KeyCombo.Backspace,
      {action_fn: _ => Action.Backspace, description: "Delete Character"},
    ),
    (
      Delete,
      {action_fn: _ => Action.Delete, description: "Delete Expression"},
    ),
    (
      ShiftTab,
      {
        action_fn: _ => Action.MoveToPrevHole,
        description: "Move to prev hole",
      },
    ),
    (
      Tab,
      {
        action_fn: _ => Action.MoveToNextHole,
        description: "Move to next hole",
      },
    ),
    (
      KeyCombo.GT,
      {
        action_fn:
          fun
          | {CursorInfo.typed: OnType, _} => Action.Construct(SOp(SArrow))
          | _ => Action.Construct(SOp(SGreaterThan)),
        description: "Insert > Operator",
      },
    ),
    (
      Ampersand,
      {
        action_fn: _ => Action.Construct(SOp(SAnd)),
        description: "Insert & operator",
      },
    ),
    (
      VBar,
      {
        action_fn: _ => Action.Construct(SOp(SOr)),
        description: "Insert | operator",
      },
    ),
    (
      LeftParen,
      {
        action_fn: _ => Action.Construct(SParenthesized),
        description: "Parenthesize expression",
      },
    ),
    (
      Colon,
      {
        action_fn: _ => Action.Construct(SAsc),
        description: "Type ascription",
      },
    ),
    (
      Equals,
      {
        action_fn: _ => Action.Construct(SOp(SEquals)),
        description: "Insert = operator",
      },
    ),
    (
      Enter,
      {
        action_fn: _ => Action.Construct(SLine),
        description: "Create new line",
      },
    ),
    (
      Backslash,
      {
        action_fn: _ => Action.Construct(SLam),
        description: "Insert Lambda expression",
      },
    ),
    (
      Plus,
      {
        action_fn: _ => Action.Construct(SOp(SPlus)),
        description: "Insert + operator",
      },
    ),
    (
      Minus,
      {
        action_fn: _ => Action.Construct(SOp(SMinus)),
        description: "Insert - operator",
      },
    ),
    (
      Asterisk,
      {
        action_fn: _ => Action.Construct(SOp(STimes)),
        description: "Insert * operator",
      },
    ),
    (
      LT,
      {
        action_fn: _ => Action.Construct(SOp(SLessThan)),
        description: "Insert < operator",
      },
    ),
    (
      Space,
      {
        action_fn: _ => Action.Construct(SOp(SSpace)),
        description: "Separate terms",
      },
    ),
    (
      Comma,
      {
        action_fn: _ => Action.Construct(SOp(SComma)),
        description: ", operator",
      },
    ),
    (
      LeftBracket,
      {
        action_fn:
          fun
          | {CursorInfo.typed: OnType, _} => Action.Construct(SList)
          | _ => Action.Construct(SListNil),
        description: "Insert a list",
      },
    ),
    (
      Semicolon,
      {
        action_fn: _ => Action.Construct(SOp(SCons)),
        description: "Cons operator",
      },
    ),
    (
      Alt_L,
      {
        action_fn: _ => Action.Construct(SInj(L)),
        description: "Left injection",
      },
    ),
    (
      Alt_R,
      {
        action_fn: _ => Action.Construct(SInj(R)),
        description: "Right injection",
      },
    ),
    (
      Alt_C,
      {
        action_fn: _ => Action.Construct(SCase),
        description: "Insert case statement",
      },
    ),
  ]
  |> List.to_seq
  |> Hashtbl.of_seq;

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  let program = model |> Model.get_program;
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
              let action_fn = Hashtbl.find(kc_actions, kc).action_fn;
              prevent_stop_inject(
                Update.Action.EditAction(
                  program |> Program.get_cursor_info |> action_fn,
                ),
              );
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
            ? UHCode.focused_view(
                ~inject,
                ~show_contenteditable=model.show_contenteditable,
                ~path=program |> Program.get_path,
                ~ci=program |> Program.get_cursor_info,
                program |> Program.get_doc,
              )
            : UHCode.view(
                ~inject,
                ~show_contenteditable=model.show_contenteditable,
                program |> Program.get_doc,
              );
        [
          Node.div(
            [Attr.id("code-container")],
            [presentation, contenteditable],
          ),
        ];
      },
    )
  );
};

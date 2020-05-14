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
    (PlusPlus, _ => Action.Construct(SOp(SPlusPlus))),
    (Asterisk, _ => Action.Construct(SOp(STimes))),
    (LT, _ => Action.Construct(SOp(SLessThan))),
    (Space, _ => Action.Construct(SOp(SSpace))),
    (Comma, _ => Action.Construct(SOp(SComma))),
    (
      LeftBracket,
      fun
      | {CursorInfo.typed: OnType, _} => Action.Construct(SList)
      | _ => Action.Construct(SList),
    ),
    (LeftQuotation, _ => Action.Construct(SQuote)),
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

let focus = () => {
  JSUtil.force_get_elem_by_id("cell")##focus;
};

let view = (~inject, model: Model.t) => {
  TimeUtil.measure_time(
    "Cell.view",
    model.measurements.measurements && model.measurements.cell_view,
    () => {
      open Vdom;
      let program = model |> Model.get_program;
      let code_view =
        UHCode.view(~model, ~inject, ~font_metrics=model.font_metrics);
      let prevent_stop_inject = a =>
        Event.Many([
          Event.Prevent_default,
          Event.Stop_propagation,
          inject(a),
        ]);
      let (key_handlers, code_view) =
        if (Model.is_cell_focused(model)) {
          let key_handlers = [
            Attr.on_keypress(_ => Event.Prevent_default),
            Attr.on_keydown(evt => {
              switch (MoveKey.of_key(JSUtil.get_key(evt))) {
              | Some(move_key) =>
                prevent_stop_inject(Update.Action.MoveAction(Key(move_key)))
              | None =>
                let s = JSUtil.get_key(evt);
                print_endline("Cell, s = " ++ s);
                switch (
                  s,
                  CursorInfo.is_text_cursor(program |> Program.get_zexp),
                ) {
                | (
                    "~" | "`" | "!" | "@" | "#" | "$" | "%" | "^" | "&" | "*" |
                    "(" |
                    ")" |
                    "-" |
                    "_" |
                    "=" |
                    "+" |
                    "{" |
                    "}" |
                    "[" |
                    "]" |
                    ":" |
                    ";" |
                    "\"" |
                    "'" |
                    "<" |
                    ">" |
                    "," |
                    "." |
                    "?" |
                    "/" |
                    "|" |
                    "\\" |
                    " ",
                    true,
                  ) =>
                  prevent_stop_inject(
                    Update.Action.EditAction(Construct(SChar(s))),
                  )
                | (_, _) =>
                  switch (KeyCombo.of_evt(evt)) {
                  | Some(Ctrl_Z) => prevent_stop_inject(Update.Action.Undo)
                  | Some(Ctrl_Shift_Z) =>
                    prevent_stop_inject(Update.Action.Redo)
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
                          Construct(
                            SChar(JSUtil.single_key_string(single_key)),
                          ),
                        ),
                      )
                    }
                  }
                };
              }
            }),
          ];
          let view =
            program
            |> Program.get_decorated_layout(
                 ~measure_program_get_doc=
                   model.measurements.measurements
                   && model.measurements.program_get_doc,
                 ~measure_layoutOfDoc_layout_of_doc=
                   model.measurements.measurements
                   && model.measurements.layoutOfDoc_layout_of_doc,
                 ~memoize_doc=model.memoize_doc,
               )
            |> code_view;
          (key_handlers, view);
        } else {
          (
            [],
            program
            |> Program.get_layout(
                 ~measure_program_get_doc=
                   model.measurements.measurements
                   && model.measurements.program_get_doc,
                 ~measure_layoutOfDoc_layout_of_doc=
                   model.measurements.measurements
                   && model.measurements.layoutOfDoc_layout_of_doc,
                 ~memoize_doc=model.memoize_doc,
               )
            |> code_view,
          );
        };
      Node.div(
        [
          Attr.id(cell_id),
          // necessary to make cell focusable
          Attr.create("tabindex", "0"),
          Attr.on_focus(_ => inject(Update.Action.FocusCell)),
          Attr.on_blur(_ => inject(Update.Action.BlurCell)),
          ...key_handlers,
        ],
        [
          Node.div([Attr.id("font-specimen")], [Node.text("X")]),
          Node.div([Attr.id("code-container")], [code_view]),
        ],
      );
    },
  );
};

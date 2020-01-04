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
      ],
      [
        Node.div(
          [
            Attr.id(cell_id),
            Attr.create("contenteditable", "true"),
            Attr.on("drop", _ => Event.Prevent_default),
            Attr.on_focus(_ => inject(FocusCell)),
            Attr.on_blur(_ => inject(BlurCell)),
            Attr.on_keypress(evt =>
              switch (
                model.cursor_info.position,
                JSUtil.is_movement_key(evt),
              ) {
              | (Staging(_), _) => Event.Prevent_default
              | (OnText(_) | OnDelim(_, _), true) => Event.Many([])
              | (OnText(_) | OnDelim(_, _), false) => Event.Prevent_default
              }
            ),
            Attr.on_keydown(evt => {
              let prevent_stop_inject = a =>
                Vdom.Event.Many([
                  Vdom.Event.Prevent_default,
                  Vdom.Event.Stop_propagation,
                  inject(a),
                ]);
              let ci = model.cursor_info;
              switch (
                ci.position,
                JSUtil.is_movement_key(evt),
                JSUtil.is_single_key(evt),
                KeyCombo.of_evt(evt),
              ) {
              | (Staging(_), true, _, _) =>
                switch (evt |> JSUtil.get_key) {
                | "ArrowLeft" =>
                  prevent_stop_inject(Update.Action.EditAction(ShiftLeft))
                | "ArrowRight" =>
                  prevent_stop_inject(Update.Action.EditAction(ShiftRight))
                | "ArrowUp" =>
                  prevent_stop_inject(Update.Action.EditAction(ShiftUp))
                | "ArrowDown" =>
                  prevent_stop_inject(Update.Action.EditAction(ShiftDown))
                | _ => Event.Ignore
                }
              | (OnText(_) | OnDelim(_, _), true, _, _) => Event.Many([])
              | (_, _, None, None) => Event.Ignore
              | (_, _, Some(single_key), opt_kc) =>
                switch (
                  entered_single_key(
                    ~prevent_stop_inject,
                    ci,
                    single_key,
                    opt_kc,
                  )
                ) {
                | Some(event) => event
                | None =>
                  let zblock = model |> Model.zblock;
                  switch (ci.position) {
                  | Staging(_)
                  | OnText(_) => Event.Ignore
                  | OnDelim(_, side) =>
                    let move_cursor =
                      switch (side) {
                      | Before => ZExp.move_cursor_left_block
                      | After => ZExp.move_cursor_right_block
                      };
                    switch (zblock |> move_cursor) {
                    | None => Event.Ignore
                    | Some(zblock) =>
                      switch (
                        CursorInfo.syn_cursor_info_block(
                          Contexts.empty,
                          zblock,
                        )
                      ) {
                      | None => Event.Ignore
                      | Some(ci) =>
                        switch (
                          entered_single_key(
                            ~prevent_stop_inject,
                            ci,
                            single_key,
                            opt_kc,
                          )
                        ) {
                        | None => Event.Ignore
                        | Some(event) => event
                        }
                      }
                    };
                  };
                }
              | (_, _, _, Some((Backspace | Delete) as kc)) =>
                let (string_edit, update, cursor_escaped) =
                  switch (kc) {
                  | Backspace => (
                      string_backspace,
                      Update.Action.EditAction(Backspace),
                      ci |> CursorInfo.is_before_node,
                    )
                  | _ => (
                      string_delete,
                      Update.Action.EditAction(Delete),
                      ci |> CursorInfo.is_after_node,
                    )
                  };
                switch (
                  kc,
                  model.user_newlines
                  |> CursorPath.StepsMap.mem(ci.node_steps),
                  cursor_escaped,
                  ci.position,
                ) {
                | (Backspace, true, _, _) =>
                  prevent_stop_inject(
                    Update.Action.RemoveUserNewline(ci.node_steps),
                  )
                | (_, true, _, _) => prevent_stop_inject(update)
                | (_, false, true, _)
                | (_, false, _, OnDelim(_, _) | Staging(_)) =>
                  prevent_stop_inject(update)
                | (_, false, false, OnText(_)) =>
                  let nodeValue = JSUtil.force_get_anchor_node_value();
                  let anchorOffset = JSUtil.get_anchor_offset();
                  let ctrlKey = Js.to_bool(evt##.ctrlKey);
                  let (nodeValue', anchorOffset') =
                    string_edit(nodeValue, anchorOffset, ctrlKey);
                  switch (
                    String.equal(nodeValue', ""),
                    int_of_string_opt(nodeValue'),
                  ) {
                  | (true, _) => prevent_stop_inject(update)
                  | (false, Some(new_n)) =>
                    prevent_stop_inject(
                      Update.Action.EditAction(
                        Construct(SNumLit(new_n, OnText(anchorOffset'))),
                      ),
                    )
                  | (false, None) =>
                    Var.is_valid(nodeValue')
                      ? prevent_stop_inject(
                          Update.Action.EditAction(
                            Construct(
                              SVar(nodeValue', OnText(anchorOffset')),
                            ),
                          ),
                        )
                      : prevent_stop_inject(
                          Update.Action.InvalidVar(nodeValue'),
                        )
                  };
                };
              | (OnText(_) | OnDelim(_, _), _, _, Some(Enter)) =>
                switch (
                  model.user_newlines
                  |> CursorPath.StepsMap.mem(ci.node_steps),
                  model |> Model.zblock |> ZExp.is_after_case_rule,
                  model |> Model.zblock |> ZExp.is_on_user_newlineable_hole,
                ) {
                | (false, false, true) =>
                  prevent_stop_inject(
                    Update.Action.AddUserNewline(ci.node_steps),
                  )
                | (_, _, _) =>
                  prevent_stop_inject(
                    Update.Action.EditAction(
                      ci.node |> Hashtbl.find(kc_actions, Enter),
                    ),
                  )
                }
              | (Staging(_), _, _, Some(Escape)) =>
                prevent_stop_inject(
                  Update.Action.EditAction(
                    ci.node |> Hashtbl.find(kc_actions, Enter),
                  ),
                )
              | (_, _, _, Some(Ctrl_Z)) =>
                prevent_stop_inject(Update.Action.Undo)
              | (_, _, _, Some(Ctrl_Shift_Z)) =>
                prevent_stop_inject(Update.Action.Redo)
              | (_, _, _, Some(kc)) =>
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

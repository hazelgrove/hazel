module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;
module KeyCombo = JSUtil.KeyCombo;
open GeneralUtil;
open ViewUtil;
open Sexplib.Std;

let string_insert = (s1, offset, s2) => {
  let prefix = String.sub(s1, 0, offset);
  let length = String.length(s1);
  let suffix = String.sub(s1, offset, length - offset);
  prefix ++ s2 ++ suffix;
};

let string_backspace = (s, offset, ctrlKey) => {
  let prefix = ctrlKey ? "" : String.sub(s, 0, offset - 1);
  let length = String.length(s);
  let suffix = String.sub(s, offset, length - offset);
  let offset' = ctrlKey ? 0 : offset - 1;
  (prefix ++ suffix, offset');
};

let string_delete = (s, offset, ctrlKey) => {
  let prefix = String.sub(s, 0, offset);
  let length = String.length(s);
  let suffix = ctrlKey ? "" : String.sub(s, offset + 1, length - offset - 1);
  (prefix ++ suffix, offset);
};

let kc_actions: Hashtbl.t(KeyCombo.t, CursorInfo.node => Action.t) =
  Hashtbl.of_seq(
    [
      (KeyCombo.Backspace, _ => Action.Backspace),
      (KeyCombo.Delete, _ => Action.Delete),
      (KeyCombo.ShiftTab, _ => Action.MoveToPrevHole),
      (KeyCombo.Tab, _ => Action.MoveToNextHole),
      (KeyCombo.Key_N, _ => Action.Construct(SNum)),
      (KeyCombo.Key_B, _ => Action.Construct(SBool)),
      (
        KeyCombo.GT,
        fun
        | CursorInfo.Typ(_) => Action.Construct(SOp(SArrow))
        | _ => Action.Construct(SOp(SGreaterThan)),
      ),
      (KeyCombo.Ampersand, _ => Action.Construct(SOp(SAnd))),
      (KeyCombo.VBar, _ => Action.Construct(SOp(SOr))),
      (KeyCombo.Key_L, _ => Action.Construct(SList)),
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
    |> List.to_seq,
  );
let entered_single_key =
    (
      ~prevent_stop_inject,
      ci: CursorInfo.t,
      single_key: JSUtil.single_key,
      opt_kc: option(KeyCombo.t),
    )
    : option(Vdom.Event.t) =>
  switch (ci.node, opt_kc) {
  | (Typ(_), Some((Key_B | Key_L | Key_N) as kc)) =>
    Some(
      prevent_stop_inject(
        Update.Action.EditAction(ci.node |> Hashtbl.find(kc_actions, kc)),
      ),
    )
  | (Pat(EmptyHole(_)), _) =>
    let shape =
      switch (single_key) {
      | Number(n) => Action.SNumLit(n, OnText(num_digits(n)))
      | Letter(x) => Action.SVar(x, OnText(Var.length(x)))
      | Underscore => Action.SWild
      };
    Some(prevent_stop_inject(Update.Action.EditAction(Construct(shape))));
  | (Pat(Wild(_)), _) =>
    let shape =
      switch (single_key) {
      | Number(n) =>
        Action.SVar("_" ++ string_of_int(n), OnText(num_digits(n) + 1))
      | Letter(x) => Action.SVar("_" ++ x, OnText(Var.length(x) + 1))
      | Underscore => Action.SVar("__", OnText(2))
      };
    Some(prevent_stop_inject(Update.Action.EditAction(Construct(shape))));
  | (Line(EmptyLine | ExpLine(EmptyHole(_))) | Exp(EmptyHole(_)), _) =>
    let shape =
      switch (single_key) {
      | Number(n) => Action.SNumLit(n, OnText(num_digits(n)))
      | Letter(x) => Action.SVar(x, OnText(Var.length(x)))
      | Underscore => Action.SVar("_", OnText(1))
      };
    Some(prevent_stop_inject(Update.Action.EditAction(Construct(shape))));
  | (
      Exp(NumLit(_, _) | BoolLit(_, _) | Var(_, _, _)) |
      Pat(NumLit(_, _) | BoolLit(_, _) | Var(_, _, _)),
      _,
    ) =>
    let (nodeValue, anchorOffset) =
      switch (ci.node, ci.position) {
      | (Exp(NumLit(_, n)) | Pat(NumLit(_, n)), OnText(j)) => (
          string_of_int(n),
          j,
        )
      | (Exp(BoolLit(_, b)) | Pat(BoolLit(_, b)), OnText(j)) => (
          b ? "true" : "false",
          j,
        )
      | (Exp(Var(_, _, x)) | Pat(Var(_, _, x)), OnText(j)) => (x, j)
      | (_, _) => assert(false)
      };
    let key_string = JSUtil.single_key_string(single_key);
    let newNodeValue = string_insert(nodeValue, anchorOffset, key_string);
    switch (int_of_string_opt(newNodeValue), single_key) {
    | (Some(_), Underscore) =>
      // OCaml accepts and ignores underscores
      // when parsing ints from strings, we don't
      Some(Vdom.Event.Ignore)
    | (Some(new_n), _) =>
      Some(
        // defensive check in case OCaml is
        // doing any other weird things
        num_digits(new_n) != String.length(newNodeValue)
          ? Vdom.Event.Ignore
          : prevent_stop_inject(
              Update.Action.EditAction(
                Action.Construct(
                  Action.SNumLit(new_n, OnText(anchorOffset + 1)),
                ),
              ),
            ),
      )
    | (None, _) =>
      Some(
        Var.is_valid(newNodeValue)
          ? prevent_stop_inject(
              Update.Action.EditAction(
                Action.Construct(
                  Action.SVar(newNodeValue, OnText(anchorOffset + 1)),
                ),
              ),
            )
          : prevent_stop_inject(Update.Action.InvalidVar(newNodeValue)),
      )
    };
  | (Line(_) | Exp(_) | Rule(_) | Pat(_) | Typ(_), _) => None
  };

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  Vdom.(
    Node.div(
      [
        Attr.id("pp_view"),
        Attr.classes(["ModelExp"]),
        Attr.create(
          "style",
          "font-size: "
          ++ (font_size |> JSUtil.px)
          ++ "; line-height: "
          ++ string_of_float(line_height)
          ++ "; padding: "
          ++ (cell_padding |> JSUtil.px)
          ++ "; border: "
          ++ (cell_border |> JSUtil.px)
          ++ " solid #CCC;",
        ),
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
                  let zblock = model |> Model.zexp;
                  switch (ci.position) {
                  | Staging(_)
                  | OnText(_) => Event.Ignore
                  | OnDelim(_, side) =>
                    let move_cursor =
                      switch (side) {
                      | Before => ZExp.move_cursor_left_zblock
                      | After => ZExp.move_cursor_right_zblock
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
                  model |> Model.zexp |> ZExp.is_after_case_rule,
                  model |> Model.zexp |> ZExp.is_on_user_newlineable_hole,
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
              | (_, _, _, Some(kc)) =>
                prevent_stop_inject(
                  Update.Action.EditAction(
                    ci.node |> Hashtbl.find(kc_actions, kc),
                  ),
                )
              };
            }),
          ],
          [
            model.is_cell_focused
              ? Code.view_of_zblock(
                  ~inject,
                  ~user_newlines=model.user_newlines,
                  model |> Model.zexp,
                )
              : Code.view_of_block(
                  ~inject,
                  ~user_newlines=model.user_newlines,
                  model |> Model.exp,
                ),
            ...CursorIndicators.view(
                 ~is_cell_focused=model.is_cell_focused,
                 ~holes_steps=
                   model |> Model.exp |> CursorIndicators.collect_holes,
                 ~ci=model.cursor_info,
               ),
          ],
        ),
      ],
    )
  );
};

let elem = () => JSUtil.force_get_elem_by_id(cell_id);

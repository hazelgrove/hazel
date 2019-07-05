module Vdom = Virtual_dom.Vdom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module KeyCombo = JSUtil.KeyCombo;
open GeneralUtil;
open ViewUtil;

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

let kc_actions: Hashtbl.t(KeyCombo.t, Action.t) =
  Hashtbl.of_seq(
    [
      (KeyCombo.Backspace, Action.Backspace),
      (KeyCombo.Delete, Action.Delete),
      (KeyCombo.ShiftTab, Action.MoveToPrevHole),
      (KeyCombo.Tab, Action.MoveToNextHole),
      (KeyCombo.Key_N, Action.Construct(SNum)),
      (KeyCombo.Key_B, Action.Construct(SBool)),
      (KeyCombo.GT, Action.Construct(SOp(SArrow))),
      (KeyCombo.VBar, Action.Construct(SOp(SVBar))),
      (KeyCombo.Key_L, Action.Construct(SList)),
      (KeyCombo.LeftParen, Action.Construct(SParenthesized)),
      (KeyCombo.Colon, Action.Construct(SAsc)),
      (KeyCombo.Equals, Action.Construct(SLet)),
      (KeyCombo.Enter, Action.Construct(SLine)),
      (KeyCombo.Backslash, Action.Construct(SLam)),
      (KeyCombo.Plus, Action.Construct(SOp(SPlus))),
      (KeyCombo.Asterisk, Action.Construct(SOp(STimes))),
      (KeyCombo.LT, Action.Construct(SOp(SLessThan))),
      (KeyCombo.Space, Action.Construct(SOp(SSpace))),
      (KeyCombo.Comma, Action.Construct(SOp(SComma))),
      (KeyCombo.LeftBracket, Action.Construct(SListNil)),
      (KeyCombo.Semicolon, Action.Construct(SOp(SCons))),
      (KeyCombo.Alt_L, Action.Construct(SInj(L))),
      (KeyCombo.Alt_R, Action.Construct(SInj(R))),
      (KeyCombo.Alt_C, Action.Construct(SCase)),
    ]
    |> List.to_seq,
  );

let multi_line_seq_indicators = (is_cell_focused, n) =>
  (
    range(n)
    |> List.map(i =>
         Vdom.(
           Node.div(
             [
               Attr.id(seq_node_indicator_id(i)),
               Attr.classes([
                 "node-indicator",
                 is_cell_focused ? "active" : "inactive",
               ]),
             ],
             [],
           )
         )
       )
  )
  @ (
    range(n)
    |> List.map(i =>
         Vdom.(
           Node.div(
             [
               Attr.id(child_indicator_id(i)),
               Attr.classes([
                 "child-indicator",
                 is_cell_focused ? "active" : "inactive",
               ]),
             ],
             [],
           )
         )
       )
  );

let single_line_seq_indicators = is_cell_focused =>
  Vdom.[
    Node.div(
      [
        Attr.id(box_node_indicator_id),
        Attr.classes([
          "node-indicator",
          is_cell_focused ? "active" : "inactive",
        ]),
      ],
      [],
    ),
    ...range(2)
       |> List.map(i =>
            Node.div(
              [
                Attr.id(child_indicator_id(i)),
                Attr.classes([
                  "child-indicator",
                  is_cell_focused ? "active" : "inactive",
                ]),
              ],
              [],
            )
          ),
  ];

let indicators = (model: Model.t) => {
  let is_cell_focused = model.is_cell_focused;
  switch (model.cursor_info.sort) {
  | IsExpr(OpSeq(_, seq) as e) =>
    Code.is_multi_line_exp(e)
      ? multi_line_seq_indicators(
          is_cell_focused,
          OperatorSeq.seq_length(seq),
        )
      : single_line_seq_indicators(is_cell_focused)
  | IsPat(OpSeq(_, seq) as p) =>
    Code.is_multi_line_pat(p)
      ? multi_line_seq_indicators(
          is_cell_focused,
          OperatorSeq.seq_length(seq),
        )
      : single_line_seq_indicators(is_cell_focused)
  | IsType(OpSeq(_, seq) as ty) =>
    Code.is_multi_line_typ(ty)
      ? multi_line_seq_indicators(
          is_cell_focused,
          OperatorSeq.seq_length(seq),
        )
      : single_line_seq_indicators(is_cell_focused)
  | _ => [
      Vdom.(
        Node.div(
          [
            Attr.id(box_node_indicator_id),
            Attr.classes([
              "node-indicator",
              model.is_cell_focused ? "active" : "inactive",
            ]),
          ],
          [],
        )
      ),
      ...{
           let child_indices =
             model.cursor_info |> CursorInfo.child_indices_of_current_node;
           let is_active = i => child_indices |> List.exists(j => j == i);
           model
           |> Model.zblock
           |> ZExp.erase_block
           |> UHExp.max_degree_block
           |> range
           |> List.map(i =>
                Vdom.(
                  Node.div(
                    [
                      Attr.id(child_indicator_id(i)),
                      Attr.classes([
                        "child-indicator",
                        is_active(i) ? "active" : "inactive",
                      ]),
                    ],
                    [],
                  )
                )
              );
         },
    ]
  };
};

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  Vdom.(
    Node.div(
      [
        Attr.id("cell"),
        Attr.create("contenteditable", "true"),
        Attr.on("drop", _ => Event.Prevent_default),
        Attr.on_focus(_ => inject(FocusCell)),
        Attr.on_blur(_ => inject(BlurCell)),
        Attr.on_keypress(evt =>
          JSUtil.is_movement_key(evt)
            ? Event.Many([]) : Event.Prevent_default
        ),
        Attr.on_keydown(evt => {
          let prevent_stop_inject = a =>
            Event.Many([
              Event.Prevent_default,
              Event.Stop_propagation,
              inject(a),
            ]);
          let ci = model.cursor_info;
          switch (JSUtil.is_single_key(evt), KeyCombo.of_evt(evt)) {
          | (None, None) => Event.Ignore
          | (Some(single_key), _) =>
            switch (ci.sort) {
            | IsLine(EmptyLine)
            | IsLine(ExpLine(EmptyHole(_)))
            | IsExpr(EmptyHole(_))
            | IsPat(EmptyHole(_)) =>
              let shape =
                switch (single_key) {
                | Number(n) => Action.SNumLit(n, OnText(num_digits(n)))
                | Letter(x) => Action.SVar(x, OnText(Var.length(x)))
                | Underscore => Action.SWild
                };
              prevent_stop_inject(
                Update.Action.EditAction(Construct(shape)),
              );
            | IsExpr(NumLit(_, _))
            | IsExpr(BoolLit(_, _))
            | IsExpr(Var(_, _, _))
            | IsPat(Var(_, _, _))
            | IsPat(NumLit(_, _))
            | IsPat(BoolLit(_, _)) =>
              let nodeValue = JSUtil.force_get_anchor_node_value();
              let anchorOffset = JSUtil.get_anchor_offset();
              let key_string = JSUtil.single_key_string(single_key);
              let newNodeValue =
                string_insert(nodeValue, anchorOffset, key_string);
              switch (int_of_string_opt(newNodeValue)) {
              | Some(new_n) =>
                prevent_stop_inject(
                  Update.Action.EditAction(
                    Action.Construct(
                      Action.SNumLit(new_n, OnText(anchorOffset + 1)),
                    ),
                  ),
                )
              | None =>
                Var.is_valid(newNodeValue)
                  ? prevent_stop_inject(
                      Update.Action.EditAction(
                        Action.Construct(
                          Action.SVar(
                            newNodeValue,
                            OnText(anchorOffset + 1),
                          ),
                        ),
                      ),
                    )
                  : prevent_stop_inject(
                      Update.Action.InvalidVar(newNodeValue),
                    )
              };
            | IsLine(_)
            | IsExpr(_)
            | IsPat(_)
            | IsType(_) => Event.Ignore
            }
          | (_, Some((Backspace | Delete) as kc)) =>
            let (string_edit, update) =
              switch (kc) {
              | Backspace => (
                  string_backspace,
                  Update.Action.EditAction(Backspace),
                )
              | _ => (string_delete, Update.Action.EditAction(Delete))
              };
            switch (ci |> CursorInfo.is_before_current_node, ci.position) {
            | (true, _)
            | (_, OnDelim(_, _)) => prevent_stop_inject(update)
            | (false, OnText(_)) =>
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
                        Construct(SVar(nodeValue', OnText(anchorOffset'))),
                      ),
                    )
                  : prevent_stop_inject(Update.Action.InvalidVar(nodeValue'))
              };
            };
          | (_, Some(kc)) =>
            prevent_stop_inject(
              Update.Action.EditAction(Hashtbl.find(kc_actions, kc)),
            )
          };
        }),
      ],
      [
        model.is_cell_focused
          ? Code.view_of_zblock(~inject, model |> Model.zblock)
          : Code.view_of_block(~inject, model |> Model.block),
        ...indicators(model),
      ],
    )
  );
};

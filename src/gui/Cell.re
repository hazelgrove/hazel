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

let multi_line_seq_indicators = (is_active, n) =>
  Vdom.[
    Node.div(
      [
        Attr.id(op_node_indicator_id),
        Attr.classes(["node-indicator", is_active ? "active" : "inactive"]),
      ],
      [],
    ),
    ...range(n)
       |> List.map(i =>
            Node.div(
              [
                Attr.id(seq_tm_indicator_id(i)),
                Attr.classes([
                  "term-indicator",
                  is_active ? "active" : "inactive",
                ]),
              ],
              [],
            )
          ),
  ];

let single_line_seq_indicators = is_active => {
  Vdom.[
    Node.div(
      [
        Attr.id(op_node_indicator_id),
        Attr.classes(["node-indicator", is_active ? "active" : "inactive"]),
      ],
      [],
    ),
    Node.div(
      [
        Attr.id(box_tm_indicator_id),
        Attr.classes(["term-indicator", is_active ? "active" : "inactive"]),
      ],
      [],
    ),
  ];
};

let indicators = (model: Model.t) => {
  let is_active =
    model.is_cell_focused && model.cursor_info.node != Line(EmptyLine);
  switch (model.cursor_info.node) {
  | Exp(OpSeq(_, seq) as e) =>
    Code.is_multi_line_exp(e)
      ? multi_line_seq_indicators(is_active, OperatorSeq.seq_length(seq))
      : single_line_seq_indicators(is_active)
  | Pat(OpSeq(_, seq) as p) =>
    Code.is_multi_line_pat(p)
      ? multi_line_seq_indicators(is_active, OperatorSeq.seq_length(seq))
      : single_line_seq_indicators(is_active)
  | Typ(OpSeq(_, seq) as ty) =>
    Code.is_multi_line_typ(ty)
      ? multi_line_seq_indicators(is_active, OperatorSeq.seq_length(seq))
      : single_line_seq_indicators(is_active)
  | _ =>
    Vdom.[
      Node.div(
        [
          Attr.id(box_node_indicator_id),
          Attr.classes(["node-indicator", is_active ? "active" : "inactive"]),
        ],
        [],
      ),
      Node.div(
        [
          Attr.id(box_tm_indicator_id),
          Attr.classes(["term-indicator", is_active ? "active" : "inactive"]),
        ],
        [],
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
              );
         },
    ]
  };
};

let font_size = 20.0;
let line_height = 1.5;
let indicator_padding = font_size *. (line_height -. 1.0) /. 2.0;

let place_box_node_indicator_over_snode_elem = elem => {
  let rect = elem |> JSUtil.get_bounding_rect;
  JSUtil.force_get_elem_by_id(box_node_indicator_id)
  |> JSUtil.place_over_rect({
       top: rect.top -. indicator_padding,
       right: rect.right +. indicator_padding,
       bottom:
         elem |> Code.elem_is_on_last_line
           ? rect.bottom +. indicator_padding
           : rect.bottom -. indicator_padding,
       left: rect.left -. indicator_padding,
     });
};

let place_box_term_indicator = steps => {
  let term_elem = Code.force_get_snode_elem(steps);
  let rect = term_elem |> JSUtil.get_bounding_rect;
  JSUtil.force_get_elem_by_id(box_tm_indicator_id)
  |> JSUtil.place_over_rect({
       top: rect.top -. indicator_padding,
       right: rect.right +. indicator_padding,
       bottom:
         term_elem |> Code.elem_is_on_last_line
           ? rect.bottom +. indicator_padding
           : rect.bottom -. indicator_padding,
       left: rect.left -. indicator_padding,
     });
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
          ++ ";",
        ),
      ],
      [
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
                switch (ci.node) {
                | Line(EmptyLine)
                | Line(ExpLine(EmptyHole(_)))
                | Exp(EmptyHole(_))
                | Pat(EmptyHole(_)) =>
                  let shape =
                    switch (single_key) {
                    | Number(n) => Action.SNumLit(n, OnText(num_digits(n)))
                    | Letter(x) => Action.SVar(x, OnText(Var.length(x)))
                    | Underscore => Action.SWild
                    };
                  prevent_stop_inject(
                    Update.Action.EditAction(Construct(shape)),
                  );
                | Exp(NumLit(_, _))
                | Exp(BoolLit(_, _))
                | Exp(Var(_, _, _))
                | Pat(Var(_, _, _))
                | Pat(NumLit(_, _))
                | Pat(BoolLit(_, _)) =>
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
                | Line(_)
                | Exp(_)
                | Rule(_)
                | Pat(_)
                | Typ(_) => Event.Ignore
                }
              | (_, Some((Backspace | Delete) as kc)) =>
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
                switch (cursor_escaped, ci.position) {
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
        ),
      ],
    )
  );
};

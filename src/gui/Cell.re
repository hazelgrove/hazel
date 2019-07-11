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
                Attr.classes(
                  ["term-indicator", is_active ? "active" : "inactive"]
                  @ (i == 0 ? ["term-indicator-first"] : [])
                  @ (i == n - 1 ? ["term-indicator-last"] : []),
                ),
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
        Attr.classes([
          "term-indicator",
          "term-indicator-first",
          "term-indicator-last",
          is_active ? "active" : "inactive",
        ]),
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
          Attr.classes([
            "term-indicator",
            "term-indicator-first",
            "term-indicator-last",
            is_active ? "active" : "inactive",
          ]),
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
let indicator_padding = font_size *. (line_height -. 1.0) /. 2.0 -. 1.5;

let get_relative_bounding_rect = elem => {
  let cell_rect =
    JSUtil.force_get_elem_by_id(cell_id) |> JSUtil.get_bounding_rect;
  elem
  |> JSUtil.get_bounding_rect(
       ~top_origin=cell_rect.top,
       ~left_origin=cell_rect.left,
     );
};

let indent_of_snode_elem = elem =>
  switch (elem |> JSUtil.get_attr("indent_level")) {
  | None => 0
  | Some(ssexp) =>
    switch (ssexp |> Sexplib.Sexp.of_string |> Code.indent_level_of_sexp) {
    | NotIndentable => 0
    | Indented(m) => m
    | OpPrefix(m, n) => m + n
    }
  };

let place_box_node_indicator_over_snode_elem = (~child_indices, elem) => {
  let rect = elem |> get_relative_bounding_rect;
  let indent = elem |> indent_of_snode_elem;
  JSUtil.force_get_elem_by_id(box_node_indicator_id)
  |> JSUtil.place_over_rect(
       ~indent,
       {
         top: rect.top -. indicator_padding,
         right: rect.right +. indicator_padding,
         bottom:
           elem |> Code.elem_is_on_last_line
             ? rect.bottom +. indicator_padding
             : rect.bottom -. indicator_padding,
         left: rect.left -. indicator_padding,
       },
     );
  switch (elem |> Code.child_elems_of_snode_elem) {
  | None => assert(false)
  | Some(child_elems) =>
    zip(child_indices, child_elems)
    |> List.iter(((i, child_elem)) => {
         let child_rect = child_elem |> get_relative_bounding_rect;
         if (elem
             |> Code.elem_is_multi_line
             && child_elem
             |> Code.snode_elem_occupies_full_sline) {
           let indent = child_elem |> Code.elem_is_multi_line ? indent : 0;
           JSUtil.force_get_elem_by_id(child_indicator_id(i))
           |> JSUtil.place_over_rect(
                ~indent,
                {...child_rect, right: rect.right},
              );
         } else {
           JSUtil.force_get_elem_by_id(child_indicator_id(i))
           |> JSUtil.place_over_rect(child_rect);
         };
       })
  };
};

let place_box_term_indicator = cursor_elem => {
  let steps =
    cursor_elem
    |> JSUtil.get_attr("term")
    |> Opt.get(() => assert(false))
    |> Sexplib.Sexp.of_string
    |> Path.steps_of_sexp;
  let term_elem = Code.force_get_snode_elem(steps);
  let term_rect = term_elem |> get_relative_bounding_rect;
  let indent = term_elem |> indent_of_snode_elem;
  if (term_elem |> Code.snode_elem_is_Block) {
    let all_sline_elems =
      JSUtil.force_get_elem_by_id(cell_id)
      |> Code.sline_elems_of_snode_elem(term_elem);
    let first_sline_index =
      cursor_elem
      |> JSUtil.force_get_parent_elem
      |> Code.line_no_of_sline_elem
      |> Opt.get(() => assert(false));
    let sub_block_rect =
      all_sline_elems
      |> filteri((i, _) => i >= first_sline_index)
      |> List.map(get_relative_bounding_rect)
      |> JSUtil.get_covering_rect;
    JSUtil.force_get_elem_by_id(box_tm_indicator_id)
    |> JSUtil.place_over_rect(
         ~indent,
         {
           top: sub_block_rect.top -. indicator_padding,
           right: sub_block_rect.right +. indicator_padding,
           bottom:
             term_elem |> Code.elem_is_on_last_line
               ? sub_block_rect.bottom +. indicator_padding
               : sub_block_rect.bottom -. indicator_padding,
           left: sub_block_rect.left -. indicator_padding,
         },
       );
  } else {
    JSUtil.force_get_elem_by_id(box_tm_indicator_id)
    |> JSUtil.place_over_rect(
         ~indent,
         {
           top: term_rect.top -. indicator_padding,
           right: term_rect.right +. indicator_padding,
           bottom:
             term_elem |> Code.elem_is_on_last_line
               ? term_rect.bottom +. indicator_padding
               : term_rect.bottom -. indicator_padding,
           left: term_rect.left -. indicator_padding,
         },
       );
  };
};

let place_box_term_indicator_over_single_line_seq = (operand1, operand2) => {
  let rect1 = operand1 |> get_relative_bounding_rect;
  let rect2 = operand2 |> get_relative_bounding_rect;
  JSUtil.force_get_elem_by_id(box_tm_indicator_id)
  |> JSUtil.place_over_rect({
       top: rect1.top -. indicator_padding,
       left: rect1.left -. indicator_padding,
       bottom:
         operand1 |> Code.elem_is_on_last_line
           ? rect2.bottom +. indicator_padding
           : rect2.bottom -. indicator_padding,
       right: rect2.right +. indicator_padding,
     });
};

let place_op_node_indicator_over_op_elem = op_elem => {
  let rect = op_elem |> get_relative_bounding_rect;
  JSUtil.force_get_elem_by_id(op_node_indicator_id)
  |> JSUtil.place_over_rect({
       top: rect.top -. indicator_padding,
       left: rect.left -. indicator_padding,
       bottom:
         op_elem |> Code.elem_is_on_last_line
           ? rect.bottom +. indicator_padding
           : rect.bottom -. indicator_padding,
       right: rect.right +. indicator_padding,
     });
};

let place_multi_line_seq_term_indicator = (steps, (a, b), opseq_elem) => {
  let a_elem = steps @ [a] |> node_id |> JSUtil.force_get_elem_by_id;
  let a_rect = a_elem |> get_relative_bounding_rect;
  JSUtil.force_get_elem_by_id(seq_tm_indicator_id(a))
  |> JSUtil.place_over_rect({
       top: a_rect.top -. indicator_padding,
       left: a_rect.left -. indicator_padding,
       bottom:
         a_elem |> Code.elem_is_on_last_line
           ? a_rect.bottom +. indicator_padding
           : a_rect.bottom -. indicator_padding,
       right: a_rect.right +. indicator_padding,
     });
  let sline_elems =
    JSUtil.force_get_elem_by_id(cell_id)
    |> Code.sline_elems_of_snode_elem(opseq_elem);
  range(~lo=a + 1, b + 1)
  |> List.map(List.nth(sline_elems))
  |> List.iteri((i, sline_elem) => {
       let rect = sline_elem |> get_relative_bounding_rect;
       JSUtil.force_get_elem_by_id(seq_tm_indicator_id(a + 1 + i))
       |> JSUtil.place_over_rect({
            top: rect.top -. indicator_padding,
            left: rect.left -. indicator_padding,
            bottom:
              sline_elem |> Code.elem_is_on_last_line
                ? rect.bottom +. indicator_padding
                : rect.bottom -. indicator_padding,
            right: rect.right +. indicator_padding,
          });
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
            Attr.id(cell_id),
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
              | (Some(single_key), opt_kc) =>
                switch (ci.node, opt_kc) {
                | (Typ(_), Some((Key_B | Key_L | Key_N) as kc)) =>
                  prevent_stop_inject(
                    Update.Action.EditAction(Hashtbl.find(kc_actions, kc)),
                  )
                | (Line(EmptyLine), _)
                | (Line(ExpLine(EmptyHole(_))), _)
                | (Exp(EmptyHole(_)), _)
                | (Pat(EmptyHole(_)), _) =>
                  let shape =
                    switch (single_key) {
                    | Number(n) => Action.SNumLit(n, OnText(num_digits(n)))
                    | Letter(x) => Action.SVar(x, OnText(Var.length(x)))
                    | Underscore => Action.SWild
                    };
                  prevent_stop_inject(
                    Update.Action.EditAction(Construct(shape)),
                  );
                | (Exp(NumLit(_, _)), _)
                | (Exp(BoolLit(_, _)), _)
                | (Exp(Var(_, _, _)), _)
                | (Pat(Var(_, _, _)), _)
                | (Pat(NumLit(_, _)), _)
                | (Pat(BoolLit(_, _)), _) =>
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
                | (Line(_), _)
                | (Exp(_), _)
                | (Rule(_), _)
                | (Pat(_), _)
                | (Typ(_), _) => Event.Ignore
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

let elem = () => JSUtil.force_get_elem_by_id(cell_id);

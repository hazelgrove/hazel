module Vdom = Virtual_dom.Vdom;
type undo_history_group = UndoHistory.undo_history_group;
type undo_history_entry = UndoHistory.undo_history_entry;
type tag_typ =
  | Exp
  | Pat
  | Typ;
let view = (~inject: Update.Action.t => Vdom.Event.t, model: Model.t) => {
  /* a helper function working as an enhanced version of List.map() */

  let rec list_map_helper_func = (func_to_list, func_to_base, base, lst) => {
    switch (lst) {
    | [] => []
    | [head, ...tail] => [
        func_to_list(base, head),
        ...list_map_helper_func(
             func_to_list,
             func_to_base,
             func_to_base(base),
             tail,
           ),
      ]
    };
  };

  let exp_str = (exp: UHExp.operand): string => {
    switch (exp) {
    | EmptyHole(meta_var) => "hole: " ++ string_of_int(meta_var)
    | Var(_, _, var_str) => "var: " ++ var_str
    | IntLit(_, num) => "Int: " ++ num
    | FloatLit(_, num) => "Float: " ++ num
    | BoolLit(_, bool_val) => "Bool: " ++ string_of_bool(bool_val)
    | ListNil(_) => "empty list"
    | Lam(_, _, _, _) => "lambada function"
    | Inj(_, side, _) =>
      switch (side) {
      | L => "left injection"
      | R => "right injection"
      }
    | Case(_, _, _, _) => "case"
    | Parenthesized(_) => "( )"
    | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
    };
  };

  let pat_str = (pat: UHPat.operand): string => {
    switch (pat) {
    | EmptyHole(meta_var) => "hole: " ++ string_of_int(meta_var)
    | Wild(_) => "wild card"
    | Var(_, _, var_str) =>
      if (Var.is_case(var_str) || Var.is_let(var_str)) {
        "keyword: " ++ var_str;
      } else {
        "var: " ++ var_str;
      }
    | IntLit(_, num) => "Int: " ++ num
    | FloatLit(_, num) => "Float: " ++ num
    | BoolLit(_, bool_val) => "Bool: " ++ string_of_bool(bool_val)
    | ListNil(_) => "empty list"
    | Parenthesized(_) => "parentheses"
    | Inj(_, side, _) =>
      switch (side) {
      | L => "left injection"
      | R => "right injection"
      }
    };
  };

  let typ_str = (typ: UHTyp.operand): string => {
    switch (typ) {
    | Hole => "type: Hole"
    | Unit => "type: Unit"
    | Int => "type: Int"
    | Float => "type: Float"
    | Bool => "type: Bool"
    | Parenthesized(_) => "parentheses"
    | List(_) => "[ ]"
    };
  };

  let display_string_of_cursor_term =
      (cursor_term: CursorInfo.cursor_term): string => {
    switch (cursor_term) {
    | Exp(_, exp) => exp_str(exp)
    | Pat(_, pat) => pat_str(pat)
    | Typ(_, typ) => typ_str(typ)
    | ExpOp(_, op) => UHExp.string_of_operator(op)
    | PatOp(_, op) => UHPat.string_of_operator(op)
    | TypOp(_, op) => UHTyp.string_of_operator(op)
    | Line(_, line_content) =>
      switch (line_content) {
      | EmptyLine => "empty line"
      | LetLine(_, _, _) => "let binding"
      | ExpLine(_) => "expression line"
      }
    | Rule(_, _) => "case rule"
    };
  };

  let display_string_of_history_entry =
      (undo_history_entry: undo_history_entry): option(string) => {
    switch (undo_history_entry.edit_action) {
    | DeleteEdit(edit_detail) =>
      switch (edit_detail) {
      | Term(cursor_term) =>
        Some("delete " ++ display_string_of_cursor_term(cursor_term))
      | Space => Some("delete space")
      | EmptyLine => Some("delete empty line")
      | TypeAnn => Some("delete type annotation")
      }
    | ConstructEdit(edit_detail) =>
      switch (edit_detail) {
      | SLet => Some("construct let binding")
      | SCase => Some("construct case")
      | SLam => Some("construct lambda")
      | _ => Some("insert " ++ Action.shape_to_string(edit_detail))
      }
    | Var(var_edit) =>
      switch (var_edit) {
      | Edit =>
        Some(
          "edit "
          ++ display_string_of_cursor_term(
               undo_history_entry.cursor_term_info.cursor_term_after,
             ),
        )
      | Insert =>
        Some(
          "insert "
          ++ display_string_of_cursor_term(
               undo_history_entry.cursor_term_info.cursor_term_after,
             ),
        )
      }
    | MatchRule => Some("insert a case rule")
    | Init => Some("initial state")
    | Ignore => None
    };
  };

  let get_cursor_term_tag_typ = (cursor_term: CursorInfo.cursor_term): tag_typ => {
    switch (cursor_term) {
    | Exp(_, _) => Exp
    | Pat(_, _) => Pat
    | Typ(_, _) => Typ
    | ExpOp(_, _) => Exp
    | PatOp(_, _) => Pat
    | TypOp(_, _) => Typ
    | Line(_, _)
    | Rule(_, _) => Exp
    };
  };
  let display_tag_typ =
      (undo_history_entry: undo_history_entry): option(tag_typ) => {
    switch (undo_history_entry.edit_action) {
    | DeleteEdit(edit_detail) =>
      switch (edit_detail) {
      | Term(cursor_term) => Some(get_cursor_term_tag_typ(cursor_term))
      | Space
      | EmptyLine =>
        Some(
          get_cursor_term_tag_typ(
            undo_history_entry.cursor_term_info.cursor_term_before,
          ),
        )
      | TypeAnn => Some(Exp)
      }
    | ConstructEdit(edit_detail) =>
      switch (edit_detail) {
      | SLet
      | SCase
      | SLam => Some(Exp)
      | _ =>
        Some(
          get_cursor_term_tag_typ(
            undo_history_entry.cursor_term_info.cursor_term_after,
          ),
        )
      }
    | Var(_) =>
      Some(
        get_cursor_term_tag_typ(
          undo_history_entry.cursor_term_info.cursor_term_after,
        ),
      )
    | MatchRule => Some(Exp)
    | Init => None
    | Ignore => None
    };
  };
  let history_entry_tab_icon =
      (group_id: int, has_hidden_part: bool, is_expanded: bool) => {
    let icon =
      if (is_expanded) {
        Icons.down_arrow(["entry-tab-icon", "history-tab-icon"]);
      } else {
        Icons.left_arrow(["entry-tab-icon", "history-tab-icon"]);
      };
    if (has_hidden_part) {
      /* expand icon*/
      Vdom.(
        Node.div(
          [
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(Update.Action.ToggleHistoryGroup(group_id)),
                inject(FocusCell),
              ])
            ),
          ],
          [icon],
        )
      );
    } else {
      /* no expand icon if there is no hidden part */
      Vdom.(Node.div([], []));
    };
  };
  let timestamp_view = (undo_history_entry: undo_history_entry) => {
    let hour = Unix.localtime(undo_history_entry.timestamp).tm_hour;
    /* if there is only 1 digit in sec/min/hour, it should be expanded into two digits */
    let str_hour =
      if (hour < 10) {
        "0" ++ string_of_int(hour);
      } else {
        string_of_int(hour);
      };
    let min = Unix.localtime(undo_history_entry.timestamp).tm_min;
    let str_min =
      if (min < 10) {
        "0" ++ string_of_int(min);
      } else {
        string_of_int(min);
      };
    let sec = Unix.localtime(undo_history_entry.timestamp).tm_sec;
    let str_sec =
      if (sec < 10) {
        "0" ++ string_of_int(sec);
      } else {
        string_of_int(sec);
      };
    Vdom.(
      Node.div(
        [Attr.classes(["timestamp-txt"])],
        [
          Node.span(
            [],
            [Node.text(str_hour ++ ":" ++ str_min ++ ":" ++ str_sec)],
          ),
        ],
      )
    );
  };
  let history_typ_tag_view = (undo_history_entry: undo_history_entry) => {
    switch (display_tag_typ(undo_history_entry)) {
    | None => Vdom.(Node.div([], []))
    | Some(typ) =>
      switch (typ) {
      | Exp =>
        Vdom.(
          Node.div(
            [Attr.classes(["history-type-tag", "history-type-tag-exp"])],
            [Node.text("EXP")],
          )
        )
      | Pat =>
        Vdom.(
          Node.div(
            [Attr.classes(["history-type-tag", "history-type-tag-pat"])],
            [Node.text("PAT")],
          )
        )
      | Typ =>
        Vdom.(
          Node.div(
            [Attr.classes(["history-type-tag", "history-type-tag-typ"])],
            [Node.text("TYP")],
          )
        )
      }
    };
  };
  let history_title_entry_view =
      (
        ~cur_history: UndoHistory.t,
        ~is_latest_selected: bool,
        ~is_expanded: bool,
        ~has_hidden_part: bool,
        group_id: int,
        elt_id: int,
        undo_history_entry: undo_history_entry,
      ) => {
    switch (display_string_of_history_entry(undo_history_entry)) {
    | None =>
      Vdom.(
        Node.div(
          if (is_latest_selected) {
            [Attr.id("cur-selected-entry")];
          } else {
            [];
          },
          [],
        )
      )
    | Some(str) =>
      Vdom.(
        Node.div(
          if (is_latest_selected) {
            [
              Attr.classes([
                "the-history-title",
                "always-display-history-entry",
              ]),
              Attr.id("cur-selected-entry"),
            ];
          } else {
            [
              Attr.classes([
                "the-history-title",
                "always-display-history-entry",
              ]),
            ];
          },
          [
            Node.div(
              [Attr.classes(["the-history-entry"])],
              [
                Node.div(
                  [
                    Attr.classes(["the-history-title-entry"]),
                    Attr.on_click(_ =>
                      Vdom.Event.Many([
                        inject(Update.Action.ShiftHistory(group_id, elt_id)),
                        inject(FocusCell),
                      ])
                    ),
                    Attr.on_mouseover(_ =>
                      Vdom.Event.Many([
                        inject(Update.Action.ShiftHistory(group_id, elt_id)),
                        inject(FocusCell),
                      ])
                    ),
                    Attr.on_mouseout(_ =>
                      Vdom.Event.Many([
                        inject(Update.Action.RecoverHistory(cur_history)),
                        inject(FocusCell),
                      ])
                    ),
                  ],
                  [
                    history_typ_tag_view(undo_history_entry),
                    Node.text(str),
                    Node.div(
                      [Attr.classes(["history-entry-right"])],
                      [
                        timestamp_view(undo_history_entry),
                        history_entry_tab_icon(
                          group_id,
                          has_hidden_part,
                          is_expanded,
                        ),
                      ],
                    ),
                  ],
                ),
              ],
            ),
          ],
        )
      )
    };
  };

  let history_hidden_entry_view =
      (
        ~cur_history: UndoHistory.t,
        ~is_latest_selected: bool,
        group_id: int,
        elt_id: int,
        undo_history_entry: undo_history_entry,
      ) => {
    switch (display_string_of_history_entry(undo_history_entry)) {
    | None =>
      Vdom.(
        Node.div(
          if (is_latest_selected) {
            [Attr.id("cur-selected-entry")];
          } else {
            [];
          },
          [],
        )
      )
    | Some(str) =>
      if (is_latest_selected) {
        Vdom.(
          Node.div(
            [Attr.classes(["the-history-entry"])],
            [
              Node.div(
                [
                  Attr.classes(["the-hidden-history-entry"]),
                  Attr.id("cur-selected-entry"),
                  Attr.on_click(_ =>
                    Vdom.Event.Many([
                      inject(Update.Action.ShiftHistory(group_id, elt_id)),
                      inject(FocusCell),
                    ])
                  ),
                  Attr.on_mouseover(_ =>
                    Vdom.Event.Many([
                      inject(Update.Action.ShiftHistory(group_id, elt_id)),
                      inject(FocusCell),
                    ])
                  ),
                  Attr.on_mouseout(_ =>
                    Vdom.Event.Many([
                      inject(Update.Action.RecoverHistory(cur_history)),
                      inject(FocusCell),
                    ])
                  ),
                ],
                [
                  Node.span(
                    [Attr.classes(["the-hidden-history-txt"])],
                    [
                      history_typ_tag_view(undo_history_entry),
                      Node.text(str),
                    ],
                  ),
                  Node.div(
                    [Attr.classes(["history-entry-right"])],
                    [timestamp_view(undo_history_entry)],
                  ),
                ],
              ),
            ],
          )
        );
      } else {
        Vdom.(
          Node.div(
            [Attr.classes(["the-history-entry"])],
            [
              Node.div(
                [
                  Attr.classes(["the-hidden-history-entry"]),
                  Attr.on_click(_ =>
                    Vdom.Event.Many([
                      inject(Update.Action.ShiftHistory(group_id, elt_id)),
                      inject(FocusCell),
                    ])
                  ),
                  Attr.on_mouseover(_ =>
                    Vdom.Event.Many([
                      inject(Update.Action.ShiftHistory(group_id, elt_id)),
                      inject(FocusCell),
                    ])
                  ),
                  Attr.on_mouseout(_ =>
                    Vdom.Event.Many([
                      inject(Update.Action.RecoverHistory(cur_history)),
                      inject(FocusCell),
                    ])
                  ),
                ],
                [
                  Node.span(
                    [Attr.classes(["the-hidden-history-txt"])],
                    [
                      history_typ_tag_view(undo_history_entry),
                      Node.text(str),
                    ],
                  ),
                  Node.div(
                    [Attr.classes(["history-entry-right"])],
                    [timestamp_view(undo_history_entry)],
                  ),
                ],
              ),
            ],
          )
        );
      }
    };
  };

  let drop_prefix_undisplay_entries =
      (entries: list(undo_history_entry))
      : (option((undo_history_entry, int)), list(undo_history_entry)) => {
    let rec helper_func =
            (entries: list(undo_history_entry), index: int)
            : (option((undo_history_entry, int)), list(undo_history_entry)) => {
      switch (entries) {
      | [] => (None, [])
      | [head, ...tail] =>
        if (head.edit_action == Ignore) {
          helper_func(tail, index + 1);
        } else {
          (Some((head, index)), tail);
        }
      };
    };
    helper_func(entries, 0);
  };

  let group_view =
      (
        ~cur_history: UndoHistory.t,
        ~is_cur_group: bool,
        group_id: int,
        group: undo_history_group,
      ) => {
    /* if the group containning selected history entry, it should be splited into different css styles */
    let suc_his_classes =
      if (is_cur_group) {
        ["the-suc-history"];
      } else {
        [];
      };
    let prev_his_classes =
      if (is_cur_group) {
        ["the-prev-history"];
      } else {
        [];
      };
    let cur_his_classes =
      if (is_cur_group) {
        ["the-cur-history"];
      } else {
        [];
      };

    switch (group.group_entries) {
    | ([], cur_entry, prev_entries) =>
      let (title, hidden_entries) =
        drop_prefix_undisplay_entries([cur_entry] @ prev_entries);
      switch (title) {
      | None => Vdom.(Node.div([Attr.id("cur-selected-entry")], []))
      | Some((title_entry, start_index)) =>
        let has_hidden_part = List.length(hidden_entries) > 0;
        let title_class =
          if (start_index != 0) {
            prev_his_classes;
          } else {
            cur_his_classes;
          };
        if (group.is_expanded) {
          Vdom.(
            Node.div(
              [],
              [
                /* title entry */
                Vdom.(
                  Node.div(
                    [Attr.classes(title_class)],
                    [
                      history_title_entry_view(
                        ~cur_history,
                        ~is_latest_selected=is_cur_group,
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        start_index /*elt_id*/,
                        title_entry,
                      ),
                    ] /* hidden entries */,
                  )
                ),
                Vdom.(
                  Node.div(
                    [
                      Attr.classes(
                        ["hidden-history-entry"] @ prev_his_classes,
                      ),
                    ],
                    list_map_helper_func(
                      history_hidden_entry_view(
                        ~cur_history,
                        ~is_latest_selected=false,
                        group_id,
                      ),
                      base => base + 1,
                      start_index + 1 /* base elt_id is 1, because there is a title entry with elt_id=0 before */,
                      hidden_entries,
                    ),
                  )
                ),
              ],
            )
          );
        } else {
          /* if the group is not expanded, only title entry is displayed */
          Vdom.(
            Node.div(
              [],
              [
                Vdom.(
                  Node.div(
                    [Attr.classes(cur_his_classes)],
                    [
                      history_title_entry_view(
                        ~cur_history,
                        ~is_latest_selected=is_cur_group,
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        start_index /*elt_id*/,
                        title_entry,
                      ),
                    ],
                  )
                ),
              ],
            )
          );
        };
      };
    | (suc_entries, cur_entry, prev_entries) =>
      let (title, hidden_entries) =
        drop_prefix_undisplay_entries(
          suc_entries @ [cur_entry] @ prev_entries,
        );
      switch (title) {
      | None => Vdom.(Node.div([Attr.id("cur-selected-entry")], []))
      | Some((title_entry, start_index)) =>
        /* title entry is in suc_entries */
        let has_hidden_part = List.length(hidden_entries) > 0;
        if (start_index + 1 <= List.length(suc_entries)) {
          let suc_entries' = ListUtil.drop(start_index + 1, suc_entries);
          if (group.is_expanded) {
            Vdom.(
              Node.div(
                [],
                [
                  /* the history title entry */
                  Vdom.(
                    Node.div(
                      [Attr.classes(suc_his_classes)],
                      [
                        history_title_entry_view(
                          ~cur_history,
                          ~is_latest_selected=false,
                          ~is_expanded=group.is_expanded,
                          ~has_hidden_part,
                          group_id,
                          start_index /*elt_id*/,
                          title_entry,
                        ),
                      ],
                    )
                  ),
                  /* the successor history entry */
                  Vdom.(
                    Node.div(
                      [
                        Attr.classes(
                          ["hidden-history-entry"] @ suc_his_classes,
                        ),
                      ],
                      list_map_helper_func(
                        history_hidden_entry_view(
                          ~cur_history,
                          ~is_latest_selected=false,
                          group_id,
                        ),
                        base => base + 1,
                        start_index + 1 /* base elt_id is 1, because there is a title entry with elt_id=0 ahead */,
                        suc_entries',
                      ),
                    )
                  ),
                  /* the selected(current) history entry */
                  Vdom.(
                    Node.div(
                      [
                        Attr.classes(
                          ["hidden-history-entry"] @ cur_his_classes,
                        ),
                      ],
                      [
                        history_hidden_entry_view(
                          ~cur_history,
                          ~is_latest_selected=is_cur_group,
                          group_id,
                          start_index + 1 + List.length(suc_entries') /* elt_id */,
                          cur_entry,
                        ),
                      ],
                    )
                  ),
                  /* the previous history entry */
                  Vdom.(
                    Node.div(
                      [
                        Attr.classes(
                          ["hidden-history-entry"] @ prev_his_classes,
                        ),
                      ],
                      list_map_helper_func(
                        history_hidden_entry_view(
                          ~cur_history,
                          ~is_latest_selected=false,
                          group_id,
                        ),
                        base => base + 1,
                        start_index + 1 + List.length(suc_entries') + 1 /* base elt_id */,
                        prev_entries,
                      ),
                    )
                  ),
                ],
              )
            );
          } else {
            Vdom.(
              Node.div(
                [],
                [
                  Vdom.(
                    Node.div(
                      [Attr.classes(suc_his_classes)],
                      [
                        history_title_entry_view(
                          ~cur_history,
                          ~is_latest_selected=is_cur_group,
                          ~is_expanded=group.is_expanded,
                          ~has_hidden_part=true,
                          group_id,
                          start_index /*elt_id*/,
                          title_entry,
                        ),
                      ],
                    )
                  ),
                ],
              )
            );
          };
        } else if (start_index == List.length(suc_entries)) {
          if (group.is_expanded) {
            Vdom.(
              Node.div(
                [],
                [
                  /* title entry */
                  Vdom.(
                    Node.div(
                      [Attr.classes(cur_his_classes)],
                      [
                        history_title_entry_view(
                          ~cur_history,
                          ~is_latest_selected=is_cur_group,
                          ~is_expanded=group.is_expanded,
                          ~has_hidden_part,
                          group_id,
                          start_index /*elt_id*/,
                          title_entry,
                        ),
                      ],
                    )
                  ),
                  /* hidden entries */
                  Vdom.(
                    Node.div(
                      [
                        Attr.classes(
                          ["hidden-history-entry"] @ prev_his_classes,
                        ),
                      ],
                      list_map_helper_func(
                        history_hidden_entry_view(
                          ~cur_history,
                          ~is_latest_selected=false,
                          group_id,
                        ),
                        base => base + 1,
                        start_index + 1 /* base elt_id is 1, because there is a title entry with elt_id=0 before */,
                        hidden_entries,
                      ),
                    )
                  ),
                ],
              )
            );
          } else {
            /* if the group is not expanded, only title entry is displayed */
            Vdom.(
              Node.div(
                [],
                [
                  Vdom.(
                    Node.div(
                      [Attr.classes(cur_his_classes)],
                      [
                        history_title_entry_view(
                          ~cur_history,
                          ~is_latest_selected=is_cur_group,
                          ~is_expanded=group.is_expanded,
                          ~has_hidden_part,
                          group_id,
                          start_index /*elt_id*/,
                          cur_entry,
                        ),
                      ],
                    )
                  ),
                ],
              )
            );
          };
        } else if (group.is_expanded) {
          Vdom.(
            Node.div(
              [],
              [
                /* title entry */
                Vdom.(
                  Node.div(
                    [Attr.classes(prev_his_classes)],
                    [
                      history_title_entry_view(
                        ~cur_history,
                        ~is_latest_selected=is_cur_group,
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        start_index /*elt_id*/,
                        title_entry,
                      ),
                    ],
                  )
                ),
                /* hidden entries */
                Vdom.(
                  Node.div(
                    [
                      Attr.classes(
                        ["hidden-history-entry"] @ prev_his_classes,
                      ),
                    ],
                    list_map_helper_func(
                      history_hidden_entry_view(
                        ~cur_history,
                        ~is_latest_selected=false,
                        group_id,
                      ),
                      base => base + 1,
                      start_index + 1 /* base elt_id is 1, because there is a title entry with elt_id=0 before */,
                      hidden_entries,
                    ),
                  )
                ),
              ],
            )
          );
        } else {
          /* if the group is not expanded, only title entry is displayed */
          Vdom.(
            Node.div(
              [],
              [
                Vdom.(
                  Node.div(
                    [Attr.classes(prev_his_classes)],
                    [
                      history_title_entry_view(
                        ~cur_history,
                        ~is_latest_selected=is_cur_group,
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        start_index /*elt_id*/,
                        title_entry,
                      ),
                    ],
                  )
                ),
              ],
            )
          );
        };
      };
    };
  };

  let prev_history_view = (history: UndoHistory.t) => {
    let (suc_groups, _, prev_groups) = history.groups;
    Vdom.(
      Node.div(
        [Attr.classes(["the-prev-history"])],
        list_map_helper_func(
          group_view(~cur_history=history, ~is_cur_group=false),
          base => base + 1,
          List.length(suc_groups) + 1,
          prev_groups,
        ),
      )
    );
  };
  let suc_history_view = (history: UndoHistory.t) => {
    let (suc_groups, _, _) = history.groups;
    Vdom.(
      Node.div(
        [Attr.classes(["the-suc-history"])],
        list_map_helper_func(
          group_view(~cur_history=history, ~is_cur_group=false),
          base => base + 1,
          0,
          suc_groups,
        ),
      )
    );
  };
  let cur_history_view = (history: UndoHistory.t) => {
    let (suc_groups, cur_group, _) = history.groups;
    Vdom.(
      Node.div(
        [],
        [
          group_view(
            ~cur_history=history,
            ~is_cur_group=true,
            List.length(suc_groups),
            cur_group,
          ),
        ],
      )
    );
  };
  let history_view = (model: Model.t) =>
    if (UndoHistory.is_empty(model.undo_history)) {
      Vdom.(
        Node.div(
          [Attr.classes(["the-history"])],
          [
            Vdom.(
              Node.div(
                [Attr.classes(["history-is-empty-msg"])],
                [Node.text("no history in scope")],
              )
            ),
          ],
        )
      );
    } else {
      Vdom.(
        Node.div(
          [Attr.classes(["the-history"])],
          [
            suc_history_view(model.undo_history),
            cur_history_view(model.undo_history),
            prev_history_view(model.undo_history),
          ],
        )
      );
    };
  let undo_button =
    Vdom.(
      Node.div(
        [
          Attr.classes(["history-button"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([inject(Update.Action.Undo), inject(FocusCell)])
          ),
        ],
        [Icons.undo(["redo-undo-icon"])],
      )
    );

  let redo_button =
    Vdom.(
      Node.div(
        [
          Attr.classes(["history-button"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([inject(Update.Action.Redo), inject(FocusCell)])
          ),
        ],
        [Icons.redo(["redo-undo-icon"])],
      )
    );

  let expand_button = (all_hidden_history_expand: bool) => {
    let icon =
      if (all_hidden_history_expand) {
        Icons.down_arrow(["all-history-tab-icon", "history-tab-icon"]);
      } else {
        Icons.left_arrow(["all-history-tab-icon", "history-tab-icon"]);
      };
    Vdom.(
      Node.div(
        [
          Attr.classes(["history-button", "all-history-tab-icon-wrapper"]),
          Attr.on_click(_ =>
            Vdom.Event.Many([
              inject(Update.Action.ToggleHiddenHistoryAll),
              inject(FocusCell),
            ])
          ),
        ],
        [icon],
      )
    );
  };

  let button_bar_view = (all_hidden_history_expand: bool) =>
    Vdom.(
      Node.div(
        [Attr.classes(["history_button_bar"])],
        [undo_button, redo_button, expand_button(all_hidden_history_expand)],
      )
    );

  Vdom.(
    Node.div(
      [Attr.classes(["panel", "context-inspector-panel"])],
      [
        Panel.view_of_main_title_bar("history"),
        button_bar_view(model.undo_history.all_hidden_history_expand),
        Node.div(
          [
            Attr.classes(["panel-body", "context-inspector-body"]),
            Attr.id("history-body"),
          ],
          [history_view(model)],
        ),
      ],
    )
  );
};

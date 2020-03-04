module Vdom = Virtual_dom.Vdom;
type undo_history_group = UndoHistory.undo_history_group;
type undo_history_entry = UndoHistory.undo_history_entry;

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
    | NumLit(_, num) => "number: " ++ string_of_int(num)
    | BoolLit(_, bool_val) => "bool: " ++ string_of_bool(bool_val)
    | ListNil(_) => "empty list"
    | Lam(_, _, _, _) => "lambada function"
    | Inj(_, side, _) =>
      switch (side) {
      | L => "left injection"
      | R => "right injection"
      }
    | Case(_, _, _, _) => "case match"
    | Parenthesized(_) => "( )"
    | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
    };
  };

  let pat_str = (pat: UHPat.operand): string => {
    switch (pat) {
    | EmptyHole(meta_var) => "hole: " ++ string_of_int(meta_var)
    | Wild(_) => "I don't know its meaning"
    | Var(_, _, var_str) => "var: " ++ var_str
    | NumLit(_, num) => "number: " ++ string_of_int(num)
    | BoolLit(_, bool_val) => "bool: " ++ string_of_bool(bool_val)
    | ListNil(_) => "empty list"
    | Parenthesized(_) => "( )"
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
    | Num => "type: Num"
    | Bool => "type: Bool"
    | Parenthesized(_) => "( )"
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
    | Rule(_, _) => "match rule"
    };
  };
  /*   let is_op = (cursor_term: CursorInfo.cursor_term): bool => {
         switch (cursor_term) {
         | Exp(_, _)
         | Pat(_, _)
         | Typ(_, _) => false
         | ExpOp(_, _)
         | PatOp(_, _)
         | TypOp(_, _) => true
         | Line(_, _)
         | Rule(_, _) => false
         };
       }; */
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
      | Space => Some("insert space")
      | EmptyLine => Some("insert new line")
      | LetBinding => Some("construct let binding")
      | CaseMatch => Some("construct case match")
      | TypeAnn => Some("insert type annotation")
      | ShapeEdit(shape) => Some("insert " ++ Action.shape_to_string(shape))
      }
    | EditVar =>
      Some(
        "edit "
        ++ display_string_of_cursor_term(
             undo_history_entry.cursor_term_info.current_cursor_term,
           ),
      )
    | Ignore => None
    };
  };
  let history_hidden_entry_view =
      (group_id: int, elt_id: int, undo_history_entry: undo_history_entry) => {
    switch (display_string_of_history_entry(undo_history_entry)) {
    | None => Vdom.(Node.div([], []))
    | Some(str) =>
      Vdom.(
        Node.div(
          [
            Attr.classes(["the-hidden-history-entry"]),
            Attr.on_click(_ =>
              inject(Update.Action.ShiftHistory(group_id, elt_id))
            ),
          ],
          [Node.text(str)],
        )
      )
    };
  };

  let history_entry_tab_icon =
      (group_id: int, has_hidden_part: bool, is_expanded: bool) => {
    let icon_classes =
      if (is_expanded) {
        ["down-triangle", "history-tab-icon"];
      } else {
        ["left-triangle", "history-tab-icon"];
      };
    if (has_hidden_part) {
      /* expand icon*/
      Vdom.(
        Node.div(
          [
            Attr.classes(icon_classes),
            Attr.on_click(_ =>
              inject(Update.Action.ToggleHistoryGroup(group_id))
            ),
          ],
          [],
        )
      );
    } else {
      /* no expand icon if there is no hidden part */
      Vdom.(Node.div([], []));
    };
  };
  let timestamp_view = (undo_history_group: undo_history_group, is_edit: bool) => {
    let hour = Unix.localtime(undo_history_group.timestamp).tm_hour;
    let min = Unix.localtime(undo_history_group.timestamp).tm_min;
    let sec = Unix.localtime(undo_history_group.timestamp).tm_sec;
    if ((hour * 60 * 60 + min * 60 + sec) mod 60 == 0 && !is_edit) {
      Vdom.(
        Node.div(
          [Attr.classes(["timestamp-wrapper"])],
          [
            Node.div(
              [Attr.classes(["timestamp-txt"])],
              [
                Node.text(
                  string_of_int(hour)
                  ++ ":"
                  ++ string_of_int(min)
                  ++ ":"
                  ++ string_of_int(sec),
                ),
              ],
            ),
          ],
        )
      );
    } else {
      Vdom.(Node.div([], []));
    };
  };
  /* The entry which is always displayed*/
  let history_title_entry_view =
      (
        ~is_expanded: bool,
        ~has_hidden_part: bool,
        group_id: int,
        elt_id: int,
        undo_history_entry: undo_history_entry,
      ) => {
    switch (display_string_of_history_entry(undo_history_entry)) {
    | None => Vdom.(Node.div([], []))
    | Some(str) =>
      Vdom.(
        Node.div(
          [
            Attr.classes([
              "the-history-title",
              "always-display-history-entry",
            ]),
          ],
          [
            Node.div(
              [],
              [
                Node.span(
                  [
                    Attr.classes(["the-history-title-txt"]),
                    Attr.on_click(_ =>
                      inject(Update.Action.ShiftHistory(group_id, elt_id))
                    ),
                  ],
                  [Node.text(str)],
                ),
                history_entry_tab_icon(
                  group_id,
                  has_hidden_part,
                  is_expanded,
                ),
              ],
            ),
          ],
        )
      )
    };
  };

  let group_view =
      (~is_cur_group: bool, group_id: int, group: undo_history_group) => {
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
    let clear_undisplay_entries =
        (entries: list(undo_history_entry))
        : (option((undo_history_entry, int)), list(undo_history_entry)) => {
      let rec helper_func =
              (entries: list(undo_history_entry), index: int)
              : (
                  option((undo_history_entry, int)),
                  list(undo_history_entry),
                ) => {
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
    switch (group.group_entries) {
    | ([], cur_entry, prev_entries) =>
      let (title, hidden_entries) =
        clear_undisplay_entries([cur_entry] @ prev_entries);
      switch (title) {
      | None => Vdom.(Node.div([], []))
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
                timestamp_view(group, is_cur_group),
                /* title entry */
                Vdom.(
                  Node.div(
                    [Attr.classes(title_class)],
                    [
                      history_title_entry_view(
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        start_index, /*elt_id*/
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
                      history_hidden_entry_view(group_id),
                      base => base + 1,
                      start_index + 1, /* base elt_id is 1, because there is a title entry with elt_id=0 before */
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
                timestamp_view(group, is_cur_group),
                Vdom.(
                  Node.div(
                    [Attr.classes(cur_his_classes)],
                    [
                      history_title_entry_view(
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        start_index, /*elt_id*/
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
        clear_undisplay_entries(suc_entries @ [cur_entry] @ prev_entries);
      switch (title) {
      | None => Vdom.(Node.div([], []))
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
                  timestamp_view(group, is_cur_group),
                  /* the history title entry */
                  Vdom.(
                    Node.div(
                      [Attr.classes(suc_his_classes)],
                      [
                        history_title_entry_view(
                          ~is_expanded=group.is_expanded,
                          ~has_hidden_part,
                          group_id,
                          start_index, /*elt_id*/
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
                        history_hidden_entry_view(group_id),
                        base => base + 1,
                        start_index + 1, /* base elt_id is 1, because there is a title entry with elt_id=0 ahead */
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
                          group_id,
                          List.length(suc_entries') + 1, /* elt_id */
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
                        history_hidden_entry_view(group_id),
                        base => base + 1,
                        List.length(suc_entries') + 2, /* base elt_id */
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
                  timestamp_view(group, is_cur_group),
                  Vdom.(
                    Node.div(
                      [Attr.classes(suc_his_classes)],
                      [
                        history_title_entry_view(
                          ~is_expanded=group.is_expanded,
                          ~has_hidden_part=true,
                          group_id,
                          start_index, /*elt_id*/
                          title_entry,
                        ),
                      ],
                    )
                  ),
                ],
              )
            );
          };
        } else if (start_index + 1 == List.length(suc_entries) + 1) {
          if (group.is_expanded) {
            Vdom.(
              Node.div(
                [],
                [
                  timestamp_view(group, is_cur_group),
                  /* title entry */
                  Vdom.(
                    Node.div(
                      [Attr.classes(cur_his_classes)],
                      [
                        history_title_entry_view(
                          ~is_expanded=group.is_expanded,
                          ~has_hidden_part,
                          group_id,
                          start_index, /*elt_id*/
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
                        history_hidden_entry_view(group_id),
                        base => base + 1,
                        start_index + 1, /* base elt_id is 1, because there is a title entry with elt_id=0 before */
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
                  timestamp_view(group, is_cur_group),
                  Vdom.(
                    Node.div(
                      [Attr.classes(cur_his_classes)],
                      [
                        history_title_entry_view(
                          ~is_expanded=group.is_expanded,
                          ~has_hidden_part,
                          group_id,
                          start_index, /*elt_id*/
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
                timestamp_view(group, is_cur_group),
                /* title entry */
                Vdom.(
                  Node.div(
                    [Attr.classes(prev_his_classes)],
                    [
                      history_title_entry_view(
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        start_index, /*elt_id*/
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
                      history_hidden_entry_view(group_id),
                      base => base + 1,
                      start_index + 1, /* base elt_id is 1, because there is a title entry with elt_id=0 before */
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
                timestamp_view(group, is_cur_group),
                Vdom.(
                  Node.div(
                    [Attr.classes(prev_his_classes)],
                    [
                      history_title_entry_view(
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        start_index, /*elt_id*/
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

  let prev_history_view =
      (history: list(UndoHistory.undo_history_group), group_id_base: int) => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-prev-history"])],
        list_map_helper_func(
          group_view(~is_cur_group=false),
          base => base + 1,
          group_id_base,
          history,
        ),
      )
    );
  };
  let suc_history_view =
      (history: list(UndoHistory.undo_history_group), group_id_base: int) => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-suc-history"])],
        list_map_helper_func(
          group_view(~is_cur_group=false),
          base => base + 1,
          group_id_base,
          history,
        ),
      )
    );
  };
  let cur_history_view = (history: undo_history_group, group_id_base: int) => {
    Vdom.(
      Node.div([], [group_view(~is_cur_group=true, group_id_base, history)])
    );
  };
  let history_view = (model: Model.t) => {
    let (suc_groups, cur_group, prev_groups) = model.undo_history;
    /*if the initial entry is the only history entry */
    if (ZList.length(model.undo_history) <= 1
        && ZList.length(ZList.prj_z(model.undo_history).group_entries) <= 1) {
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
            suc_history_view(suc_groups, 0),
            cur_history_view(cur_group, List.length(suc_groups)),
            prev_history_view(prev_groups, List.length(suc_groups) + 1),
          ],
        )
      );
    };
  };
  let undo_button =
    Vdom.(
      Node.div(
        [
          Attr.classes(["history-button"]),
          Attr.on_click(_ => inject(Update.Action.Undo)),
        ],
        [
          Node.div(
            [Attr.classes(["undo-button-txt"])],
            [Node.text("Undo")],
          ),
          Node.div([Attr.classes(["undo-button", "redo-undo-icon"])], []),
        ],
      )
    );

  let redo_button =
    Vdom.(
      Node.div(
        [
          Attr.classes(["history-button"]),
          Attr.on_click(_ => inject(Update.Action.Redo)),
        ],
        [
          Node.div([Attr.classes(["redo-button", "redo-undo-icon"])], []),
          Node.div(
            [Attr.classes(["redo-button-txt"])],
            [Node.text("Redo")],
          ),
        ],
      )
    );

  let expand_button = (all_hidden_history_expand: bool) => {
    let icon_classes =
      if (all_hidden_history_expand) {
        ["all-history-tab-icon-open", "history-tab-icon"];
      } else {
        ["all-history-tab-icon-close", "history-tab-icon"];
      };
    Vdom.(
      Node.div(
        [
          Attr.classes(icon_classes),
          Attr.on_click(_ => inject(Update.Action.ToggleHiddenHistoryAll)),
        ],
        [],
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
        button_bar_view(model.all_hidden_history_expand),
        Node.div(
          [Attr.classes(["panel-body", "context-inspector-body"])],
          [history_view(model)],
        ),
      ],
    )
  );
};

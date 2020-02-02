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
  /* special display case for backspace and delete */
  let filter_with_backspace_delete =
      (
        action: Action.t,
        cursor_pos: option(CursorPosition.t),
        otherwise_str: option(string),
        is_empty_line: bool,
      )
      : string => {
    switch (otherwise_str) {
    | None => failwith("Imposiible match, undisplayed undo history entry")
    | Some(str) =>
      switch (cursor_pos) {
      | None =>
        if (action == Backspace) {
          "backspace";
        } else if (action == Delete) {
          "delete";
        } else {
          "edit " ++ str;
        }
      | Some(cursor_pos') =>
        switch (cursor_pos') {
        | OnText(_) =>
          if (is_empty_line) {
            "clear the line";
          } else {
            "edit " ++ str;
          }
        | OnDelim(_, side) =>
          switch (side) {
          | Before =>
            if (action == Delete) {
              "select " ++ str;
            } else {
              "edit " ++ str;
            }
          | After =>
            if (action == Backspace) {
              "select " ++ str;
            } else {
              "edit " ++ str;
            }
          }
        | OnOp(_) => "edit " ++ str
        }
      }
    };
  };
  let display_string_of_cursor_term =
      (cursor_term: option(CursorInfo.cursor_term), action: Action.t): string => {
    switch (cursor_term) {
    | None => filter_with_backspace_delete(action, None, None, false)
    | Some(cursor_term') =>
      switch (cursor_term') {
      | Exp(cursor_pos, exp) =>
        let exp_str =
          switch (exp) {
          | EmptyHole(meta_var) => "hole: " ++ string_of_int(meta_var)
          | Var(_, _, var_str) => "var: " ++ var_str
          | NumLit(_, num) => "number: " ++ string_of_int(num)
          | BoolLit(_, bool_val) => "bool: " ++ string_of_bool(bool_val)
          | ListNil(_) => "empty list"
          | Lam(_, _, _, _) => "lambada function"
          | Inj(_, inj_side, _) =>
            "injection: " ++ InjSide.to_string(inj_side)
          | Case(_, _, _, _) => "case match"
          | Parenthesized(_) => "( )"
          | ApPalette(_, _, _, _) => "I don't know its meaning"
          };
        filter_with_backspace_delete(
          action,
          Some(cursor_pos),
          Some(exp_str),
          false,
        );
      | Pat(cursor_pos, pat) =>
        let pat_str =
          switch (pat) {
          | EmptyHole(meta_var) => "hole: " ++ string_of_int(meta_var)
          | Wild(_) => "I don't know its meaning"
          | Var(_, _, var_str) => "var: " ++ var_str
          | NumLit(_, num) => "number: " ++ string_of_int(num)
          | BoolLit(_, bool_val) => "bool: " ++ string_of_bool(bool_val)
          | ListNil(_) => "empty list"
          | Parenthesized(_) => "( )"
          | Inj(_, inj_side, _) =>
            "injection: " ++ InjSide.to_string(inj_side)
          };
        filter_with_backspace_delete(
          action,
          Some(cursor_pos),
          Some(pat_str),
          false,
        );
      | Typ(cursor_pos, typ) =>
        let typ_str =
          switch (typ) {
          | Hole => "type: Hole"
          | Unit => "type: Unit"
          | Num => "type: Num"
          | Bool => "type: Bool"
          | Parenthesized(_) => "type: (?)"
          | List(_) => "type: [?]"
          };
        filter_with_backspace_delete(
          action,
          Some(cursor_pos),
          Some(typ_str),
          false,
        );
      | ExpOp(cursor_pos, op) =>
        filter_with_backspace_delete(
          action,
          Some(cursor_pos),
          Some("operator: " ++ UHExp.string_of_operator(op)),
          false,
        )
      | PatOp(cursor_pos, op) =>
        filter_with_backspace_delete(
          action,
          Some(cursor_pos),
          Some("operator: " ++ UHPat.string_of_operator(op)),
          false,
        )
      | TypOp(cursor_pos, op) =>
        filter_with_backspace_delete(
          action,
          Some(cursor_pos),
          Some("operator: " ++ UHTyp.string_of_operator(op)),
          false,
        )
      | Line(cursor_pos, line_content) =>
        switch (line_content) {
        | EmptyLine =>
          filter_with_backspace_delete(
            action,
            Some(cursor_pos),
            Some("empty line"),
            true,
          )
        | LetLine(_, _, _) =>
          filter_with_backspace_delete(
            action,
            Some(cursor_pos),
            Some("let binding"),
            false,
          )
        | ExpLine(_) =>
          filter_with_backspace_delete(
            action,
            Some(cursor_pos),
            Some("epression line"),
            false,
          )
        }
      | Rule(cursor_pos, _) =>
        filter_with_backspace_delete(
          action,
          Some(cursor_pos),
          Some("match rule"),
          false,
        )
      }
    };
  };

  let display_string_of_history_entry =
      (undo_history_entry: undo_history_entry): string => {
    let action = undo_history_entry.previous_action;
    let cursor_term = undo_history_entry.cursor_term;
    switch (action) {
    | None => failwith("Imposiible match, undisplayed undo history entry")
    | Some(action') =>
      switch (action') {
      | MoveTo(_)
      | MoveToBefore(_)
      | MoveLeft
      | MoveRight
      | MoveToNextHole
      | MoveToPrevHole
      | UpdateApPalette(_) =>
        failwith("Imposiible match, not undoable actions will not be matched")
      | Delete
      | Backspace => display_string_of_cursor_term(cursor_term, action')
      | Construct(shape) =>
        switch (shape) {
        | SParenthesized => "add ( )"
        | SList => "type List"
        | SAsc => "type inference"
        | SLam => "add lambada"
        | SListNil => "add [ ]"
        | SInj(direction) =>
          switch (direction) {
          | L => "inject left"
          | R => "inject right"
          }
        | SLet => "add let binding"
        | SLine => "add new lines"
        | SCase => "add case"
        | SChar(_)
        | SOp(_) => display_string_of_cursor_term(cursor_term, action')
        | SApPalette(_) => "appalette?"
        }
      }
    };
  };

  let history_hidden_entry_view =
      (group_id: int, elt_id: int, undo_history_entry: undo_history_entry) => {
    switch (undo_history_entry.previous_action) {
    | None => Vdom.(Node.div([], [])) /* entry in initial state should not be displayed */
    | Some(_) =>
      Vdom.(
        Node.div(
          [
            Attr.classes(["the-hidden-history-entry"]),
            Attr.on_click(_ =>
              inject(Update.Action.ShiftHistory(group_id, elt_id))
            ),
          ],
          [Node.text(display_string_of_history_entry(undo_history_entry))],
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
  /* The entry which is always displayed*/
  let history_title_entry_view =
      (
        ~is_expanded: bool,
        ~has_hidden_part: bool,
        group_id: int,
        elt_id: int,
        undo_history_entry: undo_history_entry,
      ) => {
    switch (undo_history_entry.previous_action) {
    | None => Vdom.(Node.div([], [])) /* entry in the initial state should not be displayed */
    | Some(_) =>
      Vdom.(
        Node.div(
          [Attr.classes(["the-history-title"])],
          [
            Node.div(
              [Attr.classes(["the-history-title-entry"])],
              [
                Node.span(
                  [
                    Attr.classes(["the-history-title-txt"]),
                    Attr.on_click(_ =>
                      inject(Update.Action.ShiftHistory(group_id, elt_id))
                    ),
                  ],
                  [
                    Node.text(
                      display_string_of_history_entry(undo_history_entry),
                    ),
                  ],
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
    switch (group.group_entries) {
    | ([], cur_entry, prev_entries) =>
      switch (cur_entry.previous_action) {
      | None => Vdom.(Node.div([], [])) /* the entry in intial state should not be displayed */
      | Some(_) =>
        let has_hidden_part =
          switch (prev_entries) {
          | [] => false
          | _ => true
          };
        if (group.is_expanded) {
          Vdom.(
            Node.div(
              [Attr.classes(["the-history-group"])],
              [
                /* title entry */
                Vdom.(
                  Node.div(
                    [
                      Attr.classes(
                        ["always-display-history-entry"] @ cur_his_classes,
                      ),
                    ],
                    [
                      history_title_entry_view(
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        0, /*elt_id*/
                        cur_entry,
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
                      1, /* base elt_id is 1, because there is a title entry with elt_id=0 before */
                      prev_entries,
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
              [Attr.classes(["the-history-group"])],
              [
                Vdom.(
                  Node.div(
                    [
                      Attr.classes(
                        ["always-display-history-entry"] @ cur_his_classes,
                      ),
                    ],
                    [
                      history_title_entry_view(
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        0, /*elt_id*/
                        cur_entry,
                      ),
                    ],
                  )
                ),
              ],
            )
          );
        };
      }

    | ([title_entry, ...suc_entries], cur_entry, prev_entries) =>
      if (group.is_expanded) {
        Vdom.(
          Node.div(
            [Attr.classes(["the-history-group"])],
            [
              /* the history title entry */
              Vdom.(
                Node.div(
                  [
                    Attr.classes(
                      ["always-display-history-entry"] @ suc_his_classes,
                    ),
                  ],
                  [
                    history_title_entry_view(
                      ~is_expanded=group.is_expanded,
                      ~has_hidden_part=true,
                      group_id,
                      0, /*elt_id*/
                      title_entry,
                    ),
                  ],
                )
              ),
              /* the successor history entry */
              Vdom.(
                Node.div(
                  [
                    Attr.classes(["hidden-history-entry"] @ suc_his_classes),
                  ],
                  list_map_helper_func(
                    history_hidden_entry_view(group_id),
                    base => base + 1,
                    1, /* base elt_id is 1, because there is a title entry with elt_id=0 ahead */
                    suc_entries,
                  ),
                )
              ),
              /* the selected(current) history entry */
              Vdom.(
                Node.div(
                  [
                    Attr.classes(["hidden-history-entry"] @ cur_his_classes),
                  ],
                  [
                    history_hidden_entry_view(
                      group_id,
                      List.length(suc_entries) + 1, /* elt_id */
                      cur_entry,
                    ),
                  ],
                )
              ),
              /* the previous history entry */
              Vdom.(
                Node.div(
                  [
                    Attr.classes(["hidden-history-entry"] @ prev_his_classes),
                  ],
                  list_map_helper_func(
                    history_hidden_entry_view(group_id),
                    base => base + 1,
                    List.length(suc_entries) + 2, /* base elt_id */
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
            [Attr.classes(["the-history-group"])],
            [
              Vdom.(
                Node.div(
                  [
                    Attr.classes(
                      ["always-display-history-entry"] @ suc_his_classes,
                    ),
                  ],
                  [
                    history_title_entry_view(
                      ~is_expanded=group.is_expanded,
                      ~has_hidden_part=true,
                      group_id,
                      0, /*elt_id*/
                      title_entry,
                    ),
                  ],
                )
              ),
            ],
          )
        );
      }
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
    let display_content =
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
    let action = ZList.prj_z(cur_group.group_entries).previous_action;
    switch (action) {
    | None =>
      /*if the initial entry is the only history entry */
      if (List.length(suc_groups) <= 1) {
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
        display_content;
      }
    | Some(_) => display_content
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

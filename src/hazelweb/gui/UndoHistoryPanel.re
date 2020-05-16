module Vdom = Virtual_dom.Vdom;
type undo_history_group = UndoHistory.undo_history_group;
type undo_history_entry = UndoHistory.undo_history_entry;
type tag_typ =
  | Exp
  | Pat
  | Typ;

/* just copy OptionsPanel code */
let labeled_checkbox =
    (
      ~id: string,
      ~classes: List.t(string)=[],
      ~label: string,
      ~on_change: unit => Vdom.Event.t,
      ~disabled=false,
      checked: bool,
    )
    : Vdom.Node.t => {
  let checkbox_id = id ++ "_checkbox";
  Vdom.(
    Node.div(
      [Attr.id(id), Attr.classes(["labeled-checkbox", ...classes])],
      [
        Node.input(
          [
            [
              Attr.id(checkbox_id),
              Attr.type_("checkbox"),
              Attr.on_change((_, _) => on_change()),
            ],
            checked ? [Attr.checked] : [],
            disabled ? [Attr.disabled] : [],
          ]
          |> List.concat,
          [],
        ),
        Node.label(
          [
            Attr.for_(id),
            Attr.on_click(_ => on_change()),
            ...disabled ? [Attr.disabled] : [],
          ],
          [Node.text(label)],
        ),
      ],
    )
  );
};
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

  let code_view = (code: string) => {
    Vdom.(
      Node.span([Attr.classes(["panel-code-font"])], [Node.text(code)])
    );
  };
  let code_keywords_view = (code: string) => {
    Vdom.(
      Node.span(
        [Attr.classes(["panel-code-keywords-font"])],
        [Node.text(code)],
      )
    );
  };
  let indicate_words_view = (words: string) => {
    Vdom.(Node.span([], [Node.text(words)]));
  };
  let exp_view = (exp: UHExp.operand) => {
    switch (exp) {
    | EmptyHole(meta_var) =>
      indicate_words_view("hole: " ++ string_of_int(meta_var))

    | Var(_, _, var_str) =>
      if (Var.is_case(var_str) || Var.is_let(var_str)) {
        Vdom.(
          Node.span(
            [],
            [indicate_words_view("keyword: "), code_view(var_str)],
          )
        );
      } else {
        Vdom.(
          Node.span([], [indicate_words_view("var: "), code_view(var_str)])
        );
      }
    | IntLit(_, num) =>
      Vdom.(
        Node.span(
          [],
          [
            code_keywords_view("Int"),
            indicate_words_view(": "),
            code_view(num),
          ],
        )
      )
    | FloatLit(_, num) =>
      Vdom.(
        Node.span(
          [],
          [
            code_keywords_view("Float"),
            indicate_words_view(": "),
            code_view(num),
          ],
        )
      )
    | BoolLit(_, bool_val) =>
      Vdom.(
        Node.span(
          [],
          [
            code_keywords_view("Bool"),
            indicate_words_view(": "),
            code_view(string_of_bool(bool_val)),
          ],
        )
      )
    | ListNil(_) => indicate_words_view("empty list")
    | Lam(_, _, _, _) => indicate_words_view("function")

    | Inj(_, side, _) =>
      switch (side) {
      | L => indicate_words_view("left injection")
      | R => indicate_words_view("right injection")
      }
    | Case(_, _, _, _) => code_keywords_view("case")
    | Parenthesized(_) => indicate_words_view("parentheses")
    | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
    };
  };

  let pat_view = (pat: UHPat.operand) => {
    switch (pat) {
    | EmptyHole(meta_var) =>
      indicate_words_view("hole: " ++ string_of_int(meta_var))
    | Wild(_) => indicate_words_view("wild card")
    | Var(_, _, var_str) =>
      Vdom.(
        Node.span([], [indicate_words_view("var: "), code_view(var_str)])
      )
    | IntLit(_, num) =>
      Vdom.(
        Node.span(
          [],
          [
            code_keywords_view("Int"),
            indicate_words_view(": "),
            code_view(num),
          ],
        )
      )
    | FloatLit(_, num) =>
      Vdom.(
        Node.span(
          [],
          [
            code_keywords_view("Float"),
            indicate_words_view(": "),
            code_view(num),
          ],
        )
      )
    | BoolLit(_, bool_val) =>
      Vdom.(
        Node.span(
          [],
          [
            code_keywords_view("Bool"),
            indicate_words_view(": "),
            code_view(string_of_bool(bool_val)),
          ],
        )
      )
    | ListNil(_) => indicate_words_view("empty list")
    | Parenthesized(_) => indicate_words_view("parentheses")
    | Inj(_, side, _) =>
      switch (side) {
      | L => indicate_words_view("left injection")
      | R => indicate_words_view("right injection")
      }
    };
  };

  let typ_view = (typ: UHTyp.operand) => {
    switch (typ) {
    | Hole =>
      Vdom.(
        Node.span(
          [],
          [indicate_words_view("type: "), code_keywords_view("Hole")],
        )
      )

    | Unit =>
      Vdom.(
        Node.span(
          [],
          [indicate_words_view("type: "), code_keywords_view("Unit")],
        )
      )
    | Int =>
      Vdom.(
        Node.span(
          [],
          [indicate_words_view("type: "), code_keywords_view("Int")],
        )
      )
    | Float =>
      Vdom.(
        Node.span(
          [],
          [indicate_words_view("type: "), code_keywords_view("Float")],
        )
      )
    | Bool =>
      Vdom.(
        Node.span(
          [],
          [indicate_words_view("type: "), code_keywords_view("Bool")],
        )
      )
    | Parenthesized(_) => indicate_words_view("parentheses")
    | List(_) => code_keywords_view("[ ]")
    };
  };

  let cursor_term_view = (cursor_term: CursorInfo.cursor_term) => {
    switch (cursor_term) {
    | Exp(_, exp) => exp_view(exp)
    | Pat(_, pat) => pat_view(pat)
    | Typ(_, typ) => typ_view(typ)
    | ExpOp(_, op) => code_view(UHExp.string_of_operator(op))
    | PatOp(_, op) => code_view(UHPat.string_of_operator(op))
    | TypOp(_, op) => code_view(UHTyp.string_of_operator(op))
    | Line(_, line_content) =>
      switch (line_content) {
      | EmptyLine => indicate_words_view("empty line")
      | LetLine(_, _, _) =>
        Vdom.(
          Node.span(
            [],
            [code_keywords_view("let"), indicate_words_view(" binding")],
          )
        )

      | ExpLine(_) => indicate_words_view("expression line")
      }
    | Rule(_, _) =>
      Vdom.(
        Node.span(
          [],
          [code_keywords_view("case"), indicate_words_view(" rule")],
        )
      )
    };
  };

  let action_shape_view = (shape: Action.shape) => {
    switch (shape) {
    | SLam => indicate_words_view("function")
    | SInj(side) =>
      switch (side) {
      | L => indicate_words_view("left injection")
      | R => indicate_words_view("right injection")
      }
    | SLet =>
      Vdom.(
        Node.span(
          [],
          [code_keywords_view("let"), indicate_words_view(" binding")],
        )
      )
    | SCase =>
      Vdom.(
        Node.span(
          [],
          [code_keywords_view("case"), indicate_words_view(" expression")],
        )
      )
    | SList
    | SListNil
    | SLine
    | SAsc
    | SParenthesized => indicate_words_view(Action.shape_to_string(shape))
    | SChar(_) => code_view(Action.shape_to_string(shape))
    | SOp(op) =>
      switch (op) {
      | SSpace => indicate_words_view("space")
      | _ => code_view(Action.shape_to_string(shape))
      }
    | SApPalette(_) => failwith("ApPalette not implemented")
    };
  };
  let history_entry_txt_view = (undo_history_entry: undo_history_entry) => {
    switch (undo_history_entry.edit_action) {
    | DeleteEdit(edit_detail) =>
      switch (edit_detail) {
      | Term(cursor_term) =>
        Some(
          Vdom.(
            Node.span(
              [],
              [
                indicate_words_view("delete "),
                cursor_term_view(cursor_term),
              ],
            )
          ),
        )
      | Space => Some(indicate_words_view("delete space"))
      | EmptyLine => Some(indicate_words_view("delete empty line"))
      | TypeAnn => Some(indicate_words_view("delete type annotation"))
      }
    | ConstructEdit(edit_detail) =>
      switch (edit_detail) {
      | SLet =>
        Some(
          Vdom.(
            Node.span(
              [],
              [
                indicate_words_view("construct "),
                code_keywords_view("let"),
                indicate_words_view(" binding"),
              ],
            )
          ),
        )
      | SCase =>
        Some(
          Vdom.(
            Node.span(
              [],
              [
                indicate_words_view("construct "),
                code_keywords_view("case"),
              ],
            )
          ),
        )
      | SLam => Some(indicate_words_view("construct function"))

      | _ =>
        Some(
          Vdom.(
            Node.span(
              [],
              [
                indicate_words_view("insert "),
                action_shape_view(edit_detail),
              ],
            )
          ),
        )
      }
    | Var(var_edit) =>
      switch (var_edit) {
      | Edit =>
        Some(
          Vdom.(
            Node.span(
              [],
              [
                indicate_words_view("edit "),
                cursor_term_view(
                  undo_history_entry.cursor_term_info.cursor_term_after,
                ),
              ],
            )
          ),
        )

      | Insert =>
        Some(
          Vdom.(
            Node.span(
              [],
              [
                indicate_words_view("insert "),
                cursor_term_view(
                  undo_history_entry.cursor_term_info.cursor_term_after,
                ),
              ],
            )
          ),
        )
      }
    | MatchRule =>
      Some(
        Vdom.(
          Node.span(
            [],
            [
              indicate_words_view("insert "),
              code_keywords_view("case"),
              indicate_words_view(" rule"),
            ],
          )
        ),
      )
    | Init => Some(indicate_words_view("initial state"))
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
      | SLam
      | SAsc => Some(Exp)
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
                Event.Prevent_default,
                Event.Stop_propagation,
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

  let get_status_class =
      (~cur_group_id: int, ~cur_elt_id: int, ~group_id: int, ~elt_id: int)
      : list(string) =>
    if (cur_group_id > group_id
        || cur_group_id == group_id
        && cur_elt_id > elt_id) {
      ["the-suc-history"];
    } else if (cur_group_id < group_id
               || cur_group_id == group_id
               && cur_elt_id < elt_id) {
      ["the-prev-history"];
    } else {
      ["the-cur-history"];
    };
  let is_latest_selected_entry =
      (~cur_group_id: int, ~cur_elt_id: int, ~group_id: int, ~elt_id: int)
      : bool => {
    cur_group_id == group_id && cur_elt_id == elt_id;
  };

  let history_title_entry_view =
      (
        ~undo_history: UndoHistory.t,
        ~is_expanded: bool,
        ~has_hidden_part: bool,
        group_id: int,
        elt_id: int,
        undo_history_entry: undo_history_entry,
      ) => {
    let status_class =
      get_status_class(
        ~cur_group_id=undo_history.cur_group_id,
        ~cur_elt_id=undo_history.cur_elt_id,
        ~group_id,
        ~elt_id,
      );
    let is_current_entry =
      is_latest_selected_entry(
        ~cur_group_id=undo_history.cur_group_id,
        ~cur_elt_id=undo_history.cur_elt_id,
        ~group_id,
        ~elt_id,
      );
    switch (history_entry_txt_view(undo_history_entry)) {
    | None => Vdom.(Node.div([], []))
    | Some(txt_view) =>
      Vdom.(
        Node.div(
          [Attr.classes(status_class)],
          [
            Node.div(
              if (is_current_entry) {
                [
                  Attr.id("cur-selected-entry"),
                  Attr.classes(["the-history-title"]),
                  Attr.on_click(_ =>
                    Vdom.Event.Many([
                      inject(
                        Update.Action.ShiftHistory(
                          group_id,
                          elt_id,
                          true,
                          false,
                        ),
                      ),
                      inject(FocusCell),
                    ])
                  ),
                  Attr.on_mouseenter(_ =>
                    if (undo_history.show_hover_effect) {
                      Vdom.Event.Many([
                        inject(
                          Update.Action.ShiftHistory(
                            group_id,
                            elt_id,
                            false,
                            true,
                          ),
                        ),
                        inject(FocusCell),
                      ]);
                    } else {
                      Vdom.Event.Many([]);
                    }
                  ),
                  Attr.on_mouseleave(_ =>
                    if (undo_history.show_hover_effect) {
                      Vdom.Event.Many([
                        inject(
                          Update.Action.ShiftHistory(
                            undo_history.hover_recover_group_id,
                            undo_history.hover_recover_elt_id,
                            false,
                            false,
                          ),
                        ),
                        inject(FocusCell),
                      ]);
                    } else {
                      Vdom.Event.Many([]);
                    }
                  ),
                ];
              } else {
                [
                  Attr.classes(["the-history-title"]),
                  Attr.on_click(_ =>
                    Vdom.Event.Many([
                      inject(
                        Update.Action.ShiftHistory(
                          group_id,
                          elt_id,
                          true,
                          false,
                        ),
                      ),
                      inject(FocusCell),
                    ])
                  ),
                  Attr.on_mouseenter(_ =>
                    if (undo_history.show_hover_effect) {
                      Vdom.Event.Many([
                        inject(
                          Update.Action.ShiftHistory(
                            group_id,
                            elt_id,
                            false,
                            true,
                          ),
                        ),
                        inject(FocusCell),
                      ]);
                    } else {
                      Vdom.Event.Many([]);
                    }
                  ),
                  Attr.on_mouseleave(_ =>
                    if (undo_history.show_hover_effect) {
                      Vdom.Event.Many([
                        inject(
                          Update.Action.ShiftHistory(
                            undo_history.hover_recover_group_id,
                            undo_history.hover_recover_elt_id,
                            false,
                            false,
                          ),
                        ),
                        inject(FocusCell),
                      ]);
                    } else {
                      Vdom.Event.Many([]);
                    }
                  ),
                ];
              },
              [
                Node.div(
                  [Attr.classes(["the-history-entry"])],
                  [
                    history_typ_tag_view(undo_history_entry),
                    Node.div(
                      [Attr.classes(["history-entry-left"])],
                      [txt_view],
                    ),
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
        ~undo_history: UndoHistory.t,
        group_id: int,
        elt_id: int,
        undo_history_entry: undo_history_entry,
      ) => {
    let status_class =
      get_status_class(
        ~cur_group_id=undo_history.cur_group_id,
        ~cur_elt_id=undo_history.cur_elt_id,
        ~group_id,
        ~elt_id,
      );
    let is_current_entry =
      is_latest_selected_entry(
        ~cur_group_id=undo_history.cur_group_id,
        ~cur_elt_id=undo_history.cur_elt_id,
        ~group_id,
        ~elt_id,
      );
    switch (history_entry_txt_view(undo_history_entry)) {
    | None => Vdom.(Node.div([], []))
    | Some(txt_view) =>
      Vdom.(
        Node.div(
          [Attr.classes(status_class)],
          [
            Node.div(
              if (is_current_entry) {
                [
                  Attr.classes(["the-hidden-history-entry"]),
                  Attr.id("cur-selected-entry"),
                  Attr.on_click(_ =>
                    Vdom.Event.Many([
                      inject(
                        Update.Action.ShiftHistory(
                          group_id,
                          elt_id,
                          true,
                          false,
                        ),
                      ),
                      inject(FocusCell),
                    ])
                  ),
                  Attr.on_mouseenter(_ =>
                    if (undo_history.show_hover_effect) {
                      Vdom.Event.Many([
                        inject(
                          Update.Action.ShiftHistory(
                            group_id,
                            elt_id,
                            false,
                            true,
                          ),
                        ),
                        inject(FocusCell),
                      ]);
                    } else {
                      Vdom.Event.Many([]);
                    }
                  ),
                  Attr.on_mouseleave(_ =>
                    if (undo_history.show_hover_effect) {
                      Vdom.Event.Many([
                        inject(
                          Update.Action.ShiftHistory(
                            undo_history.hover_recover_group_id,
                            undo_history.hover_recover_elt_id,
                            false,
                            false,
                          ),
                        ),
                        inject(FocusCell),
                      ]);
                    } else {
                      Vdom.Event.Many([]);
                    }
                  ),
                ];
              } else {
                [
                  Attr.classes(["the-hidden-history-entry"]),
                  Attr.on_click(_ =>
                    Vdom.Event.Many([
                      inject(
                        Update.Action.ShiftHistory(
                          group_id,
                          elt_id,
                          true,
                          false,
                        ),
                      ),
                      inject(FocusCell),
                    ])
                  ),
                  Attr.on_mouseenter(_ =>
                    if (undo_history.show_hover_effect) {
                      Vdom.Event.Many([
                        inject(
                          Update.Action.ShiftHistory(
                            group_id,
                            elt_id,
                            false,
                            true,
                          ),
                        ),
                        inject(FocusCell),
                      ]);
                    } else {
                      Vdom.Event.Many([]);
                    }
                  ),
                  Attr.on_mouseleave(_ =>
                    if (undo_history.show_hover_effect) {
                      Vdom.Event.Many([
                        inject(
                          Update.Action.ShiftHistory(
                            undo_history.hover_recover_group_id,
                            undo_history.hover_recover_elt_id,
                            false,
                            false,
                          ),
                        ),
                        inject(FocusCell),
                      ]);
                    } else {
                      Vdom.Event.Many([]);
                    }
                  ),
                ];
              },
              [
                Node.div(
                  [Attr.classes(["the-history-entry"])],
                  [
                    history_typ_tag_view(undo_history_entry),
                    Node.div(
                      [Attr.classes(["history-entry-left"])],
                      [
                        Node.span(
                          [Attr.classes(["the-hidden-history-txt"])],
                          [txt_view],
                        ),
                      ],
                    ),
                    Node.div(
                      [Attr.classes(["history-entry-right"])],
                      [timestamp_view(undo_history_entry)],
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

  let group_view =
      (~undo_history: UndoHistory.t, group_id: int, group: undo_history_group) => {
    let entries = ZList.join(group.group_entries);
    switch (entries) {
    | [] => Vdom.(Node.div([], []))
    | [title_entry, ...hidden_entries] =>
      let has_hidden_part = List.length(hidden_entries) > 0;
      if (group.is_expanded) {
        Vdom.(
          Node.div(
            [],
            [
              /* title entry */
              history_title_entry_view(
                ~undo_history,
                ~is_expanded=group.is_expanded,
                ~has_hidden_part,
                group_id,
                0 /*elt_id*/,
                title_entry,
              ),
              /* hidden entries */
              ...list_map_helper_func(
                   history_hidden_entry_view(~undo_history, group_id),
                   base => base + 1,
                   1 /* base elt_id is 1, because there is a title entry with elt_id=0 before */,
                   hidden_entries,
                 ),
            ],
          )
        );
      } else {
        /* if the group is not expanded, only title entry is displayed */
        history_title_entry_view(
          ~undo_history,
          ~is_expanded=group.is_expanded,
          ~has_hidden_part,
          group_id,
          0 /*elt_id*/,
          title_entry,
        );
      };
    };
  };

  let prev_history_view = (history: UndoHistory.t) => {
    let (suc_groups, _, prev_groups) = history.groups;
    Vdom.(
      Node.div(
        [Attr.classes(["the-prev-history"])],
        list_map_helper_func(
          group_view(~undo_history=history),
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
          group_view(~undo_history=history),
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
            ~undo_history=history,
            List.length(suc_groups),
            cur_group,
          ),
        ],
      )
    );
  };
  let history_view = (model: Model.t) =>
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

  let hover_effect_checkbox = (show_hover_effect: bool) => {
    labeled_checkbox(
      ~id="show_hover_effect",
      ~label="Show Hover Effect",
      ~on_change=() => inject(ToggleShowHoverEffect),
      show_hover_effect,
    );
  };
  let button_bar_view =
      (show_hover_effect: bool, all_hidden_history_expand: bool) =>
    Vdom.(
      Node.div(
        [Attr.classes(["history_button_bar"])],
        [
          hover_effect_checkbox(show_hover_effect),
          expand_button(all_hidden_history_expand),
          redo_button,
          undo_button,
        ],
      )
    );

  Vdom.(
    Node.div(
      [Attr.classes(["panel", "context-inspector-panel"])],
      [
        Panel.view_of_main_title_bar("history"),
        button_bar_view(
          model.undo_history.show_hover_effect,
          model.undo_history.all_hidden_history_expand,
        ),
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

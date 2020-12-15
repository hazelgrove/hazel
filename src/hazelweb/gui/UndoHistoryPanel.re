module Js = Js_of_ocaml.Js;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
type undo_history_group = UndoHistory.undo_history_group;
type undo_history_entry = UndoHistory.undo_history_entry;
type tag_typ =
  | Exp
  | Pat
  | Typ;

let view = (~inject: ModelAction.t => Vdom.Event.t, model: Model.t) => {
  /* a helper function working as an enhanced version of List.map() */
  let rec list_map_helper_func = (func_to_list, base, lst) => {
    switch (lst) {
    | [] => []
    | [head, ...tail] => [
        func_to_list(base, head),
        ...list_map_helper_func(func_to_list, base + 1, tail),
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
  let rec string_view = (s: string) =>
    if (StringUtil.is_empty(s)) {
      [];
    } else if (String.length(s) == 1) {
      Vdom.[Node.span([Attr.classes(["string"])], [Node.text(s)])];
    } else {
      switch (String.sub(s, 0, 1)) {
      | "\\" =>
        switch (String.sub(s, 1, 1)) {
        | "b"
        | "t"
        | "r"
        | "n"
        | "\\"
        | "\""
        | "\'" =>
          Vdom.[
            Node.span(
              [Attr.classes(["valid-seq"])],
              [Node.text(String.sub(s, 0, 2))],
            ),
            ...string_view(String.sub(s, 2, String.length(s) - 2)),
          ]
        | "o" =>
          if (String.length(s) >= 5) {
            let ch1 = s.[2];
            let ch2 = s.[3];
            let ch3 = s.[4];
            if ((ch1 >= '0' && ch1 <= '7')
                && (ch2 >= '0' && ch2 <= '7')
                && ch3 >= '0'
                && ch3 <= '7') {
              if (ch1 <= '3') {
                Vdom.[
                  Node.span(
                    [Attr.classes(["valid-seq"])],
                    [Node.text(String.sub(s, 0, 5))],
                  ),
                  ...string_view(String.sub(s, 5, String.length(s) - 5)),
                ];
              } else {
                Vdom.[
                  Node.span(
                    [Attr.classes(["invalid-seq"])],
                    [Node.text(String.sub(s, 0, 5))],
                  ),
                  ...string_view(String.sub(s, 5, String.length(s) - 5)),
                ];
              };
            } else {
              Vdom.[
                Node.span(
                  [Attr.classes(["invalid-seq"])],
                  [Node.text(String.sub(s, 0, 2))],
                ),
                ...string_view(String.sub(s, 2, String.length(s) - 2)),
              ];
            };
          } else {
            Vdom.[
              Node.span(
                [Attr.classes(["invalid-seq"])],
                [Node.text(String.sub(s, 0, 2))],
              ),
              ...string_view(String.sub(s, 2, String.length(s) - 2)),
            ];
          }
        | "x" =>
          if (String.length(s) >= 4) {
            let ch1 = Char.lowercase_ascii(s.[2]);
            let ch2 = Char.lowercase_ascii(s.[3]);
            if ((ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f')
                && (ch2 >= '0' && ch2 <= '9' || ch2 >= 'a' && ch2 <= 'f')) {
              Vdom.[
                Node.span(
                  [Attr.classes(["valid-seq"])],
                  [Node.text(String.sub(s, 0, 4))],
                ),
                ...string_view(String.sub(s, 4, String.length(s) - 4)),
              ];
            } else {
              Vdom.[
                Node.span(
                  [Attr.classes(["invalid-seq"])],
                  [Node.text(String.sub(s, 0, 2))],
                ),
                ...string_view(String.sub(s, 2, String.length(s) - 2)),
              ];
            };
          } else {
            Vdom.[
              Node.span(
                [Attr.classes(["invalid-seq"])],
                [Node.text(String.sub(s, 0, 2))],
              ),
              ...string_view(String.sub(s, 2, String.length(s) - 2)),
            ];
          }
        | _ =>
          let ch1 = s.[1];
          if (String.length(s) >= 4) {
            let ch2 = s.[2];
            let ch3 = s.[3];
            if ((ch1 >= '0' && ch1 <= '9')
                && (ch2 >= '0' && ch2 <= '9')
                && ch3 >= '0'
                && ch3 <= '9') {
              if (int_of_string(String.sub(s, 1, 3)) < 256) {
                Vdom.[
                  Node.span(
                    [Attr.classes(["valid-seq"])],
                    [Node.text(String.sub(s, 0, 4))],
                  ),
                  ...string_view(String.sub(s, 4, String.length(s) - 4)),
                ];
              } else {
                Vdom.[
                  Node.span(
                    [Attr.classes(["invalid-seq"])],
                    [Node.text(String.sub(s, 0, 4))],
                  ),
                  ...string_view(String.sub(s, 4, String.length(s) - 4)),
                ];
              };
            } else {
              Vdom.[
                Node.span(
                  [Attr.classes(["invalid-seq"])],
                  [Node.text(String.sub(s, 0, 2))],
                ),
                ...string_view(String.sub(s, 2, String.length(s) - 2)),
              ];
            };
          } else {
            Vdom.[
              Node.span(
                [Attr.classes(["invalid-seq"])],
                [Node.text(String.sub(s, 0, 2))],
              ),
              ...string_view(String.sub(s, 2, String.length(s) - 2)),
            ];
          };
        }
      | _ =>
        Vdom.[
          Node.span(
            [Attr.classes(["string"])],
            [Node.text(String.sub(s, 0, 1))],
          ),
          ...string_view(String.sub(s, 1, String.length(s) - 1)),
        ]
      };
    };
  let exp_view = (exp: UHExp.operand, show_indicate_word: bool) => {
    switch (exp) {
    | EmptyHole(meta_var) =>
      indicate_words_view("hole: " ++ string_of_int(meta_var))

    | InvalidText(_, inv_str) =>
      if (show_indicate_word) {
        Vdom.(
          Node.span(
            [],
            [indicate_words_view("invalid text: "), code_view(inv_str)],
          )
        );
      } else {
        code_view(inv_str);
      }

    | Var(_, _, var_str) =>
      if (show_indicate_word) {
        if (Var.is_case(var_str) || Var.is_let(var_str)) {
          Vdom.(
            Node.span(
              [],
              [indicate_words_view("keyword: "), code_view(var_str)],
            )
          );
        } else {
          Vdom.(
            Node.span(
              [],
              [indicate_words_view("var: "), code_view(var_str)],
            )
          );
        };
      } else {
        code_view(var_str);
      }
    | IntLit(_, num) =>
      if (show_indicate_word) {
        Vdom.(
          Node.span(
            [],
            [
              code_keywords_view("Int"),
              indicate_words_view(" literal "),
              code_view(num),
            ],
          )
        );
      } else {
        code_view(num);
      }
    | FloatLit(_, num) =>
      if (show_indicate_word) {
        Vdom.(
          Node.span(
            [],
            [
              code_keywords_view("Float"),
              indicate_words_view(" literal "),
              code_view(num),
            ],
          )
        );
      } else {
        code_view(num);
      }
    | StringLit(_, str) =>
      let str = StringUtil.escaped_enter(str);
      let (str, flag) =
        if (String.length(str) <= 6) {
          (str, false);
        } else {
          (String.sub(str, 0, 6), true);
        };

      if (show_indicate_word) {
        Vdom.(
          Node.span(
            [],
            [
              Node.span(
                [],
                [
                  code_keywords_view("String"),
                  indicate_words_view(" literal "),
                ],
              ),
              Node.span([Attr.classes(["string"])], [Node.text("\"")]),
              ...string_view(str)
                 @ (
                   if (flag == true) {
                     [
                       Node.span(
                         [Attr.classes(["ellipses"])],
                         [Node.text("...")],
                       ),
                     ];
                   } else {
                     [Node.text("")];
                   }
                 )
                 @ [
                   Node.span(
                     [Attr.classes(["string"])],
                     [Node.text("\"")],
                   ),
                 ],
            ],
          )
        );
      } else {
        Vdom.(
          Node.span(
            [],
            [
              Node.span([Attr.classes(["string"])], [Node.text("\"")]),
              ...string_view(str)
                 @ (
                   if (flag == true) {
                     [
                       Node.span(
                         [Attr.classes(["ellipses"])],
                         [Node.text("...")],
                       ),
                     ];
                   } else {
                     [Node.text("")];
                   }
                 )
                 @ [
                   Node.span(
                     [Attr.classes(["string"])],
                     [Node.text("\"")],
                   ),
                 ],
            ],
          )
        );
      };
    | BoolLit(_, bool_val) =>
      Vdom.(
        Node.span(
          [],
          [
            code_keywords_view("Bool"),
            indicate_words_view(" literal "),
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
    | Case(_, _, _) => code_keywords_view("case")
    | Parenthesized(_) => indicate_words_view("parentheses")
    | Subscript(_, _, _, _) => indicate_words_view("substring")
    | FreeLivelit(_) => indicate_words_view("free livelit")
    | ApLivelit(_) => indicate_words_view("ap livelit")
    };
  };

  let pat_view = (pat: UHPat.operand, show_indicate_word: bool) => {
    switch (pat) {
    | EmptyHole(meta_var) =>
      indicate_words_view("hole: " ++ string_of_int(meta_var))
    | Wild(_) => indicate_words_view("wild card")
    | InvalidText(_, inv_str) =>
      if (show_indicate_word) {
        Vdom.(
          Node.span(
            [],
            [indicate_words_view("invalid text: "), code_view(inv_str)],
          )
        );
      } else {
        code_view(inv_str);
      }
    | Var(_, _, var_str) =>
      if (show_indicate_word) {
        Vdom.(
          Node.span([], [indicate_words_view("var: "), code_view(var_str)])
        );
      } else {
        code_view(var_str);
      }
    | IntLit(_, num) =>
      if (show_indicate_word) {
        Vdom.(
          Node.span(
            [],
            [
              code_keywords_view("Int"),
              indicate_words_view(" literal "),
              code_view(num),
            ],
          )
        );
      } else {
        code_view(num);
      }
    | FloatLit(_, num) =>
      if (show_indicate_word) {
        Vdom.(
          Node.span(
            [],
            [
              code_keywords_view("Float"),
              indicate_words_view(" literal "),
              code_view(num),
            ],
          )
        );
      } else {
        code_view(num);
      }
    | StringLit(_, str) =>
      let str = StringUtil.escaped_enter(str);
      let (str, flag) =
        if (String.length(str) <= 6) {
          (str, false);
        } else {
          (String.sub(str, 0, 6), true);
        };
      if (show_indicate_word) {
        Vdom.(
          Node.span(
            [],
            [
              Node.span(
                [],
                [
                  code_keywords_view("String"),
                  indicate_words_view(" literal "),
                ],
              ),
              Node.span([Attr.classes(["string"])], [Node.text("\"")]),
              ...string_view(str)
                 @ (
                   if (flag == true) {
                     [
                       Node.span(
                         [Attr.classes(["ellipses"])],
                         [Node.text("...")],
                       ),
                     ];
                   } else {
                     [Node.text("")];
                   }
                 )
                 @ [
                   Node.span(
                     [Attr.classes(["string"])],
                     [Node.text("\"")],
                   ),
                 ],
            ],
          )
        );
      } else {
        Vdom.(
          Node.span(
            [Attr.classes(["string"])],
            [
              Node.span([Attr.classes(["string"])], [Node.text("\"")]),
              ...string_view(str)
                 @ (
                   if (flag == true) {
                     [
                       Node.span(
                         [Attr.classes(["ellipses"])],
                         [Node.text("...")],
                       ),
                     ];
                   } else {
                     [Node.text("")];
                   }
                 )
                 @ [
                   Node.span(
                     [Attr.classes(["string"])],
                     [Node.text("\"")],
                   ),
                 ],
            ],
          )
        );
      };
    | BoolLit(_, bool_val) =>
      Vdom.(
        Node.span(
          [],
          [
            code_keywords_view("Bool"),
            indicate_words_view(" literal "),
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
    | String =>
      Vdom.(
        Node.span(
          [],
          [indicate_words_view("type: "), code_keywords_view("String")],
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

  let cursor_term_view =
      (cursor_term: CursorInfo.cursor_term, show_indicate_word: bool) => {
    switch (cursor_term) {
    | Exp(_, exp) => exp_view(exp, show_indicate_word)
    | Pat(_, pat) => pat_view(pat, show_indicate_word)
    | Typ(_, typ) => typ_view(typ)
    | ExpOp(_, op) => code_view(Operators_Exp.to_string(op))
    | PatOp(_, op) => code_view(Operators_Pat.to_string(op))
    | TypOp(_, op) => code_view(Operators_Typ.to_string(op))
    | Line(_, line_content) =>
      switch (line_content) {
      | EmptyLine => indicate_words_view("empty line")
      | CommentLine(comment) =>
        if (comment == "") {
          indicate_words_view("empty comment");
        } else {
          Vdom.(
            Node.span(
              [],
              [indicate_words_view("comment "), code_view(comment)],
            )
          );
        }
      | LetLine(_, _, _) =>
        Vdom.(
          Node.span(
            [],
            [code_keywords_view("let"), indicate_words_view(" binding")],
          )
        )
      | AbbrevLine(_) =>
        Vdom.(
          Node.span(
            [],
            [
              indicate_words_view("livelit "),
              code_keywords_view("abbrev"),
              indicate_words_view("iation"),
            ],
          )
        )
      | LivelitDefLine(_) =>
        Vdom.(
          Node.span(
            [],
            [
              code_keywords_view("livelit"),
              indicate_words_view(" definition"),
            ],
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
    | SAbbrev =>
      Vdom.(
        Node.span(
          [],
          [
            indicate_words_view("livelit "),
            code_keywords_view("abbrev"),
            indicate_words_view("iation"),
          ],
        )
      )
    | SLivelitDef =>
      Vdom.(
        Node.span(
          [],
          [
            code_keywords_view("livelit"),
            indicate_words_view(" definition"),
          ],
        )
      )
    | SCase =>
      Vdom.(
        Node.span(
          [],
          [code_keywords_view("case"), indicate_words_view(" expression")],
        )
      )
    | SQuote
    | SList
    | SLeftBracket
    | SListNil
    | SLine
    | SCommentLine
    | SAsc
    | SParenthesized =>
      indicate_words_view(Action_common.shape_to_string(shape))
    | SChar(_) => code_view(Action_common.shape_to_string(shape))
    | SOp(op) =>
      switch (op) {
      | SSpace => indicate_words_view("space")
      | _ => code_view(Action_common.shape_to_string(shape))
      }
    };
  };
  let history_entry_txt_view = (undo_history_entry: undo_history_entry) => {
    switch (undo_history_entry.action_group) {
    | DeleteEdit(edit_detail) =>
      switch (edit_detail) {
      | Term(cursor_term, start_from_insertion) =>
        let indicate_words =
          if (start_from_insertion) {"insert and delete "} else {"delete "};
        Vdom.(
          Node.span(
            [],
            [
              indicate_words_view(indicate_words),
              cursor_term_view(cursor_term, true),
            ],
          )
        );
      | Space => indicate_words_view("delete space")
      | EmptyLine => indicate_words_view("delete empty line")
      | TypeAnn => indicate_words_view("delete type annotation")
      }
    | ConstructEdit(edit_detail) =>
      switch (edit_detail) {
      | SLet =>
        Vdom.(
          Node.span(
            [],
            [
              indicate_words_view("construct "),
              code_keywords_view("let"),
              indicate_words_view(" binding"),
            ],
          )
        )
      | SAbbrev =>
        Vdom.(
          Node.span(
            [],
            [
              indicate_words_view("construct "),
              code_keywords_view("abbrev"),
              indicate_words_view(" abbreviation"),
            ],
          )
        )
      | SCase =>
        Vdom.(
          Node.span(
            [],
            [indicate_words_view("construct "), code_keywords_view("case")],
          )
        )
      | SLam => indicate_words_view("construct function")
      | _ =>
        Vdom.(
          Node.span(
            [],
            [
              indicate_words_view("insert "),
              action_shape_view(edit_detail),
            ],
          )
        )
      }
    | VarGroup(var_edit) =>
      switch (var_edit) {
      | Edit(edit_term) =>
        Vdom.(
          Node.span(
            [],
            [
              indicate_words_view("edit "),
              cursor_term_view(edit_term.start_from, true),
              indicate_words_view(" to "),
              cursor_term_view(edit_term.end_with, false),
            ],
          )
        )
      | Insert(term) =>
        Vdom.(
          Node.span(
            [],
            [indicate_words_view("insert "), cursor_term_view(term, true)],
          )
        )
      }
    | CaseRule =>
      Vdom.(
        Node.span(
          [],
          [
            indicate_words_view("insert "),
            code_keywords_view("case"),
            indicate_words_view(" rule"),
          ],
        )
      )
    | Subscript =>
      Vdom.(Node.span([], [indicate_words_view("insert substring")]))
    | SwapEdit(swap_group) =>
      switch (swap_group) {
      | Up => indicate_words_view("swap line up")
      | Down => indicate_words_view("swap line down")
      | Left => indicate_words_view("swap operand left")
      | Right => indicate_words_view("swap operand right")
      }
    | Init => indicate_words_view("initial state")
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
    switch (undo_history_entry.action_group) {
    | DeleteEdit(edit_detail) =>
      switch (edit_detail) {
      | Term(cursor_term, _) => Some(get_cursor_term_tag_typ(cursor_term))
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
      | SAbbrev
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
    | VarGroup(_) =>
      Some(
        get_cursor_term_tag_typ(
          undo_history_entry.cursor_term_info.cursor_term_after,
        ),
      )
    | CaseRule => Some(Exp)
    | Subscript =>
      Some(
        get_cursor_term_tag_typ(
          undo_history_entry.cursor_term_info.cursor_term_after,
        ),
      )
    | SwapEdit(swap_group) =>
      switch (swap_group) {
      | Up
      | Down => Some(Exp)
      | Left
      | Right =>
        Some(
          get_cursor_term_tag_typ(
            undo_history_entry.cursor_term_info.cursor_term_after,
          ),
        )
      }
    | Init => None
    };
  };

  let history_entry_tab_icon =
      (group_id: int, has_hidden_part: bool, is_expanded: bool) =>
    if (has_hidden_part) {
      /* expand icon*/
      if (is_expanded) {
        Vdom.(
          Node.div(
            [
              Attr.on_click(_ =>
                Vdom.Event.Many([
                  Event.Prevent_default,
                  Event.Stop_propagation,
                  inject(ModelAction.ToggleHistoryGroup(group_id)),
                  inject(FocusCell),
                ])
              ),
              Attr.create("title", "Collapse Group"),
            ],
            [Icons.down_arrow(["entry-tab-icon", "history-tab-icon"])],
          )
        );
      } else {
        Vdom.(
          Node.div(
            [
              Attr.on_click(_ =>
                Vdom.Event.Many([
                  Event.Prevent_default,
                  Event.Stop_propagation,
                  inject(ModelAction.ToggleHistoryGroup(group_id)),
                  inject(FocusCell),
                ])
              ),
              Attr.create("title", "Expand Group"),
            ],
            [Icons.left_arrow(["entry-tab-icon", "history-tab-icon"])],
          )
        );
      };
    } else {
      /* no expand icon if there is no hidden part */
      Vdom.(Node.div([], []));
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
    /* making the entry show preview effect when scrolling
       is realized by getting topmost element under the mouse.
       Due to css padding setting, this element can be at any level,
       so we have to add attribute "group_id" and "elt_id" for all div levels */
    Vdom.(
      Node.div(
        [Attr.classes(status_class)],
        [
          Node.div(
            if (is_current_entry) {
              [
                Attr.id("cur-selected-entry"),
                Attr.create("group_id", string_of_int(group_id)),
                Attr.create("elt_id", string_of_int(elt_id)),
                Attr.classes(["history-entry"]),
                Attr.on_click(_ =>
                  Vdom.Event.Many([
                    inject(
                      ModelAction.ShiftHistory({
                        group_id,
                        elt_id,
                        call_by_mouseenter: false,
                      }),
                    ),
                    inject(FocusCell),
                  ])
                ),
                Attr.on_mouseenter(_ =>
                  if (undo_history.preview_on_hover) {
                    Vdom.Event.Many([
                      inject(
                        ModelAction.ShiftHistory({
                          group_id,
                          elt_id,
                          call_by_mouseenter: true,
                        }),
                      ),
                      inject(FocusCell),
                    ]);
                  } else {
                    Vdom.Event.Ignore;
                  }
                ),
                Attr.on_mouseleave(_ =>
                  if (undo_history.preview_on_hover) {
                    Vdom.Event.Many([
                      inject(
                        ModelAction.ShiftHistory({
                          group_id: undo_history.hover_recover_group_id,
                          elt_id: undo_history.hover_recover_elt_id,
                          call_by_mouseenter: false,
                        }),
                      ),
                      inject(FocusCell),
                    ]);
                  } else {
                    Vdom.Event.Ignore;
                  }
                ),
              ];
            } else {
              [
                Attr.create("group_id", string_of_int(group_id)),
                Attr.create("elt_id", string_of_int(elt_id)),
                Attr.classes(["history-entry"]),
                Attr.on_click(_ =>
                  Vdom.Event.Many([
                    inject(
                      ModelAction.ShiftHistory({
                        group_id,
                        elt_id,
                        call_by_mouseenter: false,
                      }),
                    ),
                    inject(FocusCell),
                  ])
                ),
                Attr.on_mouseenter(_ =>
                  if (undo_history.preview_on_hover) {
                    Vdom.Event.Many([
                      inject(
                        ModelAction.ShiftHistory({
                          group_id,
                          elt_id,
                          call_by_mouseenter: true,
                        }),
                      ),
                      inject(FocusCell),
                    ]);
                  } else {
                    Vdom.Event.Ignore;
                  }
                ),
                Attr.on_mouseleave(_ =>
                  if (undo_history.preview_on_hover) {
                    Vdom.Event.Many([
                      inject(
                        ModelAction.ShiftHistory({
                          group_id: undo_history.hover_recover_group_id,
                          elt_id: undo_history.hover_recover_elt_id,
                          call_by_mouseenter: false,
                        }),
                      ),
                      inject(FocusCell),
                    ]);
                  } else {
                    Vdom.Event.Ignore;
                  }
                ),
              ];
            },
            [
              Node.div(
                [
                  Attr.create("group_id", string_of_int(group_id)),
                  Attr.create("elt_id", string_of_int(elt_id)),
                ],
                [
                  history_typ_tag_view(undo_history_entry),
                  Node.div(
                    [
                      Attr.classes(["history-entry-txt"]),
                      Attr.create("group_id", string_of_int(group_id)),
                      Attr.create("elt_id", string_of_int(elt_id)),
                    ],
                    [history_entry_txt_view(undo_history_entry)],
                  ),
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
      )
    );
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
    /* making the entry show preview effect when scrolling
       is realized by getting topmost element under the mouse.
       Due to css padding setting, this element can be at any level,
       so we have to add attribute "group_id" and "elt_id" for all div levels */
    Vdom.(
      Node.div(
        [Attr.classes(status_class)],
        [
          Node.div(
            if (is_current_entry) {
              [
                Attr.create("group_id", string_of_int(group_id)),
                Attr.create("elt_id", string_of_int(elt_id)),
                Attr.classes(["history-entry"]),
                Attr.id("cur-selected-entry"),
                Attr.on_click(_ =>
                  Vdom.Event.Many([
                    inject(
                      ModelAction.ShiftHistory({
                        group_id,
                        elt_id,
                        call_by_mouseenter: false,
                      }),
                    ),
                    inject(FocusCell),
                  ])
                ),
                Attr.on_mouseenter(_ =>
                  if (undo_history.preview_on_hover) {
                    Vdom.Event.Many([
                      inject(
                        ModelAction.ShiftHistory({
                          group_id,
                          elt_id,
                          call_by_mouseenter: true,
                        }),
                      ),
                      inject(FocusCell),
                    ]);
                  } else {
                    Vdom.Event.Ignore;
                  }
                ),
                Attr.on_mouseleave(_ =>
                  if (undo_history.preview_on_hover) {
                    Vdom.Event.Many([
                      inject(
                        ModelAction.ShiftHistory({
                          group_id: undo_history.hover_recover_group_id,
                          elt_id: undo_history.hover_recover_elt_id,
                          call_by_mouseenter: false,
                        }),
                      ),
                      inject(FocusCell),
                    ]);
                  } else {
                    Vdom.Event.Ignore;
                  }
                ),
              ];
            } else {
              [
                Attr.create("group_id", string_of_int(group_id)),
                Attr.create("elt_id", string_of_int(elt_id)),
                Attr.classes(["history-entry"]),
                Attr.on_click(_ =>
                  Vdom.Event.Many([
                    inject(
                      ModelAction.ShiftHistory({
                        group_id,
                        elt_id,
                        call_by_mouseenter: false,
                      }),
                    ),
                    inject(FocusCell),
                  ])
                ),
                Attr.on_mouseenter(_ =>
                  if (undo_history.preview_on_hover) {
                    Vdom.Event.Many([
                      inject(
                        ModelAction.ShiftHistory({
                          group_id,
                          elt_id,
                          call_by_mouseenter: true,
                        }),
                      ),
                      inject(FocusCell),
                    ]);
                  } else {
                    Vdom.Event.Ignore;
                  }
                ),
                Attr.on_mouseleave(_ =>
                  if (undo_history.preview_on_hover) {
                    Vdom.Event.Many([
                      inject(
                        ModelAction.ShiftHistory({
                          group_id: undo_history.hover_recover_group_id,
                          elt_id: undo_history.hover_recover_elt_id,
                          call_by_mouseenter: false,
                        }),
                      ),
                      inject(FocusCell),
                    ]);
                  } else {
                    Vdom.Event.Ignore;
                  }
                ),
              ];
            },
            [
              Node.div(
                [
                  Attr.classes(["hidden-history-entry"]),
                  Attr.create("group_id", string_of_int(group_id)),
                  Attr.create("elt_id", string_of_int(elt_id)),
                ],
                [
                  history_typ_tag_view(undo_history_entry),
                  Node.div(
                    [
                      Attr.classes(["history-entry-txt"]),
                      Attr.create("group_id", string_of_int(group_id)),
                      Attr.create("elt_id", string_of_int(elt_id)),
                    ],
                    [history_entry_txt_view(undo_history_entry)],
                  ),
                  timestamp_view(undo_history_entry),
                ],
              ),
            ],
          ),
        ],
      )
    );
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

  let history_view = (model: Model.t) =>
    Vdom.(
      Node.div(
        [Attr.classes(["the-history"])],
        list_map_helper_func(
          group_view(~undo_history=model.undo_history),
          0,
          ZList.join(model.undo_history.groups),
        ),
      )
    );

  let undo_button = (disabled, is_mac) => {
    let title = if (is_mac) {"Undo (Cmd+Z)"} else {"Undo (Ctrl+Z)"};
    Vdom.(
      Node.div(
        disabled
          ? [
            Attr.classes(["history-button"]),
            Attr.disabled,
            Attr.on_click(_ =>
              Vdom.Event.Many([inject(ModelAction.Undo), inject(FocusCell)])
            ),
            Attr.create("title", title),
          ]
          : [
            Attr.classes(["history-button"]),
            Attr.on_click(_ =>
              Vdom.Event.Many([inject(ModelAction.Undo), inject(FocusCell)])
            ),
            Attr.create("title", title),
          ],
        [Icons.undo(["redo-undo-icon"])],
      )
    );
  };

  let redo_button = (disabled, is_mac) => {
    let title =
      if (is_mac) {"Redo (Cmd+Shift+Z)"} else {"Redo (Ctrl+Shift+Z)"};
    Vdom.(
      Node.div(
        disabled
          ? [
            Attr.classes(["history-button"]),
            Attr.disabled,
            Attr.on_click(_ =>
              Vdom.Event.Many([inject(ModelAction.Redo), inject(FocusCell)])
            ),
            Attr.create("title", title),
          ]
          : [
            Attr.classes(["history-button"]),
            Attr.on_click(_ =>
              Vdom.Event.Many([inject(ModelAction.Redo), inject(FocusCell)])
            ),
            Attr.create("title", title),
          ],
        [Icons.undo(["redo-undo-icon", "horizontal-flip"])],
      )
    );
  };

  let expand_button = (all_hidden_history_expand: bool) =>
    if (all_hidden_history_expand) {
      Vdom.(
        Node.div(
          [
            Attr.classes(["history-button", "all-history-tab-icon-wrapper"]),
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.ToggleHiddenHistoryAll),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Collapse Groups"),
          ],
          [Icons.down_arrow(["all-history-tab-icon", "history-tab-icon"])],
        )
      );
    } else {
      Vdom.(
        Node.div(
          [
            Attr.classes(["history-button", "all-history-tab-icon-wrapper"]),
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.ToggleHiddenHistoryAll),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Expand Groups"),
          ],
          [Icons.left_arrow(["all-history-tab-icon", "history-tab-icon"])],
        )
      );
    };

  let preview_on_hover_checkbox = (preview_on_hover: bool) => {
    SettingsPanel.labeled_checkbox(
      ~id="preview_on_hover",
      ~label="Preview On Hover",
      ~on_change=() => inject(TogglePreviewOnHover),
      preview_on_hover,
    );
  };

  let button_bar_view = (undo_history: UndoHistory.t, is_mac: bool) =>
    Vdom.(
      Node.div(
        [Attr.classes(["history-button-bar"])],
        [
          preview_on_hover_checkbox(undo_history.preview_on_hover),
          expand_button(undo_history.all_hidden_history_expand),
          redo_button(UndoHistory.disable_redo(undo_history), is_mac),
          undo_button(UndoHistory.disable_undo(undo_history), is_mac),
        ],
      )
    );

  /* return option((group_id, elt_id)) */
  let get_elt_id_under_mouse = (model: Model.t): option((int, int)) => {
    let elt: Js.t(Dom_html.divElement) =
      JSUtil.element_from_point(model.mouse_position^);
    switch (
      JSUtil.get_attr("group_id", elt),
      JSUtil.get_attr("elt_id", elt),
    ) {
    | (Some(group_id), Some(elt_id)) =>
      Some((int_of_string(group_id), int_of_string(elt_id)))
    | _ => None
    };
  };

  Vdom.(
    Node.div(
      [
        Attr.id("history-panel"),
        Attr.classes(["panel", "context-inspector-panel"]),
      ],
      [
        Panel.view_of_main_title_bar("history"),
        button_bar_view(model.undo_history, model.is_mac),
        Node.div(
          if (model.undo_history.preview_on_hover) {
            [
              Attr.classes(["panel-body", "context-inspector-body"]),
              Attr.id("history-body"),
              Attr.on_mousemove(evt => {
                /* update mouse position */
                model.mouse_position := JSUtil.get_mouse_position(evt);
                Vdom.Event.Many([inject(FocusCell)]);
              }),
              Attr.on("scroll", _ => {
                /* on_mouseenter/on_mouseleave will not be fired when scrolling,
                   so we get the history entry under the mouse
                   and shift to this entry manually  */
                switch (get_elt_id_under_mouse(model)) {
                | Some((group_id, elt_id)) =>
                  Vdom.Event.Many([
                    inject(
                      ModelAction.ShiftHistory({
                        group_id,
                        elt_id,
                        call_by_mouseenter: true,
                      }),
                    ),
                    inject(FocusCell),
                  ])
                | None => Vdom.Event.Ignore
                }
              }),
            ];
          } else {
            [
              Attr.classes(["panel-body", "context-inspector-body"]),
              Attr.id("history-body"),
            ];
          },
          [history_view(model)],
        ),
      ],
    )
  );
};

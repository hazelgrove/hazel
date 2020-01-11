module Vdom = Virtual_dom.Vdom;
module KeyCombo = JSUtil.KeyCombo;
module ZList = GeneralUtil.ZList;
exception InvalidInstance;

let view = (~inject: Update.Action.t => Vdom.Event.t, model: Model.t) => {
  let shape_to_display_string = (shape: Action.shape): string => {
    switch (shape) {
    | SParenthesized => "parentheise"
    | SNum => "type Num"
    | SBool => "type Bool"
    | SList => "type List"
    | SAsc => "add ':'"
    | SVar(varstr, _) => "edit var: " ++ varstr
    | SLam => "add lambada"
    | SNumLit(value, _) => "edit number: " ++ string_of_int(value)
    | SListNil => "add list"
    | SInj(direction) =>
      switch (direction) {
      | L => "inject left"
      | R => "inject right"
      }
    | SLet => "bulid 'let'"
    | SLine => "add new line[s]"
    | SCase => "add case"
    | SOp(op) => "add operator " ++ Action.op_shape_to_string(op)
    | SApPalette(_) => "appalette?"
    /* pattern-only shapes */
    | SWild => "wild?"
    };
  };

  let action_to_display_string = (action: Action.t) => {
    switch (action) {
    | UpdateApPalette(_) => "updatePlate?"
    | Delete => "delete"
    | Backspace => "backspace"
    | Construct(shape) => shape_to_display_string(shape)
    | MoveTo(_)
    | MoveToBefore(_)
    | MoveLeft
    | MoveRight
    | MoveToNextHole
    | MoveToPrevHole
    | ShiftLeft
    | ShiftRight
    | ShiftUp
    | ShiftDown => "will not show in undo_history"
    };
  };

  let history_hidden_entry_view = (group_id: int, undo_history_entry) => {
    let (_, action, elt_id) = undo_history_entry;
    switch (action) {
    | None => Vdom.(Node.div([], [])) /* init edit-state should not be displayed */
    | Some(detail_ac) =>
      Vdom.(
        Node.div(
          [
            Attr.classes(["the-hidden-history-entry"]),
            Attr.on_click(_ =>
              inject(Update.Action.ShiftHistory(group_id, elt_id))
            ),
          ],
          [Node.text(action_to_display_string(detail_ac))],
        )
      )
    };
  };

  let history_title_entry_view =
      (
        ~is_expanded: bool,
        ~has_hidden_part: bool,
        group_id: int,
        undo_history_entry,
      ) => {
    let (_, action, elt_id) = undo_history_entry;
    let icon_classes =
      if (is_expanded) {
        ["down-triangle", "history-tab-icon"];
      } else {
        ["left-triangle", "history-tab-icon"];
      };
    let history_tab_icon = (group_id: int) =>
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
        Vdom.(Node.div([], []));
      };
    switch (action) {
    | None => Vdom.(Node.div([], []))
    | Some(detail_ac) =>
      Vdom.(
        Node.div(
          [Attr.classes(["the-history-title"])],
          [
            Node.div(
              [Attr.classes(["the-history-title-entry"])],
              [
                Node.span(
                  [
                    Attr.classes(["the-history-title-content"]),
                    Attr.on_click(_ =>
                      inject(Update.Action.ShiftHistory(group_id, elt_id))
                    ),
                  ],
                  [Node.text(action_to_display_string(detail_ac))],
                ),
                history_tab_icon(group_id),
              ],
            ),
          ],
        )
      )
    };
  };

  let group_view =
      (
        ~is_cur_group: bool,
        group_entry: (
          ZList.t(
            UndoHistory.undo_history_entry,
            UndoHistory.undo_history_entry,
          ),
          int,
          bool,
        ),
      ) => {
    let (group_lst_old_first, group_id, is_expanded) = group_entry;
    /* reverse the undo_history, so the first entry shown in panel is the latest history entry */
    let group_lst_new_first = (
      List.rev(ZList.prj_suffix(group_lst_old_first)),
      ZList.prj_z(group_lst_old_first),
      List.rev(ZList.prj_prefix(group_lst_old_first)),
    );
    /* if the group containning selected history entry, it should be splited to different css styles */
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
    switch (group_lst_new_first) {
    | ([], cur_entry, prev_lst) =>
      switch (cur_entry) {
      | (_, None, _) => Vdom.(Node.div([], [])) /* init edit-state should not be displayed */
      | (_, Some(_), _) =>
        let has_hidden_part =
          switch (prev_lst) {
          | [] => false
          | _ => true
          };
        if (is_expanded) {
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
                        ~is_expanded,
                        ~has_hidden_part,
                        group_id,
                        cur_entry,
                      ),
                    ],
                  )
                ),
                Vdom.(
                  Node.div(
                    [
                      Attr.classes(
                        ["hidden-history-entry"] @ prev_his_classes,
                      ),
                    ],
                    List.map(history_hidden_entry_view(group_id), prev_lst),
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
                        ["always-display-history-entry"] @ cur_his_classes,
                      ),
                    ],
                    [
                      history_title_entry_view(
                        ~is_expanded,
                        ~has_hidden_part,
                        group_id,
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

    | ([title_entry, ...suc_lst], cur_entry, prev_lst) =>
      if (is_expanded) {
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
                      ~is_expanded,
                      ~has_hidden_part=true,
                      group_id,
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
                  List.map(history_hidden_entry_view(group_id), suc_lst),
                )
              ),
              /* the selected(current) history entry */
              Vdom.(
                Node.div(
                  [
                    Attr.classes(["hidden-history-entry"] @ cur_his_classes),
                  ],
                  [history_hidden_entry_view(group_id, cur_entry)],
                )
              ),
              /* the previous history entry */
              Vdom.(
                Node.div(
                  [
                    Attr.classes(["hidden-history-entry"] @ prev_his_classes),
                  ],
                  List.map(history_hidden_entry_view(group_id), prev_lst),
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
                      ~is_expanded,
                      ~has_hidden_part=true,
                      group_id,
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

  let prev_history_view = history => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-prev-history"])],
        List.map(group_view(~is_cur_group=false), history),
      )
    );
  };
  let suc_history_view = history => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-suc-history"])],
        List.map(group_view(~is_cur_group=false), history),
      )
    );
  };
  let cur_history_view =
      (
        history: (
          ZList.t(
            UndoHistory.undo_history_entry,
            UndoHistory.undo_history_entry,
          ),
          int,
          bool,
        ),
      ) => {
    Vdom.(Node.div([], [group_view(~is_cur_group=true, history)]));
  };
  let history_view = (model: Model.t) => {
    let (prev_his, cur_group, suc_his) = model.undo_history;
    let display_content =
      Vdom.(
        Node.div(
          [Attr.classes(["the-history"])],
          [
            suc_history_view(List.rev(suc_his)),
            cur_history_view(cur_group),
            prev_history_view(List.rev(prev_his)),
          ],
        )
      );
    let (cur_group_lst, _, _) = cur_group;
    let (_, cur_entry, _) = cur_group_lst;
    switch (cur_entry) {
    | (_, None, _) =>
      /*if init state is only history entry */
      if (List.length(suc_his) <= 1) {
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
    | (_, Some(_), _) => display_content
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

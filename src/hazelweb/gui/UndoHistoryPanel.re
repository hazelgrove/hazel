module Vdom = Virtual_dom.Vdom;
module ZList = GeneralUtil.ZList;
type undo_history_group = UndoHistory.undo_history_group;
type undo_history_entry = UndoHistory.undo_history_entry;

let view = (~inject: Update.Action.t => Vdom.Event.t, model: Model.t) => {
  let rec incr_map_helper = (func_to_list, func_to_base, base, lst) => {
    switch (lst) {
    | [] => []
    | [head, ...tail] => [
        func_to_list(base, head),
        ...incr_map_helper(
             func_to_list,
             func_to_base,
             func_to_base(base),
             tail,
           ),
      ]
    };
  };
  let history_hidden_entry_view =
      (group_id: int, elt_id: int, undo_history_entry: undo_history_entry) => {
    switch (undo_history_entry.previous_action) {
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
          [Node.text(Action.action_to_display_string(detail_ac))],
        )
      )
    };
  };

  let history_title_entry_view =
      (
        ~is_expanded: bool,
        ~has_hidden_part: bool,
        group_id: int,
        elt_id: int,
        undo_history_entry: undo_history_entry,
      ) => {
    let history_tab_icon = (group_id: int) => {
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
        Vdom.(Node.div([], []));
      };
    };
    switch (undo_history_entry.previous_action) {
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
                    Attr.classes(["the-history-title-txt"]),
                    Attr.on_click(_ =>
                      inject(Update.Action.ShiftHistory(group_id, elt_id))
                    ),
                  ],
                  [Node.text(Action.action_to_display_string(detail_ac))],
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
      (~is_cur_group: bool, group_id: int, group: undo_history_group) => {
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
    switch (group.group_entries) {
    | ([], cur_state, prev_states) =>
      switch (cur_state.previous_action) {
      | None => Vdom.(Node.div([], [])) /* init edit-state should not be displayed */
      | Some(_) =>
        let has_hidden_part =
          switch (prev_states) {
          | [] => false
          | _ => true
          };
        if (group.is_expanded) {
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
                        cur_state,
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
                    incr_map_helper(
                      history_hidden_entry_view(group_id),
                      base => base + 1,
                      1,
                      prev_states,
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
                        ["always-display-history-entry"] @ cur_his_classes,
                      ),
                    ],
                    [
                      history_title_entry_view(
                        ~is_expanded=group.is_expanded,
                        ~has_hidden_part,
                        group_id,
                        0, /*elt_id*/
                        cur_state,
                      ),
                    ],
                  )
                ),
              ],
            )
          );
        };
      }

    | ([title_entry, ...suc_states], cur_state, prev_states) =>
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
                  incr_map_helper(
                    history_hidden_entry_view(group_id),
                    base => base + 1,
                    1,
                    suc_states,
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
                      List.length(suc_states) + 1,
                      cur_state,
                    ),
                  ] /*elt_id= List.length(suc_states)+1*/
                )
              ),
              /* the previous history entry */
              Vdom.(
                Node.div(
                  [
                    Attr.classes(["hidden-history-entry"] @ prev_his_classes),
                  ],
                  incr_map_helper(
                    history_hidden_entry_view(group_id),
                    base => base + 1,
                    List.length(suc_states) + 2,
                    prev_states,
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

  let prev_history_view = (history, group_id_base: int) => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-prev-history"])],
        incr_map_helper(
          group_view(~is_cur_group=false),
          base => base + 1,
          group_id_base,
          history,
        ),
      )
    );
  };
  let suc_history_view = (history, group_id_base: int) => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-suc-history"])],
        incr_map_helper(
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
      /*if init state is only history entry */
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

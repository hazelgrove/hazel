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
    | SVar(varstr, _) => "edit var: " ++ varstr // convert int to char? ++ Char.chr(intvar)
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

  let history_entry_view = (group_id: int, undo_history_entry) => {
    let (_, action, id) = undo_history_entry;
    switch (action) {
    | None => Vdom.(Node.div([], []))
    | Some(detail_ac) =>
      Vdom.(
        Node.div(
          [
            Attr.classes(["the-history-entry"]),
            Attr.on_click(_ =>
              inject(Update.Action.ShiftHistory(group_id, id))
            ),
          ],
          [Node.text(action_to_display_string(detail_ac))],
        )
      )
    };
  };

  let history_title_entry_view =
      (
        is_expanded: bool,
        has_hidden_part: bool,
        group_id: int,
        undo_history_entry,
      ) => {
    let (_, action, id) = undo_history_entry;
    let icon_classes =
      if (is_expanded) {
        ["history-tab-icon-open"];
      } else {
        ["history-tab-icon-close"];
      };
    let hidden_part = (group_id: int) =>
      if (has_hidden_part) {
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
              [
                Attr.classes(["the-history-title-entry"]),
                Attr.on_click(_ =>
                  inject(Update.Action.ShiftHistory(group_id, id))
                ),
              ],
              [
                Node.text(action_to_display_string(detail_ac)),
                hidden_part(group_id),
              ],
            ),
          ],
        )
      )
    };
  };
  /* (ZList.t(undo_history_entry, undo_history_entry),int,bool) */
  let group_view =
      (
        is_cur_group: bool,
        group_entry: (
          ZList.t(
            UndoHistory.undo_history_entry,
            UndoHistory.undo_history_entry,
          ),
          int,
          bool,
        ),
      ) => {
    let (group_lst_old_first, group_id, isexpanded) = group_entry;
    let group_lst_new_first = (
      List.rev(ZList.prj_suffix(group_lst_old_first)),
      ZList.prj_z(group_lst_old_first),
      List.rev(ZList.prj_prefix(group_lst_old_first)),
    );
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
        ["the-now-history"];
      } else {
        [];
      };
    switch (group_lst_new_first) {
    | ([], entry_now, prev_lst) =>
      switch (entry_now) {
      | (_, None, _) => Vdom.(Node.div([], []))
      | (_, Some(_), _) =>
        let has_hidden_part =
          switch (prev_lst) {
          | [] => false
          | _ => true
          };
        if (isexpanded) {
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
                        isexpanded,
                        has_hidden_part,
                        group_id,
                        entry_now,
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
                    List.map(history_entry_view(group_id), prev_lst),
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
                        isexpanded,
                        has_hidden_part,
                        group_id,
                        entry_now,
                      ),
                    ],
                  )
                ),
              ],
            )
          );
        };
      }

    | ([title_entry, ...suc_lst], entry_now, prev_lst) =>
      if (isexpanded) {
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
                      isexpanded,
                      true,
                      group_id,
                      title_entry,
                    ),
                  ],
                )
              ),
              Vdom.(
                Node.div(
                  [
                    Attr.classes(["hidden-history-entry"] @ suc_his_classes),
                  ],
                  List.map(history_entry_view(group_id), suc_lst),
                )
              ),
              Vdom.(
                Node.div(
                  [
                    Attr.classes(["hidden-history-entry"] @ cur_his_classes),
                  ],
                  [history_entry_view(group_id, entry_now)],
                )
              ),
              Vdom.(
                Node.div(
                  [
                    Attr.classes(["hidden-history-entry"] @ prev_his_classes),
                  ],
                  List.map(history_entry_view(group_id), prev_lst),
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
                      isexpanded,
                      true,
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
        List.map(group_view(false), history),
      )
    );
  };
  let suc_history_view = history => {
    Vdom.(
      Node.div(
        [Attr.classes(["the-suc-history"])],
        List.map(group_view(false), history),
      )
    );
  };
  let now_history_view =
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
    Vdom.(Node.div([], [group_view(true, history)]));
  };
  let history_view = (model: Model.t) => {
    let (prev_his, cur_group, suc_his) = model.undo_history;
    let display_content =
      Vdom.(
        Node.div(
          [Attr.classes(["the-history"])],
          [
            suc_history_view(List.rev(suc_his)),
            now_history_view(cur_group),
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
  Vdom.(
    Node.div(
      [Attr.classes(["panel", "context-inspector-panel"])],
      [
        Panel.view_of_main_title_bar("history"),
        Node.div(
          [Attr.classes(["panel-body", "context-inspector-body"])],
          [history_view(model)],
        ),
      ],
    )
  );
};

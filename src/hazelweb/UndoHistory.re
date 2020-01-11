module ZList = GeneralUtil.ZList;
/*new edit state, the previous action, id*/
type undo_history_entry = (Statics.edit_state, option(Action.t), int);
/* group edit action, group id */
type undo_history_group = (
  ZList.t(undo_history_entry, undo_history_entry),
  int,
  bool,
);
type t = ZList.t(undo_history_group, undo_history_group);

let push_edit_state =
    (
      undo_history: t,
      edit_state: Statics.edit_state,
      action: option(Action.t),
    )
    : t => {
  let (prev_group, prev_group_id, _) = ZList.prj_z(undo_history);
  let (_, prev_action, prev_id) = ZList.prj_z(prev_group);
  if (Action.in_same_history_group(action, prev_action)) {
    /* first add new edit state to the end, then shift_next */
    let after_push = (
      ZList.prj_prefix(prev_group),
      ZList.prj_z(prev_group),
      [(edit_state, action, prev_id + 1)],
    );
    let group_after_push =
      switch (ZList.shift_next(after_push)) {
      | None => failwith("Impossible because suffix is non-empty")
      | Some(new_group) => new_group
      };
    (
      ZList.prj_prefix(undo_history),
      (group_after_push, prev_group_id, false),
      [],
    );
  } else {
    let new_group = (
      ([], (edit_state, action, 0), []),
      prev_group_id + 1,
      false,
    );
    let after_push = (
      ZList.prj_prefix(undo_history),
      ZList.prj_z(undo_history),
      [new_group],
    );
    switch (ZList.shift_next(after_push)) {
    | None => failwith("Impossible because suffix is non-empty")
    | Some(new_history) => new_history
    };
  };
};

let undo = (undo_history: t): t => {
  let (group_now, gp_id, _) = ZList.prj_z(undo_history);
  switch (ZList.shift_prev(group_now)) {
  | None =>
    switch (ZList.shift_prev(undo_history)) {
    | None => undo_history
    | Some(new_history) =>
      let (group_lst, id, _) = ZList.prj_z(new_history);
      let new_group = (ZList.shift_end(group_lst), id, true); /* is_expanded=true because this group should be expanded*/
      ZList.replace_z(new_history, new_group);
    }
  | Some(new_group) =>
    ZList.replace_z(undo_history, (new_group, gp_id, true))
  };
};

let redo = (undo_history: t): t => {
  let (group_now, gp_id, _) = ZList.prj_z(undo_history);
  switch (ZList.shift_next(group_now)) {
  | None =>
    switch (ZList.shift_next(undo_history)) {
    | None => undo_history
    | Some(new_history) =>
      let (group_lst, id, _) = ZList.prj_z(new_history);
      let new_group = (ZList.shift_front(group_lst), id, true); /* is_expanded=true because this group should be expanded when redo*/
      ZList.replace_z(new_history, new_group);
    }
  | Some(new_group) =>
    ZList.replace_z(undo_history, (new_group, gp_id, true))
  };
};

let undoable_action = (action: Action.t): bool => {
  switch (action) {
  | UpdateApPalette(_) =>
    JSUtil.log("UpdateApPalette!!!");
    true;
  | Delete
  | Backspace => true
  | Construct(_) =>
    JSUtil.log("Construct!!!");
    true;
  | MoveTo(_)
  | MoveToBefore(_)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole
  | ShiftLeft
  | ShiftRight
  | ShiftUp
  | ShiftDown => false
  };
};

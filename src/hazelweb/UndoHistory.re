module ZList = GeneralUtil.ZList;
/*new edit state, the previous action, id*/
type undo_history_entry = (Statics.edit_state, option(Action.t), int);
type t = ZList.t(undo_history_entry, undo_history_entry);

let push_edit_state =
    (
      undo_history: t,
      edit_state: Statics.edit_state,
      action: option(Action.t),
    )
    : t => {
  let (_, _, last_id) = ZList.prj_z(undo_history);
  /* first add new edit state to the end, then shift_next */
  let after_push = (
    ZList.prj_prefix(undo_history),
    ZList.prj_z(undo_history),
    [(edit_state, action, last_id + 1)],
  );
  switch (ZList.shift_next(after_push)) {
  | None => failwith("Impossible because suffix is non-empty")
  | Some(new_history) => new_history
  };
};

let undo = (undo_history: t): t => {
  switch (ZList.shift_prev(undo_history)) {
  | None => undo_history
  | Some(new_history) => new_history
  };
};

let redo = (undo_history: t): t => {
  switch (ZList.shift_next(undo_history)) {
  | None => undo_history
  | Some(new_history) => new_history
  };
};

let undoable_action = (action: Action.t): bool => {
  switch (action) {
  | UpdateApPalette(_)
  | Delete
  | Backspace
  | Construct(_) => true
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

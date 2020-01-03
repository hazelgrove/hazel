module ZList = GeneralUtil.ZList;

type t = ZList.t(Statics.edit_state, Statics.edit_state);

let push_edit_state = (undo_history: t, edit_state: Statics.edit_state): t => {
  /* first add new edit state to the end, then shift_next */
  let after_push = (
    ZList.prj_prefix(undo_history),
    ZList.prj_z(undo_history),
    [edit_state],
  );
  switch (ZList.shift_next(after_push)) {
  | None => after_push
  | Some(new_history) => new_history
  };
};

let undo_edit_state = (undo_history: t): t => {
  switch (ZList.shift_prev(undo_history)) {
  | None => undo_history
  | Some(new_history) => new_history
  };
};

let redo_edit_state = (undo_history: t): t => {
  switch (ZList.shift_next(undo_history)) {
  | None => undo_history
  | Some(new_history) => new_history
  };
};

module ZList = GeneralUtil.ZList;

type t = ZList.t(Statics.edit_state, Statics.edit_state);

let add_history = (undo_history: t, edit_state: Statics.edit_state): t => {
  /* first add new edit state to the end, then shift_next */
  let add_new = (
    ZList.prj_prefix(undo_history),
    ZList.prj_z(undo_history),
    [edit_state],
  );
  ZList.shift_next(add_new);
};

let undo_edit_state = (undo_history: t): option(t) => {
  switch (ZList.prj_prefix(undo_history)) {
  | [] => None
  | _ => Some(ZList.shift_prev(undo_history))
  };
};

let redo_edit_state = (undo_history: t): option(t) => {
  switch (ZList.prj_suffix(undo_history)) {
  | [] => None
  | _ => Some(ZList.shift_next(undo_history))
  };
};

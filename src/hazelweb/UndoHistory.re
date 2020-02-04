type cursor_term = CursorInfo.cursor_term;
type undo_history_entry = {
  cardstacks_state: CardStacks.cardstacks_state,
  previous_action: option(Action.t),
  previous_cursor_term: option(cursor_term),
  current_cursor_term: option(cursor_term),
};

type undo_history_group = {
  group_entries: ZList.t(undo_history_entry, undo_history_entry),
  is_expanded: bool,
  /* [is_complete: bool] if any cursor-moving action interupts the current edit,
     the current group becomes complete.
     Next action will start a new group */
  is_complete: bool,
};

type t = ZList.t(undo_history_group, undo_history_group);

let get_cursor_term =
    (cardstacks_state: CardStacks.cardstacks_state): option(cursor_term) => {
  let edit_state =
    ZList.prj_z(ZList.prj_z(cardstacks_state).zcards).edit_state;
  let (zexp, _, _) = edit_state;
  CursorInfo.extract_cursor_exp_term(zexp);
};

let undoable_action = (action: option(Action.t)): bool => {
  switch (action) {
  | None =>
    failwith(
      "Impossible match. None of None-action will be pushed into history",
    )
  | Some(action') =>
    switch (action') {
    | UpdateApPalette(_) =>
      failwith("ApPalette is not implemented in undo_history")
    | Delete
    | Backspace
    | Construct(_) => true
    | MoveTo(_)
    | MoveToBefore(_)
    | MoveLeft
    | MoveRight
    | MoveToNextHole
    | MoveToPrevHole => false
    }
  };
};

let in_same_history_group =
    (entry_1: undo_history_entry, entry_2: undo_history_entry): bool => {
  switch (entry_1.previous_action, entry_2.previous_action) {
  | (None, _)
  | (_, None) => false
  | (Some(detail_action_1), Some(detail_action_2)) =>
    switch (detail_action_1, detail_action_2) {
    | (Delete, Delete)
    | (Backspace, Backspace) =>
      CursorInfo.can_group_cursor_term(
        entry_1.current_cursor_term,
        entry_2.current_cursor_term,
      )
    | (Construct(shape_1), Construct(shape_2)) =>
      /* if shapes are similar, then continue to check if they have similar cursor_term */
      if (Action.can_group_shape(shape_1, shape_2)) {
        CursorInfo.can_group_cursor_term(
          entry_1.current_cursor_term,
          entry_2.current_cursor_term,
        );
      } else {
        false;
      }
    | (UpdateApPalette(_), _) =>
      failwith("ApPalette is not implemented in undo_history")
    | (Delete, _)
    | (Backspace, _)
    | (Construct(_), _) => false
    | (MoveTo(_), _)
    | (MoveToBefore(_), _)
    | (MoveLeft, _)
    | (MoveRight, _)
    | (MoveToNextHole, _)
    | (MoveToPrevHole, _) =>
      failwith(
        "Impossible match. Not undoable actions will not be added into history",
      )
    }
  };
};

let push_edit_state =
    (
      undo_history: t,
      prev_cardstacks_state: CardStacks.cardstacks_state,
      cur_cardstacks_state: CardStacks.cardstacks_state,
      action: option(Action.t),
    )
    : t => {
  let cur_group = ZList.prj_z(undo_history);
  let cur_entry = ZList.prj_z(cur_group.group_entries);
  if (undoable_action(action)) {
    let new_entry = {
      cardstacks_state: cur_cardstacks_state,
      previous_action: action,
      previous_cursor_term: get_cursor_term(prev_cardstacks_state),
      current_cursor_term: get_cursor_term(cur_cardstacks_state),
    };
    if (!cur_group.is_complete && in_same_history_group(cur_entry, new_entry)) {
      /* group the new entry into the current group */
      let group_entries_after_push = (
        [],
        new_entry,
        [
          ZList.prj_z(cur_group.group_entries),
          ...ZList.prj_suffix(cur_group.group_entries),
        ],
      );
      (
        [],
        {
          group_entries: group_entries_after_push,
          is_expanded: false,
          is_complete: false,
        }, /* initial expanded-state of a group should be folded*/
        ZList.prj_suffix(undo_history),
      );
    } else {
      /* start a new group */
      let new_group = {
        group_entries: ([], new_entry, []),
        is_expanded: false,
        is_complete: false,
      };
      (
        [],
        new_group,
        [ZList.prj_z(undo_history), ...ZList.prj_suffix(undo_history)],
      );
    };
  } else {
    /* if any cursor-moving action interupts the current edit,
       the current group becomes complete. */
    let cur_group' = {...cur_group, is_complete: true};
    ZList.replace_z(cur_group', undo_history);
  };
};

let set_all_hidden_history = (undo_history: t, expanded: bool): t => {
  let hidden_group = (group: undo_history_group) => {
    ...group,
    is_expanded: expanded,
  };
  (
    List.map(hidden_group, ZList.prj_prefix(undo_history)),
    hidden_group(ZList.prj_z(undo_history)),
    List.map(hidden_group, ZList.prj_suffix(undo_history)),
  );
};

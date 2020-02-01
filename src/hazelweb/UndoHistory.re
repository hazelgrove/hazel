type cursor_term = CursorInfo.cursor_term;
type undo_history_entry = {
  cardstacks_state: CardStacks.cardstacks_state,
  previous_action: option(Action.t),
  cursor_term: option(cursor_term),
};

let get_cursor_term =
    (cardstacks_state: CardStacks.cardstacks_state): option(cursor_term) => {
  let edit_state =
    ZList.prj_z(ZList.prj_z(cardstacks_state).zcards).edit_state;
  let (zexp, _, _) = edit_state;
  CursorInfo.extract_cursor_exp_term(zexp);
};

type undo_history_group = {
  group_entries: ZList.t(undo_history_entry, undo_history_entry),
  is_expanded: bool,
  is_complete: bool,
};

type t = ZList.t(undo_history_group, undo_history_group);

let undoable_action = (action: option(Action.t)): bool => {
  switch (action) {
  | None => failwith("Impossible, no None action will be pushed into history")
  | Some(action') =>
    switch (action') {
    | UpdateApPalette(_)
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
    (action_1: option(Action.t), action_2: option(Action.t)): bool => {
  switch (action_1, action_2) {
  | (None, _)
  | (_, None) => false
  | (Some(detail_action_1), Some(detail_action_2)) =>
    switch (detail_action_1, detail_action_2) {
    | (UpdateApPalette(_), UpdateApPalette(_))
    | (Delete, Delete)
    | (Backspace, Backspace) => true
    | (Construct(shape_1), Construct(shape_2)) =>
      Action.is_same_shape(shape_1, shape_2)
    | (UpdateApPalette(_), _)
    | (Delete, _)
    | (Backspace, _)
    | (Construct(_), _) => false
    | (MoveTo(_), _)
    | (MoveToBefore(_), _)
    | (MoveLeft, _)
    | (MoveRight, _)
    | (MoveToNextHole, _)
    | (MoveToPrevHole, _) =>
      failwith("not undoable actions, will not be matched")
    }
  };
};

let push_edit_state =
    (
      undo_history: t,
      cardstacks_state: CardStacks.cardstacks_state,
      action: option(Action.t),
    )
    : t => {
  let cur_group = ZList.prj_z(undo_history);
  let cur_entry = ZList.prj_z(cur_group.group_entries);
  if (undoable_action(action)) {
    if (!cur_group.is_complete
        && in_same_history_group(action, cur_entry.previous_action)) {
      let new_entry = {
        cardstacks_state,
        previous_action: action,
        cursor_term: get_cursor_term(cardstacks_state),
      };
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
        }, /* initial state of group should be folded*/
        ZList.prj_suffix(undo_history),
      );
    } else {
      let new_group = {
        group_entries: (
          [],
          {
            cardstacks_state,
            previous_action: action,
            cursor_term: get_cursor_term(cardstacks_state),
          },
          [],
        ),
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
    let new_group = {...cur_group, is_complete: true};
    ZList.replace_z(new_group, undo_history);
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

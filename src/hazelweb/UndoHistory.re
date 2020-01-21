module ZList = GeneralUtil.ZList;

type undo_history_entry = {
  cardstacks_state: CardStacks.cardstacks_state,
  previous_action: option(Action.t),
};

type undo_history_group = {
  group_entries: ZList.t(undo_history_entry, undo_history_entry),
  is_expanded: bool,
};

type t = ZList.t(undo_history_group, undo_history_group);

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

let push_edit_state =
    (
      undo_history: t,
      cardstacks_state: CardStacks.cardstacks_state,
      action: option(Action.t),
    )
    : t => {
  let cur_group = ZList.prj_z(undo_history);
  let cur_state = ZList.prj_z(cur_group.group_entries);
  if (Action.in_same_history_group(action, cur_state.previous_action)) {
    let new_state = {cardstacks_state, previous_action: action};
    let group_entries_after_push = (
      [],
      new_state,
      [
        ZList.prj_z(cur_group.group_entries),
        ...ZList.prj_suffix(cur_group.group_entries),
      ],
    );
    (
      [],
      {group_entries: group_entries_after_push, is_expanded: false}, /* initial state of group should be folded*/
      ZList.prj_suffix(undo_history),
    );
  } else {
    let new_group = {
      group_entries: ([], {cardstacks_state, previous_action: action}, []),
      is_expanded: false,
    };
    (
      [],
      new_group,
      [ZList.prj_z(undo_history), ...ZList.prj_suffix(undo_history)],
    );
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

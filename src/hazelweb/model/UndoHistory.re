type cursor_term = CursorInfo.cursor_term;
type structure =
  | LetBinding
  | Lamda
  | CaseMatch
  | TypeAnn
type edit_action =
  | Delete(int)
  | Insert(int)
  | Edit
  | Construct(structure)
  | DeleteStructure(structure)
  | MoveCursor

type info = {
  previous_action: Action.t,
  previous_cursor_term: cursor_term,
  current_cursor_term: cursor_term,
  prev_is_empty_line: bool,
  edit_action,
}
type undo_history_entry = {
  cardstacks: Cardstacks.t,
  info: option(cursor_info);
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

let get_cursor_info =
    (cardstacks: Cardstacks.t): (option(cursor_term), bool) => {
  let zexp =
    ZList.prj_z(ZList.prj_z(cardstacks).zcards).program |> Program.get_zexp;
  CursorInfo.extract_cursor_term(zexp);
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
let get_last_history_entry = (group: undo_history_group):undo_history_entry => {
  switch(ZList.prj_suffix(group.group_entries)){
  | [] => ZList.prj_z(group.group_entries)
  | [head,..tail] => head;
  }
}
let get_cursor_pos = (cursor_term:cursor_term): CursorPosition.t => {
  switch (cursor_term) {
    | Exp(cursor_pos, _)
    | Pat(cursor_pos, _)
    | Typ(cursor_pos, _)
    | ExpOp(cursor_pos, _)
    | PatOp(cursor_pos, _)
    | TypOp(cursor_pos, _)
    | Line(cursor_pos, _)
    | Rule(cursor_pos, _) => cursor_pos
    }
}
let join_group = (prev_group: undo_history_group, new_entry:undo_history_entry): option(undo_history_group) => {
  let prev_last_entry = get_last_history_entry(prev_group);
  switch(prev_last_entry.info, new_entry.info){
    | (None, _)
    | (_, None) => None
    | (Some(prev_entry_info),Some(new_entry_info)) => {
      switch(prev_entry_info.previous_action, new_entry_info.previous_action) {
        | (Delete, Delete) => {
          /* if the cursor in the previous entry reaches OnDelim-before */
        }
        | (Backspace, Backspace) 
        | (Construct(shape_1), Construct(shape_2)) =>
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
    }
  }

}
let in_same_history_group =
    (~prev_entry: undo_history_entry, ~cur_entry: undo_history_entry): bool => {
  switch (prev_entry.previous_action, cur_entry.previous_action) {
  | (None, _)
  | (_, None) => false
  | (Some(detail_action_1), Some(detail_action_2)) =>
    switch (detail_action_1, detail_action_2) {
    | (Delete, Delete)
    | (Backspace, Backspace) =>
      CursorInfo.can_group_cursor_term(
        prev_entry.current_cursor_term,
        cur_entry.current_cursor_term,
      )
    | (Construct(shape_1), Construct(shape_2)) =>
      /* if shapes are similar, then continue to check if they have similar cursor_term */
      if (Action.can_group_shape(shape_1, shape_2)) {
        CursorInfo.can_group_cursor_term(
          prev_entry.current_cursor_term,
          cur_entry.current_cursor_term,
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
      prev_cardstacks: Cardstacks.t,
      cur_cardstacks: Cardstacks.t,
      action: option(Action.t),
    )
    : t => {
  let prev_group = ZList.prj_z(undo_history);
  let prev_entry = ZList.prj_z(prev_group.group_entries);
  if (undoable_action(action)) {
    let (prev_cursor_term, _) = get_cursor_info(prev_cardstacks);
    let (cur_cursor_term, prev_is_empty_line) =
      get_cursor_info(cur_cardstacks);
    let cur_entry = {
      cardstacks: cur_cardstacks,
      previous_action: action,
      previous_cursor_term: prev_cursor_term,
      current_cursor_term: cur_cursor_term,
      prev_is_empty_line,
    };
    if (!prev_group.is_complete
        && in_same_history_group(~prev_entry, ~cur_entry)) {
      /* group the new entry into the current group */
      let group_entries_after_push = (
        [],
        cur_entry,
        [
          ZList.prj_z(prev_group.group_entries),
          ...ZList.prj_suffix(prev_group.group_entries),
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
        group_entries: ([], cur_entry, []),
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
    let prev_group' = {...prev_group, is_complete: true};
    ZList.replace_z(prev_group', undo_history);
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

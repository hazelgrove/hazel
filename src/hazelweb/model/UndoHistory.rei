[@deriving sexp]
type undo_history_entry = {
  /* cardstacks after non-movement action applied */
  cardstacks_after_action: ZCardstacks.t /* cardstacks_after_move is initially the same as cardstacks_after_action.   if there is a movement action, update it. */,
  cardstacks_after_move: ZCardstacks.t,
  cursor_term_info: UndoHistoryCore.cursor_term_info,
  previous_action: ModelAction.t,
  action_group: UndoHistoryCore.action_group,
  timestamp: UndoHistoryCore.timestamp,
};

[@deriving sexp]
type undo_history_group = {
  group_entries: ZList.t(undo_history_entry, undo_history_entry),
  is_expanded: bool,
};

[@deriving sexp]
type t = {
  groups: ZList.t(undo_history_group, undo_history_group) /* expand all groups */,
  all_hidden_history_expand: bool /* history panel automatically scrolls current entry into view,   but this behavior should be disabled when user is hovering over panel */,
  disable_auto_scrolling: bool,
  preview_on_hover: bool /* group id to restore upon stopping hover (undefined when not hovering) */,
  hover_recover_group_id: int /* element id to restore upon stopping hover (undefined when not hovering) */,
  hover_recover_elt_id: int,
  cur_group_id: int,
  cur_elt_id: int,
};

let get_cardstacks: (t, ~is_after_move: bool) => ZCardstacks.t;

/**
 * Returns whether there are no more entries prior to current entry
 */
let disable_undo: t => bool;

/**
 * Returns whether there are no more entries following current entry
 */
let disable_redo: t => bool;

let shift_to_prev: t => t;
let shift_to_next: t => t;

/**
 * TODO standardize whether we use set or toggle
 */
let update_disable_auto_scrolling: (bool, t) => t;
let toggle_all_hidden_history: t => t;

/**
 * `shift_history(gid, eid, b, history)` returns a new undo history
 * where the current entry has group id `gid` and element id `eid`.
 * The boolean flag `b` indicates whether the user requested this
 * shift by hovering (as opposed to clicking).
 */
let shift_history: (int, int, bool, t) => t;

/**
 * `push_edit_state(history, before, after, a)` adds a new history
 * entry for performing action `a` starting with cardstacks `before`
 * and resulting in cardstacks `after`.
 */
let push_edit_state: (t, ZCardstacks.t, ZCardstacks.t, ModelAction.t) => t;

/**
 * Returns the `UndoHistoryCore.cursor_term_info` associated with
 * the action that was performed on `new_cardstacks_before` and
 * resulted in `new_cardstacks_after`.
 */
let get_cursor_term_info:
  (
    ~new_cardstacks_after: ZCardstacks.t,
    ~new_cardstacks_before: ZCardstacks.t
  ) =>
  UndoHistoryCore.cursor_term_info;

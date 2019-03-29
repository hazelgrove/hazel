/* The CodeHistory represents the history of the codebase,
 * and the user's current position within it.
 */
type t = {
  actions: list(Action.t), /* all of the actions committed to the history */
  action_count: int, /* the number of actions committed */
  actions_undone: int, /* current index in committed history (navigated with undo/redo) */
  undo_jumps: list(int), /* the distance between the actions undone in each undo operation */
  uncommitted_moves: list(Action.t), /* move actions yet to be committed to the history */
  snapshots: list(ZExp.t) /* prevents having to totally replay history on undo/redo */
};

let empty: t = {
  actions: [],
  action_count: 0,
  actions_undone: 0,
  undo_jumps: [],
  uncommitted_moves: [],
  snapshots: [],
};

/* Add a new action to the history. If this is a non-move action,
 * we delete any existing history which had been undone by the user.
 */
let add = (action: Action.t, history: t): t =>
  if (Action.is_move(action)) {
    /* when the undo stack is non-empty, we need to record cursor moves without erasing it */
    if (history.actions_undone > 0) {
      {
        ...history,
        uncommitted_moves: [action, ...history.uncommitted_moves],
      };
    } else {
      {
        ...history,
        actions: [action, ...history.actions],
        action_count: history.action_count + 1,
      };
    };
  } else {
    {
      /* erase the undo stack, and commit any uncommitted move actions */
      actions: [
        action,
        ...history.uncommitted_moves
           @ GeneralUtil.drop(history.actions_undone, history.actions),
      ],
      action_count:
        history.action_count
        + 1
        + List.length(history.uncommitted_moves)
        - history.actions_undone,
      actions_undone: 0,
      undo_jumps: [],
      uncommitted_moves: [],
      snapshots: history.snapshots,
    };
  };

/* Uses the given function to execute all actions up to the current position in the (committed) history. */
let execute_actions = (do_action: Action.t => unit, history: t) => {
  let actions = GeneralUtil.drop(history.actions_undone, history.actions);
  GeneralUtil.rev_iter(do_action, actions);
};

/* Find the most recent non-move action performed, returning its depth */
let find_most_recent_action = actions => {
  let rec find_next = (actions, depth) => {
    switch (actions) {
    | [head, ...tail] =>
      Action.is_move(head) ? find_next(tail, depth + 1) : depth
    | [] => depth
    };
  };
  find_next(actions, 0);
};

/* Undoes the last performed action, returning the new history */
let undo = (history: t): option(t) =>
  /* If we can't undo, don't return anything. */
  if (history.actions_undone < history.action_count) {
    let curr_actions =
      GeneralUtil.drop(history.actions_undone, history.actions);
    let depth = find_most_recent_action(curr_actions);
    let jump = depth + 1;
    let new_history = {
      ...history,
      actions_undone: history.actions_undone + jump,
      undo_jumps: [jump, ...history.undo_jumps],
      uncommitted_moves: [] /* delete these moves: they won't be needed */
    };
    Some(new_history);
  } else {
    None;
  };

/* Redoes the next action, returning the new history */
let redo = (history: t): option(t) =>
  switch (history.undo_jumps) {
  | [jump, ...rem_jumps] =>
    Some({
      ...history,
      actions_undone: history.actions_undone - jump,
      undo_jumps: rem_jumps,
      uncommitted_moves: [] /* delete these moves: they're overridden by the re-visited history */
    })
  | [] => None
  };

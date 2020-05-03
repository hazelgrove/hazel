/* The CodeHistory represents the history of the codebase,
 * and the user's current position within it.
 */
type t = {
  actions: list(Action.t), /* all of the actions ever performed */
  action_count: int,
  undo_count: int, /* current position in history */
  snapshots: list(ZExp.t) /* prevents having to totally replay history on undo/redo */
};

let empty: t = {actions: [], action_count: 0, undo_count: 0, snapshots: []};

/* Add a new action to the history, dropping any existing
 * history which has been undone by the user.
 */
let add = (action: Action.t, history: t): t => {
  actions: [action, ...ListUtil.drop(history.undo_count, history.actions)],
  action_count: history.action_count - history.undo_count + 1,
  undo_count: 0,
  snapshots: history.snapshots,
};

/* Construct the expression corresponding to the current position in the history. */
let construct_code = (_history: t): ZExp.t => {
  /* TODO: Implement this. For now, I'm constructing a dummy expression (constant 7) */
  UHExp.IntLit(NotInHole, "7")
  |> ZExp.place_before_operand
  |> ZExp.ZBlock.wrap;
};

/* Undoes the last performed action, and returns the corresponding expression. */
let undo = (history: t): (t, option(ZExp.t)) =>
  /* If we can't undo, don't return anything. */
  if (history.undo_count < history.action_count) {
    let new_history = {...history, undo_count: history.undo_count + 1};
    (new_history, Some(construct_code(new_history)));
  } else {
    (history, None);
  };

/* Redoes the next action, and returns the corresponding expression. */
let redo = (history: t): (t, option(ZExp.t)) =>
  if (history.undo_count > 0) {
    let new_history = {...history, undo_count: history.undo_count - 1};
    (new_history, Some(construct_code(new_history)));
  } else {
    (history, None);
  };

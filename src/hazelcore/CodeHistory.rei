/* The CodeHistory represents the history of the codebase,
 * and the user's current position within it.
 */
type t = {
  actions: list(Action_common.t), /* all of the actions ever performed */
  action_count: int,
  undo_count: int, /* current position in history */
  snapshots: list(ZExp.t) /* prevents having to totally replay history on undo/redo */
};

let empty: t;

/* Add a new action to the history, dropping any existing
 * history which has been undone by the user.
 */
let add: (Action_common.t, t) => t;

/* Construct the expression corresponding to the current position in the history. */
let construct_code: t => ZExp.t;

/* Undoes the last performed action, and returns the corresponding expression. */
let undo: t => (t, option(ZExp.t));

/* Redoes the next action, and returns the corresponding expression. */
let redo: t => (t, option(ZExp.t));

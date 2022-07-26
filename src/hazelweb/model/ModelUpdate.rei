open Model;

type deferred_action = Lwt.t(ModelAction.t);
type deferred_actions = list(deferred_action);

let perform_edit_action: (Action.t, t) => (t, deferred_actions);

let move_via_key: (MoveKey.t, t) => (t, deferred_actions);
let move_via_click: (Pretty.MeasuredPosition.t, t) => (t, deferred_actions);

/**
 * See `Program.move_to_case_branch`
 */
let select_case_branch: (CursorPath.steps, int, t) => (t, deferred_actions);

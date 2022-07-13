open Model;

type deferred_action = Lwt.t(option(ModelAction.t));

let perform_edit_action: (Action.t, t) => (t, deferred_action);

let move_via_key: (MoveKey.t, t) => (t, deferred_action);
let move_via_click: (Pretty.MeasuredPosition.t, t) => (t, deferred_action);

/**
 * See `Program.move_to_case_branch`
 */
let select_case_branch: (CursorPath.steps, int, t) => (t, deferred_action);

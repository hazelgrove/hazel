open Model;

let perform_edit_action: (Action.t, t) => (t, Lwt.t(ModelAction.t));

let move_via_key: (MoveKey.t, t) => (t, Lwt.t(ModelAction.t));
let move_via_click:
  (Pretty.MeasuredPosition.t, t) => (t, Lwt.t(ModelAction.t));

/**
 * See `Program.move_to_case_branch`
 */
let select_case_branch:
  (CursorPath.steps, int, t) => (t, Lwt.t(ModelAction.t));

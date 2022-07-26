let perform_edit_action:
  (Model.t, State.t, Action.t, ~schedule_action: ModelAction.t => unit) =>
  Model.t;

let move_via_key:
  (Model.t, State.t, MoveKey.t, ~schedule_action: ModelAction.t => unit) =>
  Model.t;
let move_via_click:
  (
    Model.t,
    State.t,
    Pretty.MeasuredPosition.t,
    ~schedule_action: ModelAction.t => unit
  ) =>
  Model.t;

/**
 * See `Program.move_to_case_branch`
 */
let select_case_branch:
  (
    Model.t,
    State.t,
    CursorPath.steps,
    int,
    ~schedule_action: ModelAction.t => unit
  ) =>
  Model.t;

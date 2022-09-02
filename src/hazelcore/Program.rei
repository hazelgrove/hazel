/**
 * A Hazel program ready for user interaction.
 * Contains, in addition to `Statics.edit_state`,
 * user interface state such as the current width of
 * the editor, whether the editor is focused, etc.
 */
[@deriving sexp]
type t =
  pri {
    edit_state: Statics.edit_state,
    width: int,
    start_col_of_vertical_movement: option(int),
    is_focused: bool,
  };

let mk: (~width: int, ~is_focused: bool=?, Statics.edit_state) => t;

let focus: t => t;
let blur: t => t;

let get_zexp: t => ZExp.t;
let get_uhexp: t => UHExp.t;

let get_path: t => CursorPath.t;
let get_steps: t => CursorPath.steps;

let get_id_gen: t => IDGen.t;
/**
 * Raised when `CursorInfo_Exp.syn_cursor_info` returns None
 * (indicates a bug, either in that function or in Action
 * because Action needs to return a well-typed edit state)
 */
exception MissingCursorInfo;
let get_cursor_info: t => CursorInfo.t;

let get_decoration_paths: t => UHDecorationPaths.t;

/**
 * Raised when edit state does not elaborate
 * (indicates a bug, either in that function or in Action
 * because Action needs to return a well-typed edit state) */
exception DoesNotElaborate;
let get_elaboration: t => DHExp.t;

/**
 * Raised when evaluation fails (indicates a bug, either in that function or in
 * Action because Action needs to return a well-typed edit state)
 */
exception EvalError(EvaluatorError.t);
let get_result: t => EvaluatorResult.t;
let elaborate_only: t => EvaluatorResult.t;

/**
 * Raised when an attempted edit action does not succeed
 */
exception FailedAction;
exception CursorEscaped;
let perform_edit_action: (Action.t, t) => t;
let move_via_key: (~settings: Settings.t, MoveKey.t, t) => (t, Action.t);
let move_via_click:
  (~settings: Settings.t, Pretty.MeasuredPosition.t, t) => (t, Action.t);

exception HoleNotFound;
let move_to_hole: (MetaVar.t, t) => Action.t;

/**
 * `move_to_case_branch(steps, n)` returns an action that moves the cursor to
 * the `n`th branch in case expression found at `steps` (when the user
 * clicks on a branch type in the error message for a case expression with
 * inconsistent branches)
 */
let move_to_case_branch: (CursorPath.steps, int) => Action.t;
let move_to_list_element: CursorPath.t => Action.t;

let get_layout: (~settings: Settings.t, t) => UHLayout.t;

let cursor_on_exp_hole: t => option(MetaVar.t);

let get_caret_position:
  (~settings: Settings.t, t) => Pretty.MeasuredPosition.t;

let get_path_to_test: (t, KeywordID.t) => option(CursorPath.t);

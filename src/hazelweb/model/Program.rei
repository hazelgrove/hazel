[@deriving sexp]
type current_splice = option((MetaVar.t, SpliceName.t));

/**
 * A Hazel program ready for user interaction.
 * Contains, in addition to `Statics_common.edit_state`,
 * user interface state such as the current width of
 * the editor, whether the editor is focused, etc.
 */
[@deriving sexp]
type t =
  pri {
    edit_state: Statics_common.edit_state,
    width: int,
    start_col_of_vertical_movement: option(int),
    is_focused: bool,
  };

let mk: (~width: int, ~is_focused: bool=?, Statics_common.edit_state) => t;

let focus: t => t;
let blur: t => t;

let get_zexp: t => ZExp.t;
let get_uhexp: t => UHExp.t;

let get_path: t => CursorPath_common.t;
let get_steps: t => CursorPath_common.steps;

/**
 * Raised when `CursorInfo_Exp.syn_cursor_info` returns None
 * (indicates a bug, either in that function or in Action
 * because Action needs to return a well-typed edit state)
 */
exception MissingCursorInfo;
let get_cursor_info: t => CursorInfo_common.t;

let get_decoration_paths: t => UHDecorationPaths.t;

/**
 * Raised when edit state does not elaborate
 * (indicates a bug, either in that function or in Action
 * because Action needs to return a well-typed edit state) */
exception DoesNotElaborate;
let get_expansion: (~livelit_holes: bool=?, t) => DHExp.t;

/**
 * Raised when evaluation fails with the InvalidInput output
 * (indicates a bug, either in that function or in Action
 * because Action needs to return a well-typed edit state)
 */
exception InvalidInput;
let get_result: t => Result.t;

/**
 * Raised when an attempted edit action does not succeed
 */
exception FailedAction;
exception CursorEscaped;
let perform_edit_action: (Action_common.t, t) => t;
let move_via_key:
  (
    ~measure_program_get_doc: bool,
    ~measure_layoutOfDoc_layout_of_doc: bool,
    ~memoize_doc: bool,
    MoveKey.t,
    t
  ) =>
  (t, Action_common.t);
let move_via_click:
  (
    ~measure_program_get_doc: bool,
    ~measure_layoutOfDoc_layout_of_doc: bool,
    ~memoize_doc: bool,
    option((MetaVar.t, SpliceName.t)),
    Pretty.MeasuredPosition.t,
    t
  ) =>
  (t, Action_common.t);

exception NodeNotFound;
let move_to_node: (TaggedNodeInstance.kind, MetaVar.t, t) => Action_common.t;

/**
 * `move_to_case_branch(steps, n)` returns an action that moves the cursor to
 * the `n`th branch in case expression found at `steps` (when the user
 * clicks on a branch type in the error message for a case expression with
 * inconsistent branches)
 */
let move_to_case_branch: (CursorPath_common.steps, int) => Action_common.t;

let get_doc:
  (~measure_program_get_doc: bool, ~memoize_doc: bool, t) =>
  UHDoc_common.with_splices;
let get_layout:
  (
    ~measure_program_get_doc: bool,
    ~measure_layoutOfDoc_layout_of_doc: bool,
    ~memoize_doc: bool,
    t
  ) =>
  UHLayout.with_splices;

let get_caret_position:
  (
    ~measure_program_get_doc: bool,
    ~measure_layoutOfDoc_layout_of_doc: bool,
    ~memoize_doc: bool,
    t
  ) =>
  (Pretty.MeasuredPosition.t, current_splice);

let cursor_on_inst: t => option((TaggedNodeInstance.kind, MetaVar.t));
let cursor_through_insts: t => list((TaggedNodeInstance.kind, MetaVar.t));

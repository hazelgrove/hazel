[@deriving sexp]
type current_splice = option((MetaVar.t, SpliceName.t));

module EditState: {
  [@deriving sexp]
  type term =
    | Focused(ZExp.t)
    | Unfocused(UHExp.t);

  [@deriving sexp]
  type t = {
    term,
    ty: HTyp.t,
    u_gen: MetaVarGen.t,
  };

  let focus: t => t;
  let blur: t => t;
};

/**
 * A Hazel program ready for user interaction.
 * Contains, in addition to the term edit state,
 * user interface state such as the current width of
 * the editor, whether the editor is focused, etc.
 */
[@deriving sexp]
type t = {
  edit_state: EditState.t,
  width: int,
  start_col_of_vertical_movement: option(int),
};

let mk: (~width: int, EditState.t) => t;

let focus: t => t;
let blur: t => t;

let get_zexp: t => option(ZExp.t);
let get_uhexp: t => UHExp.t;

let get_path: t => option(CursorPath.t);

/**
 * Raised when the program is focused and
 * `CursorInfo_Exp.syn_cursor_info` returns None
 * (indicates a bug, either in that function or in Action
 * because Action needs to return a well-typed edit state)
 */
exception MissingCursorInfo;
let get_cursor_info: t => option(CursorInfo.t);

let get_decoration_paths: t => UHDecorationPaths.t;

/**
 * Raised when term does not elaborate
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
let perform_action:
  (~settings: Settings.t, ~move_via: MoveInput.t=?, Action.t, t) => t;

exception NodeNotFound;
let move_to_node: (TaggedNodeInstance.kind, MetaVar.t, t) => Action.t;

/**
 * `move_to_case_branch(steps, n)` returns an action that moves the cursor to
 * the `n`th branch in case expression found at `steps` (when the user
 * clicks on a branch type in the error message for a case expression with
 * inconsistent branches)
 */
let move_to_case_branch: (CursorPath.steps, int) => Action.t;

let target_path_of_key_input:
  (~settings: Settings.t, MoveKey.t, t) => option(CursorPath.t);
let target_path_of_click_input:
  (
    ~settings: Settings.t,
    option((MetaVar.t, SpliceName.t)),
    Pretty.MeasuredPosition.t,
    t
  ) =>
  CursorPath.t;

let get_layout: (~settings: Settings.t, t) => UHLayout.with_splices;

let get_caret_position:
  (~settings: Settings.t, t) =>
  option((Pretty.MeasuredPosition.t, current_splice));

let cursor_on_inst: t => option((TaggedNodeInstance.kind, MetaVar.t));
let cursor_through_insts: t => list((TaggedNodeInstance.kind, MetaVar.t));

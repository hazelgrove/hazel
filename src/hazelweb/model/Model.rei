[@deriving sexp]
type t = {
  cardstacks: ZCardstacks.t,
  cell_width: int,
  selected_instances: UserSelectedInstances.t,
  just_selected_instance: bool,
  undo_history: UndoHistory.t,
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  font_metrics: FontMetrics.t,
  is_mac: bool,
  /**
   * Preview on undo history entry mainly implemented by
   * on_mouseenter/leave will not work when scrolling
   * because mouse pointer stay still.
   * Recording mouse_position can retrive elements under mouse
   * to realize preview when scrolling.
   */
  mouse_position: ref(MousePosition.t),
  settings: Settings.t,
};

let cardstack_info: list(CardstackInfo.t);

/**
 * See <https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml>
 */
let cutoff: (t, t) => bool;
let init: unit => t;

let get_program: t => Program.t;
let map_program: (Program.t => Program.t, t) => t;

let get_card: t => ZCard.t;
let get_cardstack: t => Cardstack.t;

let get_cursor_info: t => option(CursorInfo.t);

let get_undo_history: t => UndoHistory.t;
let put_undo_history: (UndoHistory.t, t) => t;

/**
 * Update selected instances when user clicks on a hole
 * instance (in result or context inspector)
 */
let select_instance: (TaggedNodeInstance.t, t) => t;
let get_selected_instance: t => option(TaggedNodeInstance.t);

let prev_card: t => t;
let next_card: t => t;

// TODO(d) remove livelit_move
let perform_action:
  (~livelit_move: bool=?, ~move_via: MoveInput.t=?, Action.t, t) => t;

let move_via_key: (MoveKey.t, t) => option(t);
let move_via_click:
  (option((MetaVar.t, SpliceName.t)), Pretty.MeasuredPosition.t, t) => t;

/**
 * See `Program.move_to_case_branch`
 */
let select_case_branch: (CursorPath.steps, int, t) => t;

/**
 * Show/hide sidebars
 */
let toggle_left_sidebar: t => t;
let toggle_right_sidebar: t => t;

/**
 * Load an expression into the editor
 */
let load_example: (t, UHExp.t) => t;

/**
 * Load a selected cardstack into view
 */
let load_cardstack: (t, int) => t;

/**
 * load_undo_history(model, undo_history, ~is_after_move)
 * updates the model's undo history with undo_history and
 * loads the current undo_history edit state into the cell.
 *
 * There are two methods of undoing/redoing: either by using
 * the usual key combos or by clicking/hovering over an
 * undo history panel entry.
 * For the former, is_after_move should be set to true
 * in order to load the last edit state including movements.
 * For the latter, is_after_move should be set to false
 * in order to load the selected edit state entry ignoring
 * movements.
 */
let load_undo_history: (t, UndoHistory.t, ~is_after_move: bool) => t;

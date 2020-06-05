type compute_results = {
  compute_results: bool,
  show_case_clauses: bool,
  show_fn_bodies: bool,
  show_casts: bool,
  show_unevaluated_expansion: bool,
};

type measurements = {
  measurements: bool,
  model_perform_edit_action: bool,
  program_get_doc: bool,
  layoutOfDoc_layout_of_doc: bool,
  uhcode_view: bool,
  cell_view: bool,
  page_view: bool,
  hazel_create: bool,
  update_apply_action: bool,
};

type t = {
  /**
   * all cardstacks along with current cardstack,
   * current cardstack contains current Program
   */
  cardstacks: ZCardstacks.t,
  cell_width: int,
  /**
   * the instance path to the selected hole instance
   */
  selected_instances: UserSelectedInstances.t,
  undo_history: UndoHistory.t,
  compute_results,
  measurements,
  memoize_doc: bool,
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  font_metrics: FontMetrics.t,
  is_mac: bool,
};

let cardstack_info: list(CardstackInfo.t);

let cutoff: (t, t) => bool;

let init: unit => t;

let get_program: t => Program.t;

let get_edit_state: t => Statics.edit_state;

let get_card: t => ZCard.t;
let get_cardstack: t => Cardstack.t;

let get_cursor_info: t => CursorInfo.t;

let get_undo_history: t => UndoHistory.t;
let put_undo_history: (UndoHistory.t, t) => t;

let focus_cell: t => t;
let blur_cell: t => t;
let is_cell_focused: t => bool;

let select_hole_instance: (HoleInstance.t, t) => t;
let get_selected_hole_instance: t => option(HoleInstance.t);

let prev_card: t => t;
let next_card: t => t;

let perform_edit_action: (Action.t, t) => t;

let move_via_key: (JSUtil.MoveKey.t, t) => t;
let move_via_click: ((CursorMap.Row.t, CursorMap.Col.t), t) => t;

let select_case_branch: (CursorPath.steps, int, t) => t;

let toggle_left_sidebar: t => t;
let toggle_right_sidebar: t => t;

let load_example: (t, UHExp.t) => t;

let load_cardstack: (t, int) => t;

let load_undo_history: (t, UndoHistory.t, ~is_after_move: bool) => t;

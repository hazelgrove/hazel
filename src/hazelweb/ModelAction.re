module EditAction = {
  include Action;
  include Action_common;
};
module Sexp = Sexplib.Sexp;
open Sexplib.Std;

[@deriving sexp]
type move_input =
  | Key(MoveKey.t)
  | Click(Pretty.MeasuredPosition.t);

[@deriving sexp]
type shift_history_info = {
  group_id: int,
  elt_id: int,
  call_by_mouseenter: bool,
};
[@deriving sexp]
type group_id = int;
[@deriving sexp]
type editor_id = int;
let main_editor_id: editor_id = 0;
let assistant_editor_id: editor_id = 1;
[@deriving sexp]
type t =
  | EditAction(EditAction.t) // NOTE(andrew): needs edtior id?
  | MoveAction(move_input) // NOTE(andrew): needs editor id?
  | ToggleLeftSidebar
  | ToggleRightSidebar
  | LoadExample(Examples.id)
  | LoadCardstack(int)
  | NextCard
  | PrevCard
  | UpdateSettings(Settings.update)
  | SelectHoleInstance(HoleInstance.t)
  | SelectCaseBranch(CursorPath.steps, int)
  | InvalidVar(string)
  | FocusCell(editor_id)
  | BlurCell
  | Redo
  | Undo
  | ShiftHistory(shift_history_info)
  | ShiftWhenScroll
  | ToggleHistoryGroup(group_id)
  | ToggleHiddenHistoryAll
  | TogglePreviewOnHover
  | UpdateFontMetrics(FontMetrics.t)
  | UpdateIsMac(bool)
  | AcceptSuggestion(Action.t);

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
  id: UndoHistory.id,
  call_by_mouseenter: bool,
};
[@deriving sexp]
type t =
  | EditAction(EditAction.t)
  | MoveAction(move_input)
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
  | FocusCell
  | BlurCell
  | Redo
  | Undo
  | ShiftHistory(shift_history_info)
  | ShiftWhenScroll
  | ToggleHistoryGroup(UndoHistory.group_id)
  | ToggleHiddenHistoryAll
  | TogglePreviewOnHover
  | UpdateFontMetrics(FontMetrics.t)
  | UpdateIsMac(bool);

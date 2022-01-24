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
type serialize_object =
  | UHExp
  | DHExp
  | ZExp;

[@deriving sexp]
type t =
  | EditAction(EditAction.t)
  | MoveAction(move_input)
  | ToggleLeftSidebar
  | ToggleRightSidebar
  | LoadCard(int)
  | LoadCardstack(int)
  | NextCard
  | PrevCard
  | UpdateSettings(Settings.update)
  | UpdateCursorInspector(CursorInspectorModel.update)
  | SelectHoleClosure(HoleClosure.t)
  | SelectCaseBranch(CursorPath.steps, int)
  | FocusCell
  | BlurCell
  | Redo
  | Undo
  | ShiftHistory(shift_history_info)
  | ToggleHistoryGroup(group_id)
  | ToggleHiddenHistoryAll
  | TogglePreviewOnHover
  | UpdateFontMetrics(FontMetrics.t)
  | SerializeToConsole(serialize_object);

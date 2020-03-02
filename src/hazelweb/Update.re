module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module EditAction = Action;
module Sexp = Sexplib.Sexp;
open Sexplib.Std;
//open ViewUtil;

module Action = {
  [@deriving sexp]
  type t =
    | EditAction(EditAction.t)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id)
    | LoadCardstack(int)
    | NextCard
    | PrevCard
    | SetComputeResults(bool)
    | SetShowContenteditable(bool)
    | SetShowPresentation(bool)
    | SelectHoleInstance(MetaVar.t, MetaVarInst.t)
    | InvalidVar(string)
    | MoveToHole(MetaVar.t)
    | FocusCell
    | BlurCell
    | FocusWindow
    | Redo
    | Undo
    | SelectionChange;
};

/*
 [@deriving sexp]
 type timestamp = {
   year: int,
   month: int,
   day: int,
   hours: int,
   minutes: int,
   seconds: int,
   milliseconds: int,
 };

 [@deriving sexp]
 type timestamped_action = (timestamp, Action.t);

 let get_current_timestamp = (): timestamp => {
   let date = {
     %js
     new Js.date_now;
   };
   {
     year: date##getFullYear,
     month: date##getMonth,
     day: date##getDay,
     hours: date##getHours,
     minutes: date##getMinutes,
     seconds: date##getSeconds,
     milliseconds: date##getMilliseconds,
   };
 };

 let mk_timestamped_action = (a: Action.t) => (get_current_timestamp(), a);

 let log_action = (action: Action.t): unit => {
   // log interesting actions
   switch (action) {
   | EditAction(_)
   | ToggleLeftSidebar
   | ToggleRightSidebar
   | LoadExample(_)
   | LoadCardstack(_)
   | NextCard
   | PrevCard
   | SetComputeResults(_)
   | SetShowContenteditable(_)
   | SetShowPresentation(_)
   | SelectHoleInstance(_)
   | InvalidVar(_)
   | FocusCell
   | BlurCell
   | FocusWindow
   | MoveToHole(_)
   | Undo
   | Redo =>
     Logger.append(
       Sexp.to_string(
         sexp_of_timestamped_action(mk_timestamped_action(action)),
       ),
     )
   | SelectionChange => ()
   };
 };
 */

let perform_edit_action = (a: EditAction.t, model) =>
  switch (model |> Model.perform_edit_action(a)) {
  | new_model => new_model
  | exception Program.FailedAction =>
    JSUtil.log("[Program.FailedAction]");
    model;
  | exception Program.CursorEscaped =>
    JSUtil.log("[Program.CursorEscaped]");
    model;
  | exception Program.MissingCursorInfo =>
    JSUtil.log("[Program.MissingCursorInfo]");
    model;
  | exception Program.InvalidInput =>
    JSUtil.log("[Program.InvalidInput");
    model;
  | exception Program.DoesNotExpand =>
    JSUtil.log("[Program.DoesNotExpand]");
    model;
  };

let apply_action =
    (
      model: Model.t,
      action: Action.t,
      state: State.t,
      ~schedule_action: Action.t => unit,
    )
    : Model.t => {
  // log_action(action);
  switch (action) {
  | EditAction(a) => model |> perform_edit_action(a)
  | ToggleLeftSidebar => Model.toggle_left_sidebar(model)
  | ToggleRightSidebar => Model.toggle_right_sidebar(model)
  | LoadExample(id) => Model.load_example(model, Examples.get(id))
  | LoadCardstack(idx) => Model.load_cardstack(model, idx)
  | NextCard =>
    state.changing_cards := true;
    Model.next_card(model);
  | PrevCard =>
    state.changing_cards := true;
    Model.prev_card(model);
  | SetComputeResults(compute_results) => {...model, compute_results}
  | SetShowContenteditable(show_contenteditable) => {
      ...model,
      show_contenteditable,
    }
  | SetShowPresentation(show_presentation) => {...model, show_presentation}
  | SelectHoleInstance(u, i) => model |> Model.select_hole_instance((u, i))
  | InvalidVar(_) => model
  | MoveToHole(u) => model |> Model.move_to_hole(u)
  | FocusCell => model |> Model.focus_cell
  | FocusWindow =>
    state.setting_caret := true;
    JSUtil.reset_caret();
    model;
  | BlurCell => JSUtil.window_has_focus() ? model |> Model.blur_cell : model
  | Undo =>
    let new_history = UndoHistory.undo(model.undo_history);
    let new_edit_state = ZList.prj_z(new_history);
    let new_model = model |> Model.put_program(Program.mk(new_edit_state));
    {...new_model, undo_history: new_history};
  | Redo =>
    let new_history = UndoHistory.redo(model.undo_history);
    let new_edit_state = ZList.prj_z(new_history);
    let new_model = model |> Model.put_program(Program.mk(new_edit_state));
    {...new_model, undo_history: new_history};
  | SelectionChange =>
    if (! state.setting_caret^) {
      let rev_path =
        CaretMap.revpath_of_anchor(
          ~col_width=state.col_width^,
          ~row_height=state.row_height^,
        );
      let path = CursorPath.rev(rev_path);
      schedule_action(Action.EditAction(MoveTo(path)));
      /*
       let row_col =
         switch (anchor_node |> Dom_html.CoerceTo.element |> Js.Opt.to_option) {
         | None =>
           // anchor_node is text node
           let row =
             anchor_node
             |> JSUtil.force_get_closest_elem
             |> JSUtil.get_id
             |> row_of_row_text_id
             |> Option.get;
           let col = Dom_html.window##getSelection##.anchorOffset;
           (row, col);
         | Some(elem) =>
           // anchor_node is contenteditable element.
           // anchor_offset = n means is on nth child
           // of contenteditable, in particular on a <br>.
           let row = elem |> JSUtil.get_id |> row_of_row_eol_id |> Option.get;
           (row, 0);
         };
       let rev_path = CaretMap.set_caret_rowcol(~state, row_col);
       let path = CursorPath.rev(rev_path);
       schedule_action(Action.EditAction(MoveTo(path)));
       */
    };
    model;
  };
};

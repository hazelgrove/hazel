module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module EditAction = Action;
module Sexp = Sexplib.Sexp;
open Sexplib.Std;
open GeneralUtil;
open ViewUtil;
open SemanticsCommon;

module Action = {
  [@deriving sexp]
  type t =
    | EditAction(EditAction.t)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id)
    | SelectHoleInstance(MetaVar.t, Dynamics.inst_num)
    | InvalidVar(string)
    | MoveToHole(MetaVar.t)
    | SelectionChange
    | FocusCell
    | BlurCell
    | FocusWindow;
};

let closest_elem = node =>
  Js.Opt.get(Dom_html.CoerceTo.element(node), () =>
    switch (node##.nodeType) {
    | TEXT =>
      switch (Js.Opt.to_option(node##.parentNode)) {
      | None => assert(false)
      | Some(parent) =>
        Js.Opt.get(Dom_html.CoerceTo.element(parent), () => assert(false))
      }
    | _ => assert(false)
    }
  );

[@warning "-27"]
let apply_action =
    (
      model: Model.t,
      action: Action.t,
      setting_caret: ref(bool),
      ~schedule_action,
    )
    : Model.t =>
  switch (action) {
  | EditAction(a) =>
    switch (Model.perform_edit_action(model, a)) {
    | new_model => new_model
    | exception Model.FailedAction =>
      JSUtil.log("[Model.FailedAction]");
      model;
    | exception Model.CursorEscaped =>
      JSUtil.log("[CursorEscaped]");
      model;
    | exception Model.MissingCursorInfo =>
      JSUtil.log("[MissingCursorInfo]");
      model;
    | exception Model.InvalidInput =>
      JSUtil.log("[Model.InvalidInput");
      model;
    | exception Model.DoesNotExpand =>
      JSUtil.log("[Model.DoesNotExpand]");
      model;
    }
  | ToggleLeftSidebar => Model.toggle_left_sidebar(model)
  | ToggleRightSidebar => Model.toggle_right_sidebar(model)
  | LoadExample(id) => Model.load_example(model, Examples.get(id))
  | SelectHoleInstance(u, i) => Model.select_hole_instance(model, (u, i))
  | InvalidVar(x) => model
  | MoveToHole(u) => Model.move_to_hole(model, u)
  | FocusCell => model |> Model.focus_cell
  | FocusWindow =>
    setting_caret := true;
    JSUtil.reset_caret();
    model;
  | BlurCell => JSUtil.window_has_focus() ? model |> Model.blur_cell : model
  | SelectionChange =>
    if (! setting_caret^) {
      let anchorNode = Dom_html.window##getSelection##.anchorNode;
      let anchorOffset = Dom_html.window##getSelection##.anchorOffset;
      if (JSUtil.div_contains_node(
            JSUtil.force_get_elem_by_id(cell_id),
            anchorNode,
          )) {
        let closest_elem = closest_elem(anchorNode);
        let has_cls = cls => closest_elem |> JSUtil.elem_has_cls(cls);
        if (has_cls("unselectable")) {
          let s =
            Js.to_string(
              Js.Opt.get(anchorNode##.nodeValue, () =>
                raise(MalformedView(2))
              ),
            );
          let attr =
            anchorOffset <= (String.length(s) - 1) / 2
              ? "path-before" : "path-after";
          let ssexp =
            closest_elem
            |> JSUtil.get_attr(attr)
            |> Opt.get(() => raise(MalformedView(3)));
          let path = Path.t_of_sexp(Sexp.of_string(ssexp));
          schedule_action(Action.EditAction(MoveTo(path)));
        } else if (has_cls(indentation_cls)) {
          switch (
            closest_elem |> JSUtil.get_attr("goto-path"),
            closest_elem |> JSUtil.get_attr("goto-steps"),
          ) {
          | (None, None) => raise(MalformedView(4))
          | (Some(ssexp), _) =>
            let path = Path.t_of_sexp(Sexp.of_string(ssexp));
            schedule_action(Action.EditAction(MoveTo(path)));
          | (_, Some(ssexp)) =>
            let steps = Path.steps_of_sexp(Sexp.of_string(ssexp));
            schedule_action(Action.EditAction(MoveToBefore(steps)));
          };
        } else if (has_cls("sline")
                   && closest_elem
                   |> JSUtil.has_attr("goto-steps")) {
          switch (closest_elem |> JSUtil.get_attr("goto-steps")) {
          | None => assert(false)
          | Some(ssexp) =>
            let steps = Path.steps_of_sexp(Sexp.of_string(ssexp));
            schedule_action(Action.EditAction(MoveToBefore(steps)));
          };
        } else if (has_cls("unselectable-before")
                   && (anchorOffset == 0 || anchorOffset == 1)) {
          switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
          | None => raise(MalformedView(5))
          | Some(path) => schedule_action(Action.EditAction(MoveTo(path)))
          };
        } else if (has_cls("unselectable-before") && anchorOffset == 2) {
          switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
          | None => raise(MalformedView(6))
          | Some(path) => schedule_action(Action.EditAction(MoveLeft))
          };
        } else if (has_cls("unselectable-after")
                   && (anchorOffset == 2 || anchorOffset == 3)) {
          switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
          | None => raise(MalformedView(7))
          | Some(path) => schedule_action(Action.EditAction(MoveTo(path)))
          };
        } else if (has_cls("unselectable-after") && anchorOffset == 1) {
          switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
          | None => raise(MalformedView(8))
          | Some(path) => schedule_action(Action.EditAction(MoveRight))
          };
        } else if (has_cls("SSpace")) {
          let attr = anchorOffset == 0 ? "path-before" : "path-after";
          let ssexp =
            closest_elem
            |> JSUtil.get_attr(attr)
            |> Opt.get(() => raise(MalformedView(9)));
          let path = Path.t_of_sexp(Sexp.of_string(ssexp));
          schedule_action(Action.EditAction(MoveTo(path)));
        } else if (has_cls("SEmptyLine")
                   && (anchorOffset == 0 || anchorOffset == 4)) {
          switch (steps_of_text_id(Js.to_string(closest_elem##.id))) {
          | None => raise(MalformedView(11))
          | Some(steps) =>
            schedule_action(Action.EditAction(MoveTo((steps, OnText(0)))))
          };
        } else if (has_cls("SEmptyLine") && anchorOffset == 1) {
          schedule_action(Action.EditAction(MoveLeft));
        } else if (has_cls("SEmptyLine") && anchorOffset == 3) {
          schedule_action(Action.EditAction(MoveRight));
        } else {
          let is_cursor_position = node =>
            switch (Js.Opt.to_option(Dom_html.CoerceTo.element(node))) {
            | None => None
            | Some(elem) =>
              let id = Js.to_string(elem##.id);
              switch (
                steps_of_text_id(id),
                path_of_path_id(id),
                steps_of_node_id(id),
              ) {
              | (None, None, None) => None
              | (Some(steps), _, _) =>
                Some((steps, Some(OnText(anchorOffset))))
              | (_, Some((steps, cursor)), _) =>
                Some((steps, Some(cursor)))
              | (_, _, Some(steps)) => Some((steps, None))
              };
            };
          let (zblock, _, _) = model.edit_state;
          let (current_steps, current_cursor) = Path.of_zblock(zblock);
          switch (anchorNode |> JSUtil.query_ancestors(is_cursor_position)) {
          | None => ()
          | Some((next_steps, None)) =>
            next_steps == current_steps
              ? ()
              : {
                schedule_action(
                  Action.EditAction(MoveToBefore(next_steps)),
                );
              }
          | Some((next_steps, Some(next_cursor))) =>
            next_steps == current_steps && next_cursor == current_cursor
              ? ()
              : schedule_action(
                  Action.EditAction(MoveTo((next_steps, next_cursor))),
                )
          };
        };
      };
    };
    model;
  };

module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module EditAction = Action;
open Sexplib.Std;
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
    | SetCaret(Path.t);
};

let closest_elem = node =>
  Js.Opt.get(Dom_html.CoerceTo.element(node), () =>
    switch (anchorNode##.nodeType) {
    | TEXT =>
      switch (Js.Opt.to_option(node##.parentNode)) {
      | None => assert(false)
      | Some(parent) =>
        Js.Opt.get(Dom_html.CoerceTo.element(parent), () => assert(false))
      }
    | _ => assert(false)
    }
  );

let set_caret = (caret_node, caret_offset): unit => {
  let selection = Dom_html.window##getSelection;
  let range = Dom_html.document##createRange;
  range##setStart(caret_node, caret_offset);
  range##setEnd(caret_node, caret_offset);
  selection##removeAllRanges;
  selection##addRange(range);
};

[@warning "-27"]
let apply_action =
    (model: Model.t, action: Action.t, _, ~schedule_action): Model.t =>
  switch (action) {
  | EditAction(a) =>
    switch (Model.perform_edit_action(model, a)) {
    | m => m
    | exception Model.FailedAction =>
      JSUtil.log("[FailedAction]");
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
  | SelectionChange =>
    let anchorNode = Dom_html.window##getSelection##.anchorNode;
    let anchorOffset = Dom_html.window##getSelection##.anchorOffset;
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
        | (Some(steps), _, _) => Some((steps, Some(OnText(anchorOffset))))
        | (_, Some((steps, cursor)), _) => Some((steps, Some(cursor)))
        | (_, _, Some(steps)) => Some((steps, None))
        };
      };
    if (JSUtil.div_contains_node(
          JSUtil.forceGetElementById("cell"),
          anchorNode,
        )) {
      let closest_elem = closest_elem(anchorNode);
      let has_cls = JSUtil.elem_has_cls(closest_elem);
      if (has_cls("unselectable")) {
        let s =
          Js.Opt.get(anchorNode##.nodeValue, () => raise(MalformedView));
        let attr =
          anchorOffset <= (List.length(s) - 1) / 2
            ? "path-before" : "path-after";
        let ssexp =
          Js.Opt.get(closest_elem##getAttribute(attr), () =>
            raise(MalformedView)
          );
        let path = Path.t_of_sexp(Sexp.of_string(ssexp));
        schedule_action(Action.EditAction(MoveTo(path)));
      } else if (has_cls("indent")) {
        switch (
          Js.Opt.to_option(closest_elem##getAttribute("goto-path")),
          Js.Opt.to_option(closest_elem##getAttribute("goto-steps")),
        ) {
        | (None, None) => raise(MalformedView)
        | (Some(ssexp), _) =>
          let path = Path.t_of_sexp(Sexp.of_string(ssexp));
          schedule_action(Action.EditAction(MoveTo(path)));
        | (_, Some(ssexp)) =>
          let steps = Path.steps_of_sexp(Sexp.of_string(ssexp));
          schedule_action(Action.EditAction(MoveToBefore(steps)));
        /* TODO caret? */
        };
      } else if (has_cls("unselectable-before") && anchorOffset == 0) {
        switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
        | None => raise(MalformedView)
        | Some(path) =>
          schedule_action(Action.EditAction(MoveTo(path)));
          set_caret(anchorNode, 2);
        };
      } else if (has_cls("unselectable-before") && anchorOffset == 1) {
        switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
        | None => raise(MalformedView)
        | Some(path) =>
          /* TODO something about moving to next cursor position before */
          schedule_action(Action.EditAction(MoveTo(path)));
          set_caret(anchorNode, 2);
        };
      } else if (has_cls("unselectable-after") && anchorOffset == 2) {
        switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
        | None => raise(MalformedView)
        | Some(path) =>
          schedule_action(Action.EditAction(MoveTo(path)));
          set_caret(anchorNode, 0);
        };
      } else if (has_cls("unselectable-after") && anchorOffset == 1) {
        switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
        | None => raise(MalformedView)
        | Some(path) =>
          /* TODO something about moving to next cursor position after */
          schedule_action(Action.EditAction(MoveTo(path)));
          set_caret(anchorNode, 0);
        };
      } else if (has_cls("SSpace")) {
        let attr = anchorOffset == 0 ? "path-before" : "path-after";
        let ssexp =
          Js.Opt.get(closest_elem##getAttribute(attr), () =>
            raise(MalformedView)
          );
        let path = Path.t_of_sexp(Sexp.of_string(ssexp));
        schedule_action(Action.EditAction(MoveTo(path)));
        /* TODO caret? */
      } else {
        let (zblock, _, _) = model.edit_state;
        let (current_steps, current_cursor) = Path.of_zblock(zblock);
        switch (anchorNode |> JSUtil.query_ancestors(is_cursor_position)) {
        | None => ()
        | Some((next_steps, None)) =>
          next_steps == current_steps
            ? ()
            : schedule_action(Action.EditAction(MoveToBefore(next_steps)))
        | Some((steps, Some(cursor))) =>
          next_steps == current_steps && next_cursor == current_cursor
            ? ()
            : schedule_action(
                Action.EditAction(MoveTo((next_steps, next_cursor))),
              )
        };
      };
    };
    model;
  | SetCaret(path) =>
    let (steps, cursor) = path;
    let (caret_node, caret_offset) =
      switch (cursor) {
      | OnDelim(_, _) => (
          (
            JSUtil.forceGetElementById(path_id(path)): Js.t(Dom_html.element) :>
              Js.t(Dom.node)
          ),
          0,
        )
      | OnText(j) => (
          (
            JSUtil.forceGetElementById(text_id(steps)):
              Js.t(Dom_html.element) :>
              Js.t(Dom.node)
          ),
          j,
        )
      };
    set_caret(caret_node, caret_offset);
    model;
  };

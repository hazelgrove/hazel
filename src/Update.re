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
    | SetCaret(Path.t);
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

let set_caret = (caret_node, caret_offset) => {
  let selection = Dom_html.window##getSelection;
  let range = Dom_html.document##createRange;
  range##setStart(caret_node, caret_offset);
  range##setEnd(caret_node, caret_offset);
  selection##removeAllRanges;
  selection##addRange(range);
};

let caret_position_of_path =
    ((steps, cursor) as path): (Js.t(Dom.node), int) =>
  switch (cursor) {
  | OnDelim(_, _) =>
    let anchor_parent = JSUtil.forceGetElementById(path_id(path));
    let has_cls = JSUtil.elem_has_cls(anchor_parent);
    let anchor_offset =
      if (has_cls("unselectable-before")) {
        2;
      } else if (has_cls("unselectable-after")) {
        0;
      } else {
        0;
      };
    let anchor_parent_node = (
      anchor_parent: Js.t(Dom_html.element) :> Js.t(Dom.node)
    );
    let anchor =
      Js.Opt.get(anchor_parent_node##.firstChild, () =>
        raise(MalformedView(0))
      );
    (anchor, anchor_offset);
  | OnText(j) =>
    let anchor_parent = (
      JSUtil.forceGetElementById(text_id(steps)): Js.t(Dom_html.element) :>
        Js.t(Dom.node)
    );
    let anchor =
      Js.Opt.get(anchor_parent##.firstChild, () => raise(MalformedView(1)));
    (anchor, j);
  };

let is_caret_consistent_with_path = path =>
  caret_position_of_path(path)
  == (
       Dom_html.window##getSelection##.anchorNode,
       Dom_html.window##getSelection##.anchorOffset,
     );

[@warning "-27"]
let apply_action =
    (model: Model.t, action: Action.t, _, ~schedule_action): Model.t =>
  switch (action) {
  | EditAction(a) =>
    switch (Model.perform_edit_action(model, a)) {
    | new_model =>
      is_caret_consistent_with_path(new_model |> Model.get_path)
        ? () : schedule_action(Action.SetCaret(new_model |> Model.get_path));
      new_model;
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
  | SelectionChange =>
    let anchorNode = Dom_html.window##getSelection##.anchorNode;
    let anchorOffset = Dom_html.window##getSelection##.anchorOffset;
    if (JSUtil.div_contains_node(
          JSUtil.forceGetElementById("cell"),
          anchorNode,
        )) {
      let closest_elem = closest_elem(anchorNode);
      let has_cls = JSUtil.elem_has_cls(closest_elem);
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
      } else if (has_cls("indent")) {
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
      } else if (has_cls("unselectable-before") && anchorOffset == 0) {
        switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
        | None => raise(MalformedView(5))
        | Some(path) => schedule_action(Action.EditAction(MoveTo(path)))
        };
      } else if (has_cls("unselectable-before") && anchorOffset == 1) {
        switch (path_of_path_id(Js.to_string(closest_elem##.id))) {
        | None => raise(MalformedView(6))
        | Some(path) => schedule_action(Action.EditAction(MoveLeft))
        };
      } else if (has_cls("unselectable-after") && anchorOffset == 2) {
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
            | (_, Some((steps, cursor)), _) => Some((steps, Some(cursor)))
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
            : schedule_action(Action.EditAction(MoveToBefore(next_steps)))
        | Some((next_steps, Some(next_cursor))) =>
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
    let (caret_node, caret_offset) = caret_position_of_path(path);
    set_caret(caret_node, caret_offset);
    model;
  };

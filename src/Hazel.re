module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;
open SemanticsCommon;

// https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml
module Model = MyModel;
module Action = Update.Action;
module State = {
  type t = unit;
};

[@warning "-27"]
let on_startup = (~schedule_action, _) => Async_kernel.return();

let caret_position_of_path = (path: Path.t): (Js.t(Dom.node), int) => {
  let (steps, cursor) = path;
  switch (cursor) {
  | OnDelim(_, _) => (
      (
        JSUtil.forceGetElementById(MyView.path_id(path)):
          Js.t(Dom_html.element) :>
          Js.t(Dom.node)
      ),
      0,
    )
  | OnText(j) => (
      (
        JSUtil.forceGetElementById(MyView.node_id(steps)):
          Js.t(Dom_html.element) :>
          Js.t(Dom.node)
      ),
      j,
    )
  };
};

[@warning "-27"]
let set_cursor = (path: Path.t, _: State.t, ~schedule_action): unit => {
  let selection = Dom_html.window##getSelection;
  let range = Dom_html.document##createRange;
  let (caret_node, caret_offset) = caret_position_of_path(path);
  range##setStart(caret_node, caret_offset);
  range##setEnd(caret_node, caret_offset);
  selection##removeAllRanges;
  selection##addRange(range);
};

[@warning "-27"]
let create = (model, ~old_model, ~inject) => {
  open Incr.Let_syntax;
  let%map model = model;

  Component.create(
    ~apply_action=Update.apply_action(model),
    ~on_display=set_cursor(MyModel.get_path(model)),
    model,
    MyView.view(~inject, model),
  );
};

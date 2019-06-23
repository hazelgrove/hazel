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

[@warning "-27"]
let set_caret = (path: Path.t, _: State.t, ~schedule_action): unit => {
  let (steps, cursor) = path;
  let (caret_node, caret_offset) =
    switch (cursor) {
    | OnDelim(_, _) => (
        (
          JSUtil.forceGetElementById(Code.path_id(path)):
            Js.t(Dom_html.element) :>
            Js.t(Dom.node)
        ),
        0,
      )
    | OnText(j) => (
        (
          JSUtil.forceGetElementById(Code.node_id(steps)):
            Js.t(Dom_html.element) :>
            Js.t(Dom.node)
        ),
        j,
      )
    };

  let selection = Dom_html.window##getSelection;
  let range = Dom_html.document##createRange;
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
    ~on_display=set_caret(MyModel.get_path(model)),
    model,
    Page.view(~inject, model),
  );
};

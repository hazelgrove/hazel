module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;
open ViewUtil;

// https://github.com/janestreet/incr_dom/blob/6aa4aca2cfc82a17bbcc0424ff6b0ae3d6d8d540/example/text_input/README.md
// https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml

module Model = Model;
module Action = Update.Action;
module State = {
  type t = unit;
};

[@warning "-27"]
let on_startup = (~schedule_action, _) => {
  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("selectionchange"), Dom_html.document, _ =>
      schedule_action(Update.Action.SelectionChange)
    );
  Async_kernel.Deferred.return();
};

[@warning "-27"]
let create = (model, ~old_model, ~inject) => {
  open Incr.Let_syntax;
  let%map model = model;
  Component.create(
    ~apply_action=Update.apply_action(model),
    ~on_display=
      (_, ~schedule_action) => {
        let path = model |> Model.path;
        if (model.is_cell_focused) {
          if (!Code.is_caret_consistent_with_path(path)) {
            switch (Code.caret_position_of_path(path)) {
            | None => raise(MalformedView(10))
            | Some((node, offset)) => JSUtil.set_caret(node, offset)
            };
          } else {
            let elems =
              Dom_html.document##getElementsByClassName(Js.string("cursor"))
              |> Dom.list_of_nodeList;
            switch (elems) {
            | [] => ()
            | [cursor_elem, ..._] =>
              switch (Code.child_elems_of_snode_elem(cursor_elem)) {
              | None => raise(MalformedView(11))
              | Some(children_elems) =>
                JSUtil.force_get_elem_by_id(node_indicator_id)
                |> JSUtil.place_over(cursor_elem);
                children_elems
                |> List.iteri((i, child_elem) =>
                     JSUtil.force_get_elem_by_id(child_indicator_id(i))
                     |> JSUtil.place_over(child_elem)
                   );
              }
            };
          };
        };
      },
    model,
    Page.view(~inject, model),
  );
};

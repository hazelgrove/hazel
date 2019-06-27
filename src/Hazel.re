module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;
open SemanticsCommon;

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
      Dom.Event.make("selectionchange"),
      Dom_html.document,
      _ => {
        let anchorNode = Dom_html.window##getSelection##.anchorNode;
        let anchorOffset = Dom_html.window##getSelection##.anchorOffset;
        if (JSUtil.div_contains_node(
              JSUtil.forceGetElementById("cell"),
              anchorNode,
            )) {
          switch (Js.Opt.to_option(Dom_html.CoerceTo.element(anchorNode))) {
          | None => ()
          | Some(elem) =>
            if (JSUtil.elem_has_cls(elem, "not-editable")) {
              (); // caret transport will trigger a second selectionchange
                // event, let second trigger do the work
            } else {
              let is_cursor_position = node =>
                switch (Js.Opt.to_option(Dom_html.CoerceTo.element(node))) {
                | None => None
                | Some(elem) =>
                  let id = Js.to_string(elem##.id);
                  switch (
                    Code.steps_of_steps_id(id),
                    Code.path_of_path_id(id),
                  ) {
                  | (None, None) => None
                  | (Some(steps), _) => Some((steps, OnText(anchorOffset)))
                  | (_, Some(path)) => Some(path)
                  };
                };
              switch (
                anchorNode |> JSUtil.query_ancestors(is_cursor_position)
              ) {
              | None => ()
              | Some(path) =>
                schedule_action(Update.Action.EditAction(MoveTo(path)))
              };
            }
          };
        };
      },
    );
  Async_kernel.Deferred.return();
};

[@warning "-27"]
let create = (model, ~old_model, ~inject) => {
  open Incr.Let_syntax;
  let%map model = model;

  Component.create(
    ~apply_action=Update.apply_action(model),
    model,
    Page.view(~inject, model),
  );
};

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
        let is_cursor_position = node =>
          switch (Js.Opt.to_option(Dom_html.CoerceTo.element(node))) {
          | None => None
          | Some(elem) =>
            let id = Js.to_string(elem##.id);
            switch (
              Code.steps_of_text_id(id),
              Code.path_of_path_id(id),
              Code.steps_of_node_id(id),
            ) {
            | (None, None, None) => None
            | (Some(steps), _, _) =>
              Some((steps, Some(OnText(anchorOffset))))
            | (_, Some((steps, cursor)), _) => Some((steps, Some(cursor)))
            | (_, _, Some(steps)) => Some((steps, None))
            };
          };
        if (JSUtil.div_contains_node(
              JSUtil.forceGetElementById("cell"),
              anchorNode,
            )) {
          switch (anchorNode##.nodeType) {
          | TEXT =>
            switch (Js.Opt.to_option(anchorNode##.parentNode)) {
            | None => ()
            | Some(parent) =>
              let parent_elem =
                Js.Opt.get(Dom_html.CoerceTo.element(parent), () =>
                  assert(false)
                );
              if (JSUtil.elem_has_cls(parent_elem, "not-editable")) {
                (); // caret transport will trigger a second selectionchange
                  // event, let second trigger do the work
              } else {
                switch (
                  anchorNode |> JSUtil.query_ancestors(is_cursor_position)
                ) {
                | None => ()
                | Some((steps, None)) =>
                  schedule_action(
                    Update.Action.EditAction(MoveToBefore(steps)),
                  )
                | Some((steps, Some(cursor))) =>
                  schedule_action(
                    Update.Action.EditAction(MoveTo((steps, cursor))),
                  )
                };
              };
            }
          | ELEMENT =>
            let elem =
              Js.Opt.get(Dom_html.CoerceTo.element(anchorNode), () =>
                assert(false)
              );
            JSUtil.log("ELEMENT");
            JSUtil.log(elem);
            if (JSUtil.elem_has_cls(elem, "not-editable")) {
              (); // caret transport will trigger a second selectionchange
                // event, let second trigger do the work
            } else {
              switch (
                anchorNode |> JSUtil.query_ancestors(is_cursor_position)
              ) {
              | None => ()
              | Some((steps, None)) =>
                schedule_action(
                  Update.Action.EditAction(MoveToBefore(steps)),
                )
              | Some((steps, Some(cursor))) =>
                schedule_action(
                  Update.Action.EditAction(MoveTo((steps, cursor))),
                )
              };
            };
          | _ => ()
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

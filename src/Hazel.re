module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;
open GeneralUtil;
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

              switch (
                cursor_elem |> Code.child_elems_of_snode_elem,
                // cursor_elem is either SBox or SSeq
                cursor_elem |> Code.elem_is_SBox,
              ) {
              | (None, _) => raise(MalformedView(12))
              | (Some(child_elems), true) =>
                JSUtil.force_get_elem_by_id(box_node_indicator_id)
                |> JSUtil.place_over(cursor_elem);
                let child_indices =
                  model.cursor_info |> CursorInfo.child_indices_of_current_node;
                zip(child_indices, child_elems)
                |> List.iter(((i, child_elem)) =>
                     JSUtil.force_get_elem_by_id(child_indicator_id(i))
                     |> JSUtil.place_over(child_elem)
                   );
              | (Some(child_elems), false) =>
                switch (model.cursor_info.position) {
                | OnText(_) => assert(false)
                | OnDelim(k, _) =>
                  switch (Code.seq_lines_rooted_at_oph)
                  // use delim index to identify op elem
                  let (steps, _) = model |> Model.path;

                  // get tagged range (a,b) on delim elem
                  if (cursor_elem |> Code.elem_is_multi_line) {
                    {
                      // use (a,_) to find that child and place the first pair of indicators
                      // use (a,b) to find tail lines and place remaining indicators
                    };
                  } else {
                    {
                      // use (a,b) to place the three indicators
                    };
                  };
                }
              }
            };
          };
        };
      },
    model,
    Page.view(~inject, model),
  );
};

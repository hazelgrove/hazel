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
  type t = ref(bool);
};

[@warning "-27"]
let on_startup = (~schedule_action, _) => {
  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("selectionchange"), Dom_html.document, _ =>
      schedule_action(Update.Action.SelectionChange)
    );
  Async_kernel.Deferred.return(ref(false));
};

[@warning "-27"]
let create = (model, ~old_model, ~inject) => {
  open Incr.Let_syntax;
  let%map model = model;
  Component.create(
    ~apply_action=Update.apply_action(model),
    ~on_display=
      (setting_caret: State.t, ~schedule_action) => {
        let path = model |> Model.path;
        if (model.is_cell_focused) {
          if (!Code.is_caret_consistent_with_path(path)) {
            switch (Code.caret_position_of_path(path)) {
            | None => raise(MalformedView(10))
            | Some((node, offset)) =>
              setting_caret := true;
              JSUtil.set_caret(node, offset);
            };
          } else {
            setting_caret := false;
            let cursor_elem = JSUtil.force_get_elem_by_cls("cursor");
            // cursor_elem is either SBox or SSeq
            if (cursor_elem |> Code.elem_is_SBox) {
              switch (cursor_elem |> Code.child_elems_of_snode_elem) {
              | None => assert(false)
              | Some(child_elems) =>
                JSUtil.force_get_elem_by_id(box_node_indicator_id)
                |> JSUtil.place_over_elem(cursor_elem);
                let child_indices =
                  model.cursor_info |> CursorInfo.child_indices_of_current_node;
                zip(child_indices, child_elems)
                |> List.iter(((i, child_elem)) =>
                     JSUtil.force_get_elem_by_id(child_indicator_id(i))
                     |> JSUtil.place_over_elem(child_elem)
                   );
                switch (cursor_elem |> JSUtil.get_attr("term")) {
                | None => assert(false)
                | Some(ssexp) =>
                  let term_steps =
                    Path.steps_of_sexp(Sexplib.Sexp.of_string(ssexp));
                  JSUtil.force_get_elem_by_id(box_tm_indicator_id)
                  |> JSUtil.(
                       place_over_elem(
                         force_get_elem_by_id(node_id(term_steps)),
                       )
                     );
                };
              };
            } else {
              switch (model.cursor_info.position) {
              | OnText(_) => assert(false)
              | OnDelim(k, _) =>
                let (steps, _) = model |> Model.path;
                let op_elem = JSUtil.force_get_elem_by_id(op_id(steps, k));
                JSUtil.force_get_elem_by_id(op_node_indicator_id)
                |> JSUtil.place_over_elem(op_elem);
                switch (op_elem |> JSUtil.get_attr("op-range")) {
                | None => assert(false)
                | Some(ssexp) =>
                  let (a, b) =
                    Code.seq_range_of_sexp(Sexplib.Sexp.of_string(ssexp));
                  if (cursor_elem |> Code.elem_is_multi_line) {
                    JSUtil.force_get_elem_by_id(seq_tm_indicator_id(a))
                    |> JSUtil.(
                         place_over_elem(
                           force_get_elem_by_id(node_id(steps @ [a])),
                         )
                       );
                    let sline_elems =
                      Code.sline_elems_of_snode_elem(cursor_elem);
                    range(~lo=a + 1, b + 1)
                    |> List.map(List.nth(sline_elems))
                    |> List.iteri((i, sline_elem) =>
                         JSUtil.force_get_elem_by_id(
                           seq_tm_indicator_id(a + 1 + i),
                         )
                         |> JSUtil.place_over_elem(sline_elem)
                       );
                  } else {
                    let tm_a =
                      JSUtil.force_get_elem_by_id(node_id(steps @ [a]));
                    let tm_b =
                      JSUtil.force_get_elem_by_id(node_id(steps @ [b]));
                    let rect_a = tm_a |> JSUtil.get_bounding_rect;
                    let rect_b = tm_b |> JSUtil.get_bounding_rect;
                    JSUtil.force_get_elem_by_id(box_tm_indicator_id)
                    |> JSUtil.place_over_rect({
                         top: rect_b.top,
                         right: rect_b.right,
                         bottom: rect_b.bottom,
                         left: rect_a.left,
                       });
                  };
                };
              };
            };
          };
        };
      },
    model,
    Page.view(~inject, model),
  );
};

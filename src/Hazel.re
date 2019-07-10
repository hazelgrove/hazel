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
  type setting_caret = ref(bool);
  type t = setting_caret;
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
      (setting_caret: State.t, ~schedule_action: Update.Action.t => unit) => {
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
              Cell.place_box_node_indicator_over_snode_elem(
                ~child_indices=
                  model.cursor_info |> CursorInfo.child_indices_of_current_node,
                cursor_elem,
              );
              Cell.place_box_term_indicator(cursor_elem);
            } else {
              switch (model.cursor_info.position) {
              | OnText(_) => assert(false)
              | OnDelim(k, _) =>
                let (steps, _) = model |> Model.path;
                let op_elem = JSUtil.force_get_elem_by_id(op_id(steps, k));
                op_elem |> Cell.place_op_node_indicator_over_op_elem;
                switch (op_elem |> JSUtil.get_attr("op-range")) {
                | None => assert(false)
                | Some(ssexp) =>
                  let (a, b) =
                    Code.seq_range_of_sexp(Sexplib.Sexp.of_string(ssexp));
                  if (cursor_elem |> Code.elem_is_multi_line) {
                    Cell.place_multi_line_seq_term_indicator(
                      steps,
                      (a, b),
                      cursor_elem,
                    );
                  } else {
                    let tm_a =
                      JSUtil.force_get_elem_by_id(node_id(steps @ [a]));
                    let tm_b =
                      JSUtil.force_get_elem_by_id(node_id(steps @ [b]));
                    Cell.place_box_term_indicator_over_single_line_seq(
                      tm_a,
                      tm_b,
                    );
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

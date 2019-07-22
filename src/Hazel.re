module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;
open ViewUtil;
open SemanticsCommon;

// https://github.com/janestreet/incr_dom/blob/6aa4aca2cfc82a17bbcc0424ff6b0ae3d6d8d540/example/text_input/README.md
// https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml

module Model = Model;
module Action = Update.Action;
module State = State;

[@warning "-27"]
let on_startup = (~schedule_action, _) => {
  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("selectionchange"), Dom_html.document, _ =>
      schedule_action(Update.Action.SelectionChange)
    );
  Dom_html.window##.onfocus :=
    Dom_html.handler(_ => {
      schedule_action(Update.Action.FocusWindow);
      Js._true;
    });
  schedule_action(Update.Action.FocusCell);
  Async_kernel.Deferred.return(
    State.{setting_caret: ref(false), changing_cards: ref(false)},
  );
};

[@warning "-27"]
let create =
    (
      model: Incr.t(Model.t),
      ~old_model: Incr.t(Model.t),
      ~inject: Update.Action.t => Vdom.Event.t,
    ) => {
  open Incr.Let_syntax;
  let%map model = model;
  Component.create(
    ~apply_action=Update.apply_action(model),
    ~on_display=
      (state: State.t, ~schedule_action: Update.Action.t => unit) => {
        let path = model |> Model.path;
        if (state.changing_cards^) {
          switch (Code.caret_position_of_path(path)) {
          | None => assert(false)
          | Some((node, offset)) =>
            state.setting_caret := true;
            JSUtil.set_caret(node, offset);
          };
          state.changing_cards := false;
        } else if (model.is_cell_focused) {
          let (steps, cursor) = path;
          let cursor_elem = JSUtil.force_get_elem_by_cls("cursor");
          switch (cursor) {
          | OnText(_)
          | OnDelim(_, _) =>
            if (!Code.is_caret_consistent_with_path(path)) {
              switch (Code.caret_position_of_path(path)) {
              | None => assert(false)
              | Some((node, offset)) =>
                state.setting_caret := true;
                JSUtil.set_caret(node, offset);
              };
            } else {
              state.setting_caret := false;
              // cursor_elem is either SBox or SSeq
              if (cursor_elem |> Code.elem_is_SBox) {
                cursor_elem
                |> Cell.draw_box_node_indicator(
                     ~child_indices=
                       model.cursor_info
                       |> CursorInfo.child_indices_of_current_node,
                   );
                cursor_elem
                |> Cell.draw_box_term_indicator(
                     ~cursor_info=model.cursor_info,
                   );
                Cell.draw_hole_indicators(model);
              } else {
                // cursor_elem is SSeq
                switch (model.cursor_info.position) {
                | Staging(_) => assert(false)
                | OnText(_) => assert(false)
                | OnDelim(k, _) =>
                  let (steps, _) = model |> Model.path;
                  let op_elem = JSUtil.force_get_elem_by_id(op_id(steps, k));
                  op_elem |> Cell.draw_op_node_indicator;
                  switch (op_elem |> JSUtil.get_attr("op-range")) {
                  | None => assert(false)
                  | Some(ssexp) =>
                    let (a, b) =
                      Code.seq_range_of_sexp(Sexplib.Sexp.of_string(ssexp));
                    if (cursor_elem |> Code.elem_is_multi_line) {
                      Cell.draw_multi_line_seq_term_indicator(
                        steps,
                        (a, b),
                        cursor_elem,
                      );
                    } else {
                      let tm_a =
                        JSUtil.force_get_elem_by_id(node_id(steps @ [a]));
                      let tm_b =
                        JSUtil.force_get_elem_by_id(node_id(steps @ [b]));
                      Cell.draw_box_term_indicator_over_single_line_seq(
                        tm_a,
                        tm_b,
                      );
                    };
                  };
                };
              };
            }
          | Staging(delim_index) =>
            JSUtil.unset_caret();
            // only SBox elems can be in staging mode
            cursor_elem
            |> Cell.draw_box_node_indicator(
                 ~child_indices=
                   model.cursor_info
                   |> CursorInfo.child_indices_of_current_node,
               );
            let sdelim_elem =
              Code.force_get_sdelim_elem((steps, delim_index));
            Cell.draw_shift_targets(
              ~cursor_info=model.cursor_info,
              sdelim_elem,
            );
          };
        };
      },
    model,
    Page.view(~inject, model),
  );
};

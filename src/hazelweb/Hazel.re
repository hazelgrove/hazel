module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;

// https://github.com/janestreet/incr_dom/blob/6aa4aca2cfc82a17bbcc0424ff6b0ae3d6d8d540/example/text_input/README.md
// https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml

module Model = Model;
module Action = Update.Action;
module State = State;

let on_startup = (~schedule_action, _) => {
  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("selectionchange"),
      Dom_html.document,
      _ => {
        let anchorNode = Dom_html.window##getSelection##.anchorNode;
        let contenteditable = JSUtil.force_get_elem_by_id("contenteditable");
        if (JSUtil.div_contains_node(contenteditable, anchorNode)) {
          schedule_action(Update.Action.SelectionChange);
        };
      },
    );
  Dom_html.window##.onfocus :=
    Dom_html.handler(_ => {
      schedule_action(Update.Action.FocusWindow);
      Js._true;
    });
  schedule_action(Update.Action.FocusCell);

  let col_width = ref(0.0);
  let row_height = ref(0.0);
  let set_font_metrics = () => {
    let specimen = JSUtil.force_get_elem_by_id("font-specimen");
    let rect = specimen##getBoundingClientRect;
    col_width := Js.to_float(rect##.right) -. Js.to_float(rect##.left);
    row_height := Js.to_float(rect##.bottom) -. Js.to_float(rect##.top);
  };
  set_font_metrics();
  Dom_html.window##.onresize :=
    Dom_html.handler(_ => {
      set_font_metrics();
      Js._true;
    });
  Async_kernel.Deferred.return(
    State.{
      setting_caret: ref(false),
      changing_cards: ref(false),
      col_width,
      row_height,
    },
  );
};

let create =
    (
      model: Incr.t(Model.t),
      ~old_model as _: Incr.t(Model.t),
      ~inject: Update.Action.t => Vdom.Event.t,
    ) => {
  open Incr.Let_syntax;
  let%map model = model;
  Component.create(
    ~apply_action=Update.apply_action(model),
    ~on_display=
      (state: State.t, ~schedule_action as _: Update.Action.t => unit) => {
        let (steps, cursor) = model |> Model.get_program |> Program.get_path;
        let rev_path = (cursor, List.rev(steps));
        if (state.changing_cards^) {
          state.changing_cards := false;
          let (anchor_node, offset) = CaretMap.anchor_of_revpath(rev_path);
          //CaretMap.set_caret_revpath(~state, rev_path);
          state.setting_caret := true;
          JSUtil.set_caret(anchor_node, offset);
        } else if (model.is_cell_focused) {
          let (expected_node, expected_offset) =
            CaretMap.anchor_of_revpath(rev_path);
          let (actual_node, actual_offset) = JSUtil.get_selection_anchor();
          if (actual_node === expected_node
              && actual_offset === expected_offset) {
            state.setting_caret := false;
          } else {
            state.setting_caret := true;
            JSUtil.set_caret(expected_node, expected_offset);
          };
        };
      },
    model,
    Page.view(~inject, model),
  );
};

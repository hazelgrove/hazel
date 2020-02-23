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
  let setting_caret = ref(false);
  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("selectionchange"), Dom_html.document, _ =>
      if (! setting_caret^) {
        let anchor_offset = Dom_html.window##getSelection##.anchorOffset;
        let anchor_node = Dom_html.window##getSelection##.anchorNode;
        let contenteditable = JSUtil.force_get_elem_by_id("contenteditable");
        if (JSUtil.div_contains_node(contenteditable, anchor_node)) {
          let anchor_parent = anchor_node |> JSUtil.force_get_closest_elem;
          Code.Contenteditable.schedule_move_or_transport(
            ~schedule_action,
            ~setting_caret,
            anchor_offset,
            anchor_parent,
          );
        };
      } else {
        setting_caret := false;
      }
    );
  Dom_html.window##.onfocus :=
    Dom_html.handler(_ => {
      schedule_action(Update.Action.FocusWindow);
      Js._true;
    });
  schedule_action(Update.Action.FocusCell);
  Async_kernel.Deferred.return(
    State.{setting_caret, changing_cards: ref(false)},
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
        let path = model |> Model.get_program |> Program.get_path;
        if (state.changing_cards^) {
          state.changing_cards := false;
          let (anchor_parent, anchor_offset) =
            path |> Code.Contenteditable.caret_position_of_path;
          state.setting_caret := true;
          JSUtil.set_caret(anchor_parent, anchor_offset);
        } else {
          state.setting_caret := false;
        };
      },
    model,
    Page.view(~inject, model),
  );
};
